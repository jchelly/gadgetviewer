#include <stdlib.h>
#include <stdio.h>
#define H5_USE_16_API
#include <hdf5.h>
#include <string.h>
#include <math.h>
#include "read_eagle.h"

/* Set this for lots of output */
/* #define VERBOSE */

/* Whether to abort on errors */
static int abort_err = 1;
void abort_on_error(int flag)
{
  if(flag)
    abort_err = 1;
  else
    abort_err = 0;
}

/* Description of last error encountered */
static char last_error[MAX_NAMELEN] = "";

/*
  Copy a string

  Copies at most n characters and ensures that
  the result is null terminated.
*/
char *my_strncpy(char *str1, const char *str2, size_t n)
{
  char *res = strncpy(str1, str2, n);
  *(str1+n-1) = (char) 0;
  return res;
}

/* Function to set the error description */
void set_error(char *str)
{
  my_strncpy(last_error, str, MAX_NAMELEN);
  if(abort_err)
    {
      fprintf(stderr, "Call failed in read_eagle.c\n");
      fprintf(stderr, "Reason: %s\n", last_error);
      exit(1);
    }
}

/* Return a pointer to the error string */
char *get_error(void)
{
  return last_error;
}

/* Functions to read whole datasets and attributes */
int read_hdf5_dataset(hid_t file_id, char *name, hid_t dtype_id, void *buf)
{
  hid_t dset_id = H5Dopen(file_id, name);
  if(dset_id >= 0)
    {
      if(H5Dread(dset_id, dtype_id, H5S_ALL, H5S_ALL,
		 H5P_DEFAULT, buf) < 0)
	{
	  H5Dclose(dset_id);
	  return -1;
	}
    }
  else
    return -1;
  H5Dclose(dset_id);
  return 0;
}

int read_hdf5_attribute(hid_t file_id, char *group, char *name, 
			hid_t dtype_id, void *buf)
{
  hid_t attr_id;
  hid_t group_id = H5Gopen(file_id, group);
  if(group_id < 0)return -1;

  attr_id = H5Aopen_name(group_id, name);
  if(attr_id < 0)
    {
      H5Gclose(group_id);
      return -1;
    }

  if(H5Aread(attr_id, dtype_id, buf)<0)
    {
      H5Gclose(group_id);
      H5Aclose(attr_id);
      return -1;
    }

  H5Gclose(group_id);
  H5Aclose(attr_id);
  return 0;
}

/*
  Deallocate any allocated components of snap - used if
  open_snapshot needs to abort. Also frees snap itself.
*/
void cleanup(EagleSnapshot *snap)
{
  int itype;
  int ifile;

  if(snap->hashmap)free(snap->hashmap);
  if(snap->first_key_in_file)free(snap->first_key_in_file);
  if(snap->last_key_in_file)free(snap->last_key_in_file);
  if(snap->num_keys_in_file)free(snap->num_keys_in_file);
  for(itype=0;itype<6;itype++)
    {
      if(snap->part_per_cell[itype])
	{
	  for(ifile=0;ifile<snap->numfiles;ifile++)
	    if(snap->part_per_cell[itype][ifile])
	      free(snap->part_per_cell[itype][ifile]);
	  free(snap->part_per_cell[itype]);
	}
      if(snap->first_in_cell[itype])
	{
	  for(ifile=0;ifile<snap->numfiles;ifile++)
	    if(snap->first_in_cell[itype][ifile])
	      free(snap->first_in_cell[itype][ifile]);
	  free(snap->first_in_cell[itype]);
	}
      if(snap->dataset_name[itype])
	free(snap->dataset_name[itype]);
    }
  free(snap);
  return;
}


/*
  Examine the snapshot file to see what datasets are present
*/
void get_dataset_list(EagleSnapshot *snap, hid_t loc_id, int itype, char *prefix)
{
  herr_t err;
  hsize_t nobj;
  int i;
  int otype;
  char name[MAX_NAMELEN];
  char fullname[MAX_NAMELEN];
  hid_t group_id;

  /* Get number of members */
  err = H5Gget_num_objs(loc_id, &nobj);  
  
  /* Loop over members */
  for(i=0; i<nobj; i+=1)
    {
      otype = H5Gget_objtype_by_idx(loc_id, i);
      H5Gget_objname_by_idx(loc_id, i, name, MAX_NAMELEN);
      snprintf(fullname, MAX_NAMELEN, "%s/%s", prefix, name);
      if (otype == H5G_DATASET)
	{
	  /* Member is a dataset, so store its name */
	  if(snap->dataset_name[itype])
	    {
	      my_strncpy(snap->dataset_name[itype]+MAX_NAMELEN*snap->num_datasets[itype],
			 fullname, MAX_NAMELEN);
	    }
	  snap->num_datasets[itype] += 1;
	}
      else if (otype == H5G_GROUP)
	{
	  /* Member is a group, so examine it recusively */
	  group_id = H5Gopen(loc_id, name);
	  if(group_id >= 0)
	    {
	      get_dataset_list(snap, group_id, itype, fullname);
	      H5Gclose(group_id);
	    }
	}
    }
}


/*
  Open a snapshot
*/
EagleSnapshot *open_snapshot(char *fname)
{
  hid_t file_id;
  hid_t group_id;
  EagleSnapshot *snap;
  int err1, err2, err3, err4, err5;
  int i, itype, ifile, n;
  char str[MAX_NAMELEN];
  unsigned int nptot[6], nptot_hw[6];
  char name[MAX_NAMELEN];
  
  H5open();

#ifdef VERBOSE
  fprintf(stderr,"open_snapshot() called\n");
#endif

  /* Open the specified file */
  file_id = H5Fopen(fname, H5F_ACC_RDONLY, H5P_DEFAULT); 
  if(file_id<0)
    {
      sprintf(str, "Unable to open file: %s", fname);
      set_error(str);
      return NULL;
    }

  /* Allocate snapshot data structure */
  if(!(snap = malloc(sizeof(EagleSnapshot))))
    {
      set_error("Failed to allocate memory");
      H5Fclose(file_id);
      return NULL;
    }
  
#ifdef VERBOSE
  fprintf(stderr,"  - Opened file: %s\n", fname);
#endif

  /* Read bits we need from the header */
  err1 = read_hdf5_attribute(file_id, "Header",    "BoxSize",                H5T_NATIVE_DOUBLE, &(snap->boxsize));
  err2 = read_hdf5_attribute(file_id, "Header",    "NumFilesPerSnapshot",    H5T_NATIVE_INT,    &(snap->numfiles));
  err3 = read_hdf5_attribute(file_id, "Header",    "NumPart_Total",          H5T_NATIVE_UINT,     nptot);
  err4 = read_hdf5_attribute(file_id, "Header",    "NumPart_Total_HighWord", H5T_NATIVE_UINT,     nptot_hw);
  err5 = read_hdf5_attribute(file_id, "HashTable", "HashBits",               H5T_NATIVE_INT,    &(snap->hashbits));
  if(err1<0 || err2<0 || err3<0 || err4<0 || err5 <0)
    {
      sprintf(str, "Unable to read hash table information from file: %s", fname);
      set_error(str);
      free(snap);
      H5Fclose(file_id);
      return NULL;
    }
  snap->ncell = (1 << snap->hashbits);      /* Number of cells in 1D */
  snap->nhash = (1 << (3*snap->hashbits));  /* Total cells */
  for(i=0;i<6;i++)
    snap->numpart_total[i] = nptot[i] + (((long long) nptot_hw[i]) << 32);

#ifdef VERBOSE
  fprintf(stderr,"  - Read in file header\n");
#endif

  /* Allocate and initialise hashmap */
  if(!(snap->hashmap = malloc(snap->nhash*sizeof(unsigned char))))
    {
      set_error("Failed to allocate memory");
      free(snap);
      H5Fclose(file_id);
      return NULL;
    }
  for(i=0;i<snap->nhash;i++)
    snap->hashmap[i] = 0;

  /* Store base name */
  my_strncpy(snap->basename, fname, MAX_NAMELEN);
  n = strlen(snap->basename) - 6;
  while((n > 0) && (snap->basename[n] != '.'))
    {
      snap->basename[n] = (char) 0;
      n -= 1;
    }
  if(n <= 0)
    {
      set_error("Don't understand snapshot file name!");
      free(snap);
      H5Fclose(file_id);
      return NULL; 
    }
  snap->basename[n] = (char) 0;

#ifdef VERBOSE
  fprintf(stderr,"  - Base name is %s\n", snap->basename);
#endif

  /* Ensure that pointers in snap are allocated or NULL */
  for(itype=0;itype<6;itype++)
    if(snap->numpart_total[itype] > 0)
      {
	snap->first_key_in_file[itype] = NULL;
	snap->last_key_in_file[itype]  = NULL;
	snap->num_keys_in_file[itype]  = NULL;
	snap->part_per_cell[itype]     = NULL;
	snap->first_in_cell[itype]     = NULL;
	snap->dataset_name[itype]      = NULL;
      }

  /* Read range of hash cells in each file */
  for(itype=0;itype<6;itype++)
    {
      if(snap->numpart_total[itype] > 0)
	{
#ifdef VERBOSE
	  fprintf(stderr,"  - Have particles of type %i\n", itype);
#endif
	  /* First key in file */
	  if(!(snap->first_key_in_file[itype] = malloc(sizeof(long long) * snap->numfiles)))
	    {
	      set_error("Failed to allocate memory");
	      cleanup(snap);
	      H5Fclose(file_id);
	      return NULL;
	    }
	  sprintf(str, "HashTable/PartType%i/FirstKeyInFile", itype);
	  if(read_hdf5_dataset(file_id, str, H5T_NATIVE_LLONG, snap->first_key_in_file[itype]) < 0)
	    {
	      set_error("Failed to allocate memory");
	      cleanup(snap);
	      H5Fclose(file_id);
	      return NULL; 
	    }
	  /* Last key in file */
	  if(!(snap->last_key_in_file[itype] = malloc(sizeof(long long) * snap->numfiles)))
	    {
	      set_error("Failed to allocate memory");
	      cleanup(snap);
	      H5Fclose(file_id);
	      return NULL; 
	    }
	  sprintf(str, "HashTable/PartType%i/LastKeyInFile", itype);
	  if(read_hdf5_dataset(file_id, str, H5T_NATIVE_LLONG, snap->last_key_in_file[itype]) < 0)
	    {
	      set_error("Failed to allocate memory");
	      cleanup(snap);
	      H5Fclose(file_id);
	      return NULL; 
	    }
	  /* Number of keys in file */
	  if(!(snap->num_keys_in_file[itype] = malloc(sizeof(long long) * snap->numfiles)))
	    {
	      set_error("Failed to allocate memory");
	      cleanup(snap);
	      H5Fclose(file_id);
	      return NULL; 
	    }
	  sprintf(str, "HashTable/PartType%i/NumKeysInFile", itype);
	  if(read_hdf5_dataset(file_id, str, H5T_NATIVE_LLONG, snap->num_keys_in_file[itype]) < 0)
	    {
	      set_error("Failed to allocate memory");
	      cleanup(snap);
	      H5Fclose(file_id);
	      return NULL; 
	    }
	}
    }

  /* Close file */
  H5Fclose(file_id);

  /* 
     Initialise pointers to hash table data - this is only read as required,
     NULL indicates not read yet.
  */
  for(itype=0;itype<6;itype++)
    {
      if(snap->numpart_total[itype] > 0)
	{
	  if(!(snap->part_per_cell[itype] = malloc(snap->numfiles*sizeof(int *))))
	    {
	      set_error("Failed to allocate memory");
	      cleanup(snap);
	      return NULL;
	    }
	  for(ifile=0;ifile<snap->numfiles;ifile++)
	    snap->part_per_cell[itype][ifile] = NULL;
	  if(!(snap->first_in_cell[itype] = malloc(snap->numfiles*sizeof(int *))))
	    {
	      set_error("Failed to allocate memory");
	      cleanup(snap);
	      return NULL;
	    }
	  for(ifile=0;ifile<snap->numfiles;ifile++)
	    snap->first_in_cell[itype][ifile] = NULL;
	}
    }


  /* Initialise list of datasets for each type */
  for(itype=0;itype<6;itype++)
    {
      snap->num_datasets[itype] = 0;
      snap->dataset_name[itype] = NULL;
    }
  
  /* Determine what datasets we have */
  for(itype=0;itype<6;itype++)
    {
      if(snap->numpart_total[itype] > 0)
	{
	  /* Find a file which has some particles of this type */
	  ifile = 0;
	  while(snap->num_keys_in_file[itype][ifile] == 0)
	    ifile += 1;
      
	  /* Open the file */
	  sprintf(name, "%s.%i.hdf5", snap->basename, ifile);
	  if((file_id = H5Fopen(name, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
	    {
	      set_error("Unable to open snapshot file!");
	      cleanup(snap);
	      return 0;
	    }
	  
	  /* Open the PartType group */
	  sprintf(str, "PartType%d", itype);
	  group_id = H5Gopen(file_id, str);
	  if(group_id < 0)
	    {
	      H5Fclose(file_id);
	      set_error("Unable to open PartType group");
	      cleanup(snap);
	      return 0;
	    }

	  /* Count datasets, allocate memory, then store names */
	  get_dataset_list(snap, group_id, itype, "");
	  snap->dataset_name[itype] = malloc(snap->num_datasets[itype]*MAX_NAMELEN);
	  if(!(snap->dataset_name[itype]))
	    {
	      H5Gclose(group_id);
	      H5Fclose(file_id);
	      set_error("Unable to allocate memory for dataset names");
	      cleanup(snap);
	      return 0; 
	    }
	  snap->num_datasets[itype] = 0;
	  get_dataset_list(snap, group_id, itype, "");
	  
	  /* Close file and go on to next particle type */
	  H5Gclose(group_id);
	  H5Fclose(file_id);
	}
    }
      
  /* Return a pointer to the snap structure */
  return snap;  
}

/*
  Close a snapshot and deallocate associated data
*/
void close_snapshot(EagleSnapshot *snap)
{
  int itype, ifile;

#ifdef VERBOSE
  fprintf(stderr,"close_snapshot() called\n");
#endif

  for(itype=0;itype<6;itype++)
    {
      if(snap->numpart_total[itype] > 0)
	{
	  for(ifile=0;ifile<snap->numfiles;ifile++)
	    {
	      if(snap->part_per_cell[itype][ifile])
		free(snap->part_per_cell[itype][ifile]);
	      if(snap->first_in_cell[itype][ifile])
		free(snap->first_in_cell[itype][ifile]);
	    }
	  free(snap->first_in_cell[itype]);
	  free(snap->part_per_cell[itype]);
	  free(snap->first_key_in_file[itype]);
	  free(snap->last_key_in_file[itype]);
	  free(snap->num_keys_in_file[itype]);
	  if(snap->dataset_name[itype])
	    free(snap->dataset_name[itype]);
	}
    }
  free(snap->hashmap);
  free(snap);
  return;
}


/*
  Set the hashmap flag for all hash cells in the specified
  region
*/
void select_region(EagleSnapshot *snap, 
		   double xmin, double xmax,
		   double ymin, double ymax,
		   double zmin, double zmax)
{
  int ixmin = (int) floor(xmin / snap->boxsize * snap->ncell);
  int ixmax = (int) floor(xmax / snap->boxsize * snap->ncell);
  int iymin = (int) floor(ymin / snap->boxsize * snap->ncell);
  int iymax = (int) floor(ymax / snap->boxsize * snap->ncell);
  int izmin = (int) floor(zmin / snap->boxsize * snap->ncell);
  int izmax = (int) floor(zmax / snap->boxsize * snap->ncell);
  int iix, iiy, iiz;
  int ix, iy, iz;
  int n;

#ifdef VERBOSE
  fprintf(stderr,"select_region() called\n");
#endif

  n = 0;
  for(ix=ixmin;ix<=ixmax;ix+=1)
    {
      iix = ix;
      while(iix < 0)      iix += snap->ncell;
      while(iix >= snap->ncell) iix -= snap->ncell;
      for(iy=iymin;iy<=iymax;iy+=1)
	{
	  iiy = iy;
	  while(iiy < 0)      iiy += snap->ncell;
	  while(iiy >= snap->ncell) iiy -= snap->ncell;
	  for(iz=izmin;iz<=izmax;iz+=1)
	    {
	      iiz = iz;
	      while(iiz < 0)      iiz += snap->ncell;
	      while(iiz >= snap->ncell) iiz -= snap->ncell;
	      snap->hashmap[peano_hilbert_key(iix, iiy, iiz, snap->hashbits)] = 1;
	      n += 1;
	    }
	}
    }

#ifdef VERBOSE
  fprintf(stderr,"  - Selected %d cells of %d\n", n, snap->nhash);
#endif

  return;
}


/*
  Clear flags for all hash cells
*/
void clear_selection(EagleSnapshot *snap)
{
  int i;

#ifdef VERBOSE
  fprintf(stderr,"clear_selection() called\n");
#endif

  for(i=0;i<snap->nhash;i++)
    snap->hashmap[i] = 0;

  return;
}


/*
  Load the hash table for a particular file and particle type.
  Returns 1 on success, 0 on failure.
*/
int load_hash_table(EagleSnapshot *snap, int itype, int ifile)
{
  int i;
  hid_t file_id;
  char name[MAX_NAMELEN];
#ifdef VERBOSE
  fprintf(stderr,"  - Reading hash table for file %d particle type %d\n", ifile, itype);
#endif

  if(!(snap->part_per_cell[itype][ifile] = 
       malloc(sizeof(unsigned int)*snap->num_keys_in_file[itype][ifile])))
    return 0;
  
  if(!(snap->first_in_cell[itype][ifile] = 
       malloc(sizeof(unsigned int)*snap->num_keys_in_file[itype][ifile])))
    {
      free(snap->part_per_cell[itype][ifile]);
      snap->part_per_cell[itype][ifile] = NULL;
      return 0;
    }

  sprintf(name, "%s.%i.hdf5", snap->basename, ifile);
  if((file_id = H5Fopen(name, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
    {
      free(snap->part_per_cell[itype][ifile]);
      snap->part_per_cell[itype][ifile] = NULL;
      free(snap->first_in_cell[itype][ifile]);
      snap->first_in_cell[itype][ifile] = NULL;
      return 0;
    }

  sprintf(name, "HashTable/PartType%i/NumParticleInCell", itype);
  if(read_hdf5_dataset(file_id, name, H5T_NATIVE_UINT, snap->part_per_cell[itype][ifile])<0)
    {
      free(snap->part_per_cell[itype][ifile]);
      snap->part_per_cell[itype][ifile] = NULL;
      free(snap->first_in_cell[itype][ifile]);
      snap->first_in_cell[itype][ifile] = NULL;
      H5Fclose(file_id);
      return 0;
    }
  H5Fclose(file_id);  

  /* Calculate offset to first particle in each hash cell */
  snap->first_in_cell[itype][ifile][0] = 0;
  for(i=1;i<snap->num_keys_in_file[itype][ifile];i++)
    snap->first_in_cell[itype][ifile][i] = snap->first_in_cell[itype][ifile][i-1] + 
      snap->part_per_cell[itype][ifile][i-1];
  
  return 1;
}



/*
  Count selected particles
*/
long long count_particles(EagleSnapshot *snap, int itype)
{  
  int       ifile;
  long long key;
  long long n_to_read;

  /* Range check on itype */
  if(itype<0 || itype>5)
    {
      set_error("Particle type itype is outside range 0-5!");
      return -1;
    }

#ifdef VERBOSE
  fprintf(stderr,"count_particles() called\n");
#endif

  /* Check if there are any particles of this type */
  if(snap->numpart_total[itype] == 0)
    return 0;

  /* Count of number of particles in selected cells */
  n_to_read = 0;

  /* Loop over files */
  for(ifile=0; ifile<snap->numfiles; ifile++)
    {
      /* Loop over hash keys in this file */
      if(snap->num_keys_in_file[itype][ifile] > 0)
	{
	  for(key=snap->first_key_in_file[itype][ifile]; key<=snap->last_key_in_file[itype][ifile]; key++)
	    {
	      if(snap->hashmap[key])
		{
		  /* This key is in the current file, so make sure hash table for the file is in memory */
		  if(!(snap->part_per_cell[itype][ifile]))
		    {
		      if(load_hash_table(snap, itype, ifile)==0)
			{
			  set_error("Unable to read hash table data");
			  return -1;
			}
		    }
		  /* Count particles to read from this cell */
		  long long offset = key - snap->first_key_in_file[itype][ifile];
		  n_to_read += snap->part_per_cell[itype][ifile][offset];
		}
	    }
	}
    }
  return n_to_read;
}


/*
  Read a dataset for the selected particles
*/
long long read_dataset(EagleSnapshot *snap, int itype, char *dset_name, hid_t hdf5_type, void *buf, size_t n)
{

  int       ifile;
  long long key, end_key, this_key;
  char name[MAX_NAMELEN];
  char fname[MAX_NAMELEN];
  char str[MAX_NAMELEN];
  hsize_t start[2], count[2];

  /* Dataset info */
  int rank;

  /* Output buffer info */
  hid_t memspace_id;
  hsize_t dims[2];
  long long ooffset;

  /* Current file being read */
  int   open_file;
  hid_t file_id;
  hid_t dset_id;
  hid_t dspace_id;
  unsigned int nread_file;

  /* Range check on itype */
  if(itype<0 || itype>5)
    {
      set_error("Particle type itype is outside range 0-5!");
      return -1;
    }

#ifdef VERBOSE
  fprintf(stderr,"read_dataset() called\n");
#endif

  /* Check if there are any particles of this type */
  if(snap->numpart_total[itype] == 0)
    return 0;

  /* Initially have no file open */
  open_file = -1;

  /* Set hdf5 identifiers to -1 to aid cleaning up if something goes wrong */
  memspace_id = -1;
  file_id     = -1;
  dspace_id   = -1;
  dset_id     = -1;

  nread_file = 0;
  rank = 0;

  /* Offset into output array */
  ooffset     = 0;

  /* Loop over files */
  for(ifile=0; ifile<snap->numfiles; ifile++)
    {
      /* Loop over hash keys in this file */
      if(snap->num_keys_in_file[itype][ifile] > 0)
	{
	  for(key=snap->first_key_in_file[itype][ifile];
	      key<=snap->last_key_in_file[itype][ifile]; key++)
	    {
	      if(snap->hashmap[key])
		{
		  /* This key is in the current file, so make sure hash table for the file is in memory */
		  if(!(snap->part_per_cell[itype][ifile]))
		    {
		      if(load_hash_table(snap, itype, ifile)==0)
			{
			  set_error("Unable to read hash table data");
			  goto cleanup;
			}
		    }

		  /* Check if there's a series of consecutive selected cells we can read. */
		  end_key = key;
		  while(end_key <= snap->last_key_in_file[itype][ifile] && snap->hashmap[end_key])
		    end_key += 1;
		  end_key -= 1;
  
		  /* Check if we need to open a new file */
		  if(ifile != open_file)
		    {
		      if(open_file >= 0)
			{
			  /*
			    Already have a file open, so read from it then close it
			    First need to create the memory dataspace if we haven't already.
			  */
			  if(nread_file>0)
			    {
			      if(memspace_id < 0)
				{
				  if(rank==2)
				    {
				      dims[0] = n / 3;
				      dims[1] = 3;
				    }
				  else if(rank==1)
				    {
				      dims[0] = n;
				    }
				  else
				    {
				      set_error("Can only read 1D or 2D datasets!");
				      goto cleanup;
				    }
				  if((memspace_id = H5Screate_simple(rank, dims, NULL)) < 0)
				    {
				      set_error("Failed to create memory dataspace for output buffer");
				      goto cleanup;
				    }
				}
			      /* Select part of output buffer to write to */
			      start[0] = ooffset;
			      count[0] = nread_file;
			      start[1] = 0; /* ignored if dataset is 1D */
			      count[1] = 3;
			      if((H5Sselect_hyperslab(memspace_id, H5S_SELECT_SET, start, NULL, count, NULL)) < 0)
				{
				  set_error("Unable to select elements in output buffer - buffer too small?");
				  goto cleanup;
				}
			      /* Read the data */
			      if((H5Dread(dset_id, hdf5_type, memspace_id, dspace_id, H5P_DEFAULT, buf)) < 0)
				{
				  set_error("Unable to read dataset");
				  goto cleanup;
				}
			    }
			  /* Close this file, dataset etc */
			  H5Fclose(file_id);   file_id   = -1;
			  H5Dclose(dset_id);   dset_id   = -1;
			  H5Sclose(dspace_id); dspace_id = -1;
			  /* Advance offset into output buffer */
			  ooffset += nread_file;
			}
		      /* Now open the new file */
		      sprintf(fname, "%s.%i.hdf5", snap->basename, ifile);
		      if((file_id = H5Fopen(fname, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
			{
			  sprintf(str, "Unable to open file: %s", fname);
			  set_error(str);
			  goto cleanup;
			}
#ifdef VERBOSE
		      fprintf(stderr,"  - Opened file %d\n", ifile);
#endif
		      open_file = ifile;
		      /* Open the dataset and get its dataspace */
		      sprintf(name, "PartType%i/%s", itype, dset_name);
		      if((dset_id = H5Dopen(file_id, name)) < 0)
			{
			  sprintf(str, "Unable to open dataset: %s", name);
			  set_error(str);
			  goto cleanup;
			}
		      dspace_id = H5Dget_space(dset_id);
		      /* Initially no particles are selected */
		      H5Sselect_none(dspace_id);
		      /* Get the rank of the dataset in case its a vector quantity */
		      rank = H5Sget_simple_extent_ndims(dspace_id);
		      /* Haven't selected anything to read yet */
		      nread_file = 0;
		    }
		  
		  /* Determine number of particles to select */
		  count[0] = 0;
		  count[1] = 3;
		  for(this_key=key;this_key<=end_key;this_key++)
		    count[0] += snap->part_per_cell[itype][ifile][this_key-snap->first_key_in_file[itype][ifile]];

		  /* Find offset to first particle */
		  start[0] = snap->first_in_cell[itype][ifile][key-snap->first_key_in_file[itype][ifile]];;
		  start[1] = 0;

		  /* Add these particles to the selection */
		  if(H5Sselect_hyperslab(dspace_id, H5S_SELECT_OR, start, NULL, count, NULL) < 0)
		    {
		      set_error("Unable to select elements in snapshot file");
		      goto cleanup;
		    }
#ifdef VERBOSE
		  fprintf(stderr,"    Selected %d particles starting at offset %d\n", 
			 (int) count[0], (int) start[0]);
#endif		  
		  /* Count particles from this file */
		  nread_file += count[0];

		  /* Skip keys we've read */
		  key = end_key;
		}
	    }
	}
    }

  /* Read any data remaining and close the last file */
  if(open_file >= 0)
    {
      /*
	Still have a file open, so read from it then close it
	First need to create the memory dataspace if we haven't already.
      */
      if(nread_file>0)
	{
	  if(memspace_id < 0)
	    {
	      if(rank==2)
		{
		  dims[0] = n / 3;
		  dims[1] = 3;
		}
	      else if(rank==1)
		{
		  dims[0] = n;
		}
	      else
		{
		  set_error("Can only read 1D or 2D datasets!");
		  goto cleanup;
		}
	      if((memspace_id = H5Screate_simple(rank, dims, NULL)) < 0)
		{
		  set_error("Failed to create memory dataspace for output buffer");
		  goto cleanup;
		}
	    }
	  /* Select part of output buffer to write to */
	  start[0] = ooffset;
	  count[0] = nread_file;
	  start[1] = 0; /* ignored if dataset is 1D */
	  count[1] = 3;	  
	  if(H5Sselect_hyperslab(memspace_id, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
	    {
	      set_error("Unable to select elements in output buffer - buffer too small?");
	      goto cleanup;
	    }
	  /* Read the data */
	  if(H5Dread(dset_id, hdf5_type, memspace_id, dspace_id, H5P_DEFAULT, buf) < 0)
	    {
	      set_error("Unable to read dataset");
	      goto cleanup;
	    }
	}
      /* Close this file, dataset etc */
      H5Fclose(file_id);   file_id   = -1;
      H5Dclose(dset_id);   dset_id   = -1;
      H5Sclose(dspace_id); dspace_id = -1;
      ooffset += nread_file;
    }  

  if(memspace_id >= 0)
    {
      H5Sclose(memspace_id);
      memspace_id = -1;
    }

  return ooffset;


 cleanup:

  /* Something went wrong - deallocate anything still allocated and return an error code */
  if(file_id     >= 0) H5Fclose(file_id);
  if(dset_id     >= 0) H5Dclose(dset_id);
  if(dspace_id   >= 0) H5Sclose(dspace_id);
  if(memspace_id >= 0) H5Sclose(memspace_id);
  return -1;

}


/* Return type and rank of a dataset */
int get_dataset_info(EagleSnapshot *snap, int itype, char *dset_name, TypeCode *typecode, int *rank)
{
  char fname[MAX_NAMELEN];
  char name[MAX_NAMELEN];
  char str[MAX_NAMELEN];
  int ifile;
  hid_t file_id;
  hid_t dset_id;
  hid_t dspace_id;
  hid_t dtype_id;
  H5T_class_t dclass;
  size_t dsize;

  /* Range check on itype */
  if(itype<0 || itype>5)
    {
      set_error("Particle type itype is outside range 0-5!");
      return -1;
    }

  /* Check we have particles of this type */
  if(snap->numpart_total[itype] == 0)
    {
      set_error("There are no particles of the requested type!");
      return -1;
    }

  /* Find a file which has some particles of this type */
  ifile = 0;
  while(snap->num_keys_in_file[itype][ifile] == 0)
    ifile += 1;
  
  /* Open the file */
  sprintf(fname, "%s.%i.hdf5", snap->basename, ifile);
  if((file_id = H5Fopen(fname, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
    {
      sprintf(str, "Unable to open file: %s", fname);
      set_error(str);
      return -1;
    }

  /* Open the dataset */
  sprintf(name, "PartType%i/%s", itype, dset_name);
  if((dset_id = H5Dopen(file_id, name)) < 0)
    {
      sprintf(str, "Unable to open dataset: %s", name);
      set_error(str);
      H5Fclose(file_id);
      return -1;
    }

  /* Extract type and dimensions info */
  dspace_id = H5Dget_space(dset_id);
  dtype_id  = H5Dget_type(dset_id);
  *rank = H5Sget_simple_extent_ndims(dspace_id); 
  dclass = H5Tget_class(dtype_id);
  dsize = H5Tget_size(dtype_id);
  if(dclass==H5T_INTEGER)
    {
      if(dsize <= 4)
	*typecode = t_int;
      else
	*typecode = t_long_long;
    }
  else if(dclass==H5T_FLOAT)
    {
      if(dsize <= 4)
	*typecode = t_float;
      else
	*typecode = t_double;
    }
  else
    {
      set_error("Dataset is not a float or integer type!");
      H5Dclose(dset_id);
      H5Sclose(dspace_id);
      H5Tclose(dtype_id);
      H5Fclose(file_id);
      return -1;
    }
 
  /* Close file */
  H5Dclose(dset_id);
  H5Sclose(dspace_id);
  H5Tclose(dtype_id);
  H5Fclose(file_id);

  return 0;
}


/*
  Return the number of datasets for the specified particle type.

  Return -1 on failure.
*/
int get_dataset_count(EagleSnapshot *snap, int itype)
{
  /* Range check on itype */
  if(itype<0 || itype>5)
    {
      set_error("Particle type itype is outside range 0-5!");
      return -1;
    }
  
  /* 
     Check we have particles of this type 
     
     If not, just return 0 datasets rather than
     calling it an error.
  */
  if(snap->numpart_total[itype] == 0)
    return 0;

  return snap->num_datasets[itype];
}


/*
  Return the name of the specified dataset.

  Returns length of string, or negative number on failure.
*/
int get_dataset_name(EagleSnapshot *snap, int itype, int iset, char *buf, size_t len)
{
  /* Range check on itype */
  if(itype<0 || itype>5)
    {
      set_error("Particle type itype is outside range 0-5!");
      return -1;
    }

  /* Range check on iset */
  if(iset<0 || iset>=snap->num_datasets[itype])
    {
      set_error("Dataset index is out of range!");
      return -1;
    }

  my_strncpy(buf, snap->dataset_name[itype]+MAX_NAMELEN*iset, len);
  return strlen(buf);
}


/*
  The following code is taken from Gadget-2 (http://wwwmpa.mpa-garching.mpg.de/gadget/)
  Copyright (c) 2005       Volker Springel
                           Max-Plank-Institute for Astrophysics
*/

static char quadrants[24][2][2][2] = {
  /* rotx=0, roty=0-3 */
  {{{0, 7}, {1, 6}}, {{3, 4}, {2, 5}}},
  {{{7, 4}, {6, 5}}, {{0, 3}, {1, 2}}},
  {{{4, 3}, {5, 2}}, {{7, 0}, {6, 1}}},
  {{{3, 0}, {2, 1}}, {{4, 7}, {5, 6}}},
  /* rotx=1, roty=0-3 */
  {{{1, 0}, {6, 7}}, {{2, 3}, {5, 4}}},
  {{{0, 3}, {7, 4}}, {{1, 2}, {6, 5}}},
  {{{3, 2}, {4, 5}}, {{0, 1}, {7, 6}}},
  {{{2, 1}, {5, 6}}, {{3, 0}, {4, 7}}},
  /* rotx=2, roty=0-3 */
  {{{6, 1}, {7, 0}}, {{5, 2}, {4, 3}}},
  {{{1, 2}, {0, 3}}, {{6, 5}, {7, 4}}},
  {{{2, 5}, {3, 4}}, {{1, 6}, {0, 7}}},
  {{{5, 6}, {4, 7}}, {{2, 1}, {3, 0}}},
  /* rotx=3, roty=0-3 */
  {{{7, 6}, {0, 1}}, {{4, 5}, {3, 2}}},
  {{{6, 5}, {1, 2}}, {{7, 4}, {0, 3}}},
  {{{5, 4}, {2, 3}}, {{6, 7}, {1, 0}}},
  {{{4, 7}, {3, 0}}, {{5, 6}, {2, 1}}},
  /* rotx=4, roty=0-3 */
  {{{6, 7}, {5, 4}}, {{1, 0}, {2, 3}}},
  {{{7, 0}, {4, 3}}, {{6, 1}, {5, 2}}},
  {{{0, 1}, {3, 2}}, {{7, 6}, {4, 5}}},
  {{{1, 6}, {2, 5}}, {{0, 7}, {3, 4}}},
  /* rotx=5, roty=0-3 */
  {{{2, 3}, {1, 0}}, {{5, 4}, {6, 7}}},
  {{{3, 4}, {0, 7}}, {{2, 5}, {1, 6}}},
  {{{4, 5}, {7, 6}}, {{3, 2}, {0, 1}}},
  {{{5, 2}, {6, 1}}, {{4, 3}, {7, 0}}}
};


static char rotxmap_table[24] = { 4, 5, 6, 7, 8, 9, 10, 11,
  12, 13, 14, 15, 0, 1, 2, 3, 17, 18, 19, 16, 23, 20, 21, 22
};

static char rotymap_table[24] = { 1, 2, 3, 0, 16, 17, 18, 19,
  11, 8, 9, 10, 22, 23, 20, 21, 14, 15, 12, 13, 4, 5, 6, 7
};

static char rotx_table[8] = { 3, 0, 0, 2, 2, 0, 0, 1 };
static char roty_table[8] = { 0, 1, 1, 2, 2, 3, 3, 0 };

static char sense_table[8] = { -1, -1, -1, +1, +1, -1, -1, -1 };


peanokey peano_hilbert_key(int x, int y, int z, int bits)
{
  int i, bitx, bity, bitz, mask, quad, rotation;
  char sense, rotx, roty;
  peanokey key;

  mask = 1 << (bits - 1);
  key = 0;
  rotation = 0;
  sense = 1;

  for(i = 0; i < bits; i++, mask >>= 1)
    {
      bitx = (x & mask) ? 1 : 0;
      bity = (y & mask) ? 1 : 0;
      bitz = (z & mask) ? 1 : 0;

      quad = quadrants[rotation][bitx][bity][bitz];

      key <<= 3;
      key += (sense == 1) ? (quad) : (7 - quad);

      rotx = rotx_table[quad];
      roty = roty_table[quad];
      sense *= sense_table[quad];

      while(rotx > 0)
	{
	  rotation = rotxmap_table[rotation];
	  rotx--;
	}

      while(roty > 0)
	{
	  rotation = rotymap_table[rotation];
	  roty--;
	}
    }

  return key;
}



