#include "../../config.h"
#include <stdlib.h>
#include <string.h>
#include <hdf5.h>

/* Check we can use this HDF5 version */
#if (H5_VERS_MAJOR < 1) || ((H5_VERS_MAJOR == 1) && (H5_VERS_MINOR < 6))
#error Need HDF5 1.6 or later!
#endif


/* Define macros for functions which differ between HDF5 versions */
#if ((H5_VERS_MAJOR == 1) && (H5_VERS_MINOR < 8))
/* Use HDF5 1.6 API */
#define h5_open_dataset(file_id, name)      H5Dopen(file_id, name)
#define h5_open_group(file_id, name)        H5Gopen(file_id, name)
#define h5_errors_off                       H5Eset_auto(NULL, NULL)
#define h5_open_attribute(parent_id, name)  H5Aopen_name(parent_id, name)
#else
/* Use HDF5 1.8 API - only "2" versions of functions are guaranteed to exist */
#define h5_open_dataset(file_id, name)      H5Dopen2(file_id, name, H5P_DEFAULT)
#define h5_open_group(file_id, name)        H5Gopen2(file_id, name, H5P_DEFAULT)
#define h5_errors_off                       H5Eset_auto2(H5E_DEFAULT, NULL, NULL)
#define h5_open_attribute(parent_id, name)  H5Aopen_by_name(parent_id, ".", name, H5P_DEFAULT, H5P_DEFAULT)
#endif

static hid_t file_id;
static int   need_init = 1;

/*
  Memory type codes for datasets:

  0 integer*4
  1 integer*8
  2 real*4
  3 real*8
*/

static hid_t hdf5_type[4];

/*
  Initialise HDF5
*/
void init_hdf5()
{
  if(need_init==0)return;
  
  H5open();
  
  /* Find endianness */
  int x = 1;
  if (!(*(char*)&x))
    {
      /* Big endian */
      hdf5_type[0] = H5T_STD_I32BE;
      hdf5_type[1] = H5T_STD_I64BE;
      hdf5_type[2] = H5T_IEEE_F32BE;
      hdf5_type[3] = H5T_IEEE_F64BE;
    }
  else
    {
      /* Little endian */
      hdf5_type[0] = H5T_STD_I32LE;
      hdf5_type[1] = H5T_STD_I64LE;
      hdf5_type[2] = H5T_IEEE_F32LE;
      hdf5_type[3] = H5T_IEEE_F64LE;
    }
  /* Don't print hdf5 errors */
  h5_errors_off;

  need_init = 0;
}


/*
  Open a HDF5 file
 */
#define OPENHDF5_F90 FC_FUNC (openhdf5, OPENHDF5)
void OPENHDF5_F90(char *filename, int *iostat)
{
  init_hdf5();
  file_id = H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT);
  if(file_id < 0)
    *iostat = 1;
  else
    *iostat = 0;
  return;
}

/*
  Report HDF5 version
*/
#define HDF5VERSION_F90 FC_FUNC (hdf5version, HDF5VERSION)
void HDF5VERSION_F90(char *str, int *maxlen)
{
  int i, len;
  char version[500];
  unsigned majnum, minnum, relnum;

  H5get_libversion(&majnum, &minnum, &relnum);
  sprintf(version, "%d.%d.%d", majnum, minnum, relnum);

  for(i=0;i<*maxlen;i++)
    str[i] = ' ';

  len = strlen(version);
  if(*maxlen < len)
    len = *maxlen;
  strncpy(str, version, len);
}

/*
  Read a dataset
 */
#define READDATASET_F90 FC_FUNC (readdataset, READDATASET)
void READDATASET_F90(char *name, int *type, void *data, 
		     int *rank, int *start, int *count, int *iostat)
{
  int i;
  hsize_t dims[7], h5start[7], h5count[7];

  /* Open dataset */
  hid_t dset_id = h5_open_dataset(file_id, name);
  if(dset_id < 0)
    {
      *iostat = 1;
      return;
    }

  /* Create memory dataspace */
  for(i=0;i<(*rank);i++)
    dims[i] = count[*rank - i - 1];
  hid_t memspace_id = H5Screate_simple(*rank, dims, dims); 
  
  /* Get file dataspace */
  hid_t filespace_id = H5Dget_space(dset_id); 
  
  /* Select part of file dataspace */
  for(i=0;i<(*rank);i++)
    {
      h5start[i] = start[*rank - i - 1];
      h5count[i] = count[*rank - i - 1];
    }
  H5Sselect_hyperslab(filespace_id, 
		      H5S_SELECT_SET, h5start, NULL, h5count, NULL);

  /* Read dataset */
  herr_t err = H5Dread(dset_id, hdf5_type[*type], memspace_id, filespace_id, 
		       H5P_DEFAULT, data); 
  H5Sclose(filespace_id);
  H5Sclose(memspace_id);
  if(err != 0)
    {
      *iostat = 1;
      return;
    }
  
  /* Close dataset */
  H5Dclose(dset_id);

  /* Done */
  *iostat = 0;
  return;
}

/*
  Read an attribute
*/
#define READATTRIB_F90 FC_FUNC (readattrib, READATTRIB)
void READATTRIB_F90(char *name, int *type, void *data, int *iostat)
{
  char dset_name[500];

  *iostat = 1;

  /* Find parent group or dataset */
  int i = strlen(name)-1;
  while((i>0) && (name[i] != '/'))
    i = i - 1;
  if(i==0)
      return;

  strncpy(dset_name, name, (size_t) i);
  dset_name[i] = (char) 0;

  /* Open group or dataset */
  hid_t parent_id;
  int   is_group = 0;
  parent_id = h5_open_group(file_id, dset_name); 
  if(parent_id < 0)
    parent_id = h5_open_dataset(file_id, dset_name); 
  else
    is_group = 1;
  if (parent_id < 0)
    return;
  
  /* Open attribute */
  hid_t attr_id = h5_open_attribute(parent_id, &(name[i+1])); 
  if(attr_id >= 0)
    {
      /* Try to read attribute */
      if(H5Aread(attr_id, hdf5_type[*type], data) == 0)
	*iostat = 0;
	
      /* Close attribute */
      H5Aclose(attr_id);
    }

  /* Close group/dataset */
  if(is_group)
    H5Gclose(parent_id);
  else
    H5Dclose(parent_id);

  return;
}

/*
  Get type of a dataset
*/
#define DATASETTYPE_F90 FC_FUNC (datasettype, DATASETTYPE)
void DATASETTYPE_F90(char *name, int *type, int *iostat)
{
  *iostat = 1;

  /* Open dataset */
  hid_t dset_id = h5_open_dataset(file_id, name);
  if(dset_id < 0)
    return;
  
  /* Get data type */
  hid_t dtype_id = H5Dget_type(dset_id);

  /* Set return value, -1 for unknown types */
  *type = -1;
  H5T_class_t class = H5Tget_class(dtype_id);
  size_t       size = H5Tget_size(dtype_id); 
  switch(class)
    {
    case H5T_INTEGER:
      switch(size)
	{
	case 4:
	  *type = 0;
	  break;
	case 8:
	  *type = 1;
	  break;
	}
      break;
    case H5T_FLOAT:
      switch(size)
	{
	case 4:
	  *type = 2;
	  break;
	case 8:
	  *type = 3;
	  break;
	}
      break;
    }

  H5Tclose(dtype_id);
  H5Dclose(dset_id);

  *iostat = 0;
  return;
}

/*
  Get size of a dataset
*/
#define DATASETSIZE_F90 FC_FUNC (datasetsize, DATASETSIZE)
void DATASETSIZE_F90(char *name, int *rank, int *dims, int *iostat)
{
  *iostat = 1;

  /* Open dataset */
  hid_t dset_id = h5_open_dataset(file_id, name);
  if(dset_id < 0)
    return;
  
  /* Get dataspace */
  hid_t dspace_id = H5Dget_space(dset_id);

  /* Get dimensions of dataspace */
  *rank = H5Sget_simple_extent_ndims(dspace_id);
  hsize_t h5dims[20];
  H5Sget_simple_extent_dims(dspace_id, h5dims, NULL); 
  int i;
  for(i=0;i<(*rank);i++)
    dims[i] = h5dims[i];
  
  H5Sclose(dspace_id);
  H5Dclose(dset_id);

  *iostat = 0;
  return;
}

/*
  Close the file
*/
#define CLOSEHDF5_F90 FC_FUNC (closehdf5, CLOSEHDF5)
void CLOSEHDF5_F90(int *iostat)
{
  H5Fclose(file_id);
}



/*
  Visit all datasets below the specified group, recursively opening sub groups.

  Returns 0 on success, a negative value otherwise.
*/

#if ((H5_VERS_MAJOR == 1) && (H5_VERS_MINOR < 8))

/* HDF5 1.6 version using H5G calls */

herr_t find_datasets(hid_t group_id, int nmax, int maxlen, int *nfound, char *datasets, char *path)
{
  /* Get number of objects in group */
  hsize_t num_obj;
  herr_t err = H5Gget_num_objs(group_id, &num_obj);
  if(err<0)return err;

  /* Examine objects in turn */
  hsize_t i;
  for(i=0; i<num_obj;i+=1)
    {
      /* Get name and type of the next object */
      char *name;
      ssize_t len;
      int itype = H5Gget_objtype_by_idx(group_id, i); 
      len = H5Gget_objname_by_idx(group_id, i, NULL, 0);
      len += 1;
      name = malloc(len);
      H5Gget_objname_by_idx(group_id, i, name, len);
      /* Get full name */
      char *new_path = malloc(strlen(path)+strlen(name)+2);
      strcpy(new_path, path);
      if(strlen(new_path) > 0)strcat(new_path, "/");
      strcat(new_path, name);
      /* Decide what to do based on type */
      if(itype==H5G_GROUP)
	{
	  /* Its a group - open and examine it */
	  hid_t subgroup_id = h5_open_group(group_id, name);
	  err = find_datasets(subgroup_id, nmax, maxlen, nfound, datasets, new_path);
	  H5Gclose(subgroup_id);
	  if(err<0)
	    {
	      free(new_path);
	      return err;
	    }
	}
      else if(itype==H5G_DATASET)
	{
	  /* Its a dataset - store its location */
	  if(*nfound < nmax)
	    {
	      len = strlen(new_path) + 1;
	      if(len>maxlen)len=maxlen;
	      strncpy(datasets+(*nfound)*maxlen, new_path, len);
	      *nfound += 1;
	    }
	}
      /* Next object */
      free(name);
      free(new_path);
    }
  /* Success! */
  return 0;
}

#else

/* HDF5 1.8 version - H5G calls to find group members are deprecated and may be removed so use H5O and H5L */

herr_t find_datasets(hid_t group_id, int nmax, int maxlen, int *nfound, char *datasets, char *path)
{
  /* Get number of objects in group */
  hsize_t num_obj;
  H5G_info_t group_info;
  herr_t err = H5Gget_info(group_id, &group_info);
  if(err<0)return err;
  num_obj = group_info.nlinks;

  /* Examine objects in turn */
  hsize_t i;
  for(i=0; i<num_obj;i+=1)
    {
      /* Get type of the next object */
      H5O_info_t object_info;
      H5Oget_info_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_NATIVE, i, &object_info, H5P_DEFAULT); 
      /* Get name of the next object */
      char *name;
      ssize_t len;
      len = H5Lget_name_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_NATIVE, i, NULL, 0, H5P_DEFAULT);
      len += 1;
      name = malloc(len);
      len = H5Lget_name_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_NATIVE, i, name, len, H5P_DEFAULT);
      /* Get full name */
      char *new_path = malloc(strlen(path)+strlen(name)+2);
      strcpy(new_path, path);
      if(strlen(new_path) > 0)strcat(new_path, "/");
      strcat(new_path, name);
      /* Decide what to do based on type */
      if(object_info.type==H5O_TYPE_GROUP)
	{
	  /* Its a group - open and examine it */
	  hid_t subgroup_id = h5_open_group(group_id, name);
	  err = find_datasets(subgroup_id, nmax, maxlen, nfound, datasets, new_path);
	  H5Gclose(subgroup_id);
	  if(err<0)
	    {
	      free(new_path);
	      return err;
	    }
	}
      else if(object_info.type==H5O_TYPE_DATASET)
	{
	  /* Its a dataset - store its location if there's space in the output array */
	  if(*nfound < nmax)
	    {
	      len = strlen(new_path) + 1;
	      if(len>maxlen)len=maxlen;
	      strncpy(datasets+(*nfound)*maxlen, new_path, len);
	    }
	  /* Keep counting even if we can't store any more names */
	  *nfound += 1;
	}
      /* Next object */
      free(name);
      free(new_path);
    }
  /* Success! */
  return 0;
}

#endif


/*
  List datasets under the specified location, recursively searching subgroups
*/
#define LISTDATASETS_F90 FC_FUNC (listdatasets, LISTDATASETS)
void LISTDATASETS_F90(char *groupname, int *nmax, int *maxlen, int *nfound, char *names, int *iostat)
{
  herr_t err;
  int i, j, k;
  int found_null;
  int nconvert;

  /* Open the specified group */
  hid_t group_id = h5_open_group(file_id, groupname);
  if(group_id < 0)return;
  
  /* Search for datasets */
  *iostat = 1;
  *nfound = 0;
  err = find_datasets(group_id, *nmax, *maxlen, nfound, names, "");
  if(err < 0)return;

  /* Convert output names to Fortran strings. Note: we may have found more datasets than can be stored. */
  nconvert = (*nfound) > (*nmax) ? (*nmax) : (*nfound);
  for(i=0;i<nconvert;i+=1)
    {
      found_null = 0;
      for(j=0;j<(*maxlen);j++)
	{
	  k = i*(*maxlen)+j;
	  if((!found_null) && (names[k]==(char) 0))found_null = 1;
	  if(found_null)names[k] = ' ';
	}
    }
  
  /* Success! */
  *iostat = 0;
  return;
}
