#include "hdf5.h"

#ifndef _READ_EAGLE_H
#define _READ_EAGLE_H

#define MAX_NAMELEN 500

typedef unsigned long long peanokey;

peanokey peano_hilbert_key(int x, int y, int z, int bits);

/*
  Type to store information about an open snapshot
*/
typedef struct
{
  double         boxsize;
  int            numfiles;
  int            hashbits;
  char           basename[MAX_NAMELEN];
  unsigned char *hashmap;
  int            ncell;
  int            nhash;
  long long     *first_key_in_file[6];
  long long     *last_key_in_file[6];
  long long     *num_keys_in_file[6];
  unsigned int **part_per_cell[6];
  unsigned int **first_in_cell[6];
  long long      numpart_total[6];
  int            num_datasets[6];
  char          *dataset_name[6];
} EagleSnapshot;

/* 
   Type codes for returning the type of a dataset
   (mostly for the use of Fortran/Python/IDL wrappers)
*/
typedef enum e_TypeCode
  {
    t_int       = 0,
    t_long_long = 1,
    t_float     = 2,
    t_double    = 3
  } TypeCode;

/* Return a pointer to the last error message */
char *get_error(void);

/* Set whether we should abort on errors */
void abort_on_error(int flag);

/*
  Function to open a snapshot

  Parameters:
    fname - name of one file in the snapshot

  Return value:
    Success - returns a pointer to a new EagleSnapshot
    Failure - returns a null pointer

  The EagleSnapshot should be deallocated with a call to
  close_snapshot() to avoid memory leaks.
*/
EagleSnapshot *open_snapshot(char *fname);


/*
  Function to close a snapshot

  Parameters:
    snap - pointer to the EagleSnapshot
*/
void close_snapshot(EagleSnapshot *snap);


/*
  Function to select a region in a snapshot

  Parameters:
    snap  - pointer to the EagleSnapshot
    xmin  - minimum x coordinate
    xmax  - maximum x coordinate
    ymin  - minimum y coordinate
    ymax  - maximum y coordinate
    zmin  - minimum z coordinate
    zmax  - maximum z coordinate

 Repeated calls to select_region() can be used to select
 oddly shaped or disjoint regions.
*/
void select_region(EagleSnapshot *snap, 
		  double xmin, double xmax,
		  double ymin, double ymax,
		  double zmin, double zmax);


/*
  Clear any selection associated with the specified snapshot

  Parameters:
    snap - pointer to the EagleSnapshot
*/
void clear_selection(EagleSnapshot *snap);


/*
  Count the particles in the selected region

  Parameters:
    snap      - pointer to the eagle snapshot
    itype     - particle type to read

  Return value:
    Success - Number of particles in the selected region
    Failure - a negative number
*/
long long count_particles(EagleSnapshot *snap, int itype);


/*
  Function to read a dataset for all particles in the
  selected region. Works with 1D datasets and 2D Nx3 datasets.

  Parameters:
    snap      - pointer to the eagle snapshot
    itype     - particle type to read
    name      - HDF5 dataset name relative to the PartTypeX group
    hdf5_type - HDF5 type of the output buffer 'buf'
    buf       - buffer in which to store the result
    n         - size of the buffer 'buf'

  Return value:
    Success - the number of particles read
    Failure - a negative number
*/
long long read_dataset(EagleSnapshot *snap, int itype, char *name, hid_t hdf5_type, void *buf, size_t n);

/*
  Macros for reading data into buffers of specific types.

  These avoid the need to use quantities from hdf5.h in the
  calling program. Parameters have the same meaning as in
  read_dataset().
*/
#define read_dataset_int(snap,itype,name,buf,n) read_dataset(snap,itype,name,H5T_NATIVE_INT,buf,n)
#define read_dataset_float(snap,itype,name,buf,n) read_dataset(snap,itype,name,H5T_NATIVE_FLOAT,buf,n)
#define read_dataset_double(snap,itype,name,buf,n) read_dataset(snap,itype,name,H5T_NATIVE_DOUBLE,buf,n)
#define read_dataset_long_long(snap,itype,name,buf,n) read_dataset(snap,itype,name,H5T_NATIVE_LLONG,buf,n)


/*
  Return information about a dataset given its name

  Parameters:
    snap      - pointer to the eagle snapshot
    itype     - particle type to read
    name      - HDF5 dataset name relative to the PartTypeX group
    typecode  - 
    rank      - the rank of the dataset (1 for scalar particle
                properties, 2 for vectors)

 Return value:
    Success - zero
    Failure - non-zero

*/
int get_dataset_info(EagleSnapshot *snap, int itype, char *dset_name, TypeCode *typecode, int *rank);

/*
  Return the number of datasets available for the specified particle type

  Parameters:
    snap      - pointer to the eagle snapshot
    itype     - particle type to read

  Return value:
    Success - the number of datasets
    Failure - a negative number

*/
int get_dataset_count(EagleSnapshot *snap, int itype);

/*
  Get the name of the specified dataset.

  Parameters:
    snap      - pointer to the eagle snapshot
    itype     - particle type to read
    iset      - index of the dataset
    buf       - pointer to the output buffer
    len       - length of the output buffer

  Return value:
    Success - the length of the string copied to the output buffer
    Failure - a negative number

*/
int get_dataset_name(EagleSnapshot *snap, int itype, int iset, char *buf, size_t len);

#endif
