#include "../../config.h"
#include <stdlib.h>
#include <string.h>
#include <hdf5.h>
#include "hdf5_util.h"

#define MAX_DIMS 7
#define BUFFER_SIZE (2*1024*1024)

/*
  Worker process for reading HDF5 datasets.

  Command line arguments:

  filename
  dataset name
  type code (see read_hdf5_c.c)
  number of dimensions
  offsets (one per dimension)
  counts (one per dimension)

  Writes contents of the dataset to stdout as binary data.
  Only works on 1-7 dimensional arrays of 4/8 byte float/int
  types.

*/

void usage(void) {
  fprintf(stderr, 
	  "\nUsage: gv_hdf5_reader filename dataset typecode ndims\n"	\
	  "           offset_1..offset_ndims count_1..count_ndims\n\n");
  exit(1);
}


int main(int argc, char *argv[])
{

  /* Initialize HDF5 type codes */
  init_hdf5();

  /* Extract command line arguments */
  if(argc < 7) {
    fprintf(stderr, "gv_hdf5_reader: not enough arguments\n");
    exit(1);
  }
  char *file_name    = argv[1];
  char *dataset_name = argv[2];
  int type_code;
  if(sscanf(argv[3], "%d", &type_code)!=1) {
    fprintf(stderr, "gv_hdf5_reader: unable to interpret type code\n");
    usage();
  }
  if((type_code<0) || (type_code>5)){
    fprintf(stderr, "gv_hdf5_reader: type code out of range\n");
    usage();
  }
  int ndim;
  if(sscanf(argv[4], "%d", &ndim)!=1) {
    fprintf(stderr, "gv_hdf5_reader: unable to interpret number of dimensions\n");
    usage();
  }
  if((ndim<1) || (ndim > MAX_DIMS )){
    fprintf(stderr, "gv_hdf5_reader: number of dimensions out of range\n");
    usage();
  }
  /* Check number of arguments now we have ndim */
  if(argc != 5+2*ndim) {
    fprintf(stderr, "gv_hdf5_reader: wrong number of arguments\n");
    usage();
  }
  /* Extract offsets and counts */
  long long offset[MAX_DIMS];
  long long count[MAX_DIMS];
  int i;
  for(i=0;i<ndim;i+=1) {
    if(sscanf(argv[i+5], "%lld", &offset[i])!=1) {
      fprintf(stderr, "gv_hdf5_reader: unable to interpret offset\n");
      usage();
    }
    if(sscanf(argv[i+ndim+5], "%lld", &count[i])!=1) {
      fprintf(stderr, "gv_hdf5_reader: unable to interpret offset\n");
      usage();
    }
  }
  
  /* Open the file */
  hid_t file_id = H5Fopen(file_name, H5F_ACC_RDONLY, H5P_DEFAULT);
  if(file_id<0) {
    fprintf(stderr, "Failed to open file: %s\n", file_name);
    exit(1);
  }

  /* Open the dataset */
  hid_t dataset_id = h5_open_dataset(file_id, dataset_name);
  if(dataset_id<0) {
    fprintf(stderr, "Failed to open dataset: %s\n", dataset_name);
    exit(1);
  }

  /* Get dataset dimensions */
  hid_t filespace_id = H5Dget_space(dataset_id);
  if(filespace_id<0) {
    fprintf(stderr, "Failed to open dataspace for dataset: %s\n", dataset_name);
    exit(1);
  }
  int ndim_file = H5Sget_simple_extent_ndims(filespace_id);
  if(ndim != ndim_file) {
    fprintf(stderr, "Number of dimensions is wrong for dataset: %s\n", dataset_name);
    exit(1);    
  }
  hsize_t h5dims[MAX_DIMS];
  H5Sget_simple_extent_dims(filespace_id, h5dims, NULL);
 
  /* Check that offsets and counts are in range */
  for(i=0;i<ndim;i+=1) {
    if((offset[i] < 0) || (offset[i]+count[i] > h5dims[i])) {
      fprintf(stderr, "Selection out of range for dataset: %s\n", dataset_name);
      exit(1);    
    }
  }
    
  /* Get data type in memory */
  hid_t memtype_id = hdf5_type[type_code];

  /* Determine size of the read buffer */
  long long num_per_element = 1;
  for(i=1;i<ndim;i+=1)
    num_per_element *= count[i];
  long long bytes_per_element = num_per_element * H5Tget_size(memtype_id);
  long long num_elements = count[0] > BUFFER_SIZE ? BUFFER_SIZE : count[0];
  long long num_bytes = bytes_per_element * num_elements;

  /* Allocate buffer */
  char *buffer = malloc(num_bytes);
  if(!buffer) {
      fprintf(stderr, "Failed to allocate read buffer for dataset: %s\n", dataset_name);
      exit(1);        
  }

  /* Create memory dataspace */
  hsize_t mem_dims[MAX_DIMS];
  for(i=1;i<ndim;i+=1)
    mem_dims[i] = count[i];
  mem_dims[0] = num_elements;
  hid_t memspace_id = H5Screate_simple(ndim, mem_dims, NULL); 

  /* Loop over chunks of dataset to read */
  hsize_t h5offset[MAX_DIMS];
  hsize_t h5count[MAX_DIMS];
  for(i=1;i<ndim;i+=1) {
    h5offset[i] = offset[i];
    h5count[i]  = count[i];
  }
  h5offset[0] = offset[0];
  hsize_t nleft = count[0];
  while(nleft > 0) {
    
    /* Find number to read on this iteration */
    h5count[0] = nleft > BUFFER_SIZE ? BUFFER_SIZE : nleft;

    /* Select region in the memory dataspace */
    hsize_t h5zero[MAX_DIMS];
    for(i=0;i<ndim;i+=1)
      h5zero[i] = 0;
    H5Sselect_hyperslab(memspace_id, H5S_SELECT_SET, h5zero, NULL, h5count, NULL);

    /* Select region in the file dataspace */
    H5Sselect_hyperslab(filespace_id, H5S_SELECT_SET, h5offset, NULL, h5count, NULL);

    /* Read the data */
    if(H5Dread(dataset_id, memtype_id, memspace_id, filespace_id, H5P_DEFAULT, buffer) < 0) {
      fprintf(stderr, "Failed to read dataset: %s\n", dataset_name);
      exit(1);
    }

    /* Write to stdout */
    if(fwrite(buffer, sizeof(char), bytes_per_element*h5count[0], stdout) != bytes_per_element*h5count[0]) {
      fprintf(stderr, "Failed to output data for: %s\n", dataset_name);
      exit(1);
    }
  
    /* Update offset and number of elements left */
    h5offset[0] += h5count[0];
    nleft       -= h5count[0];
  }
  
  /* Tidy up */
  H5Sclose(filespace_id);
  H5Sclose(memspace_id);
  H5Dclose(dataset_id);
  H5Fclose(file_id);
  free(buffer);

  return 0;
}
