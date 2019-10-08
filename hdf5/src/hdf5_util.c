#include "hdf5_util.h"

static int need_init = 1;

/*
  Memory type codes for datasets:

  0 integer*4
  1 integer*8
  2 real*4
  3 real*8
  4 unsigned integer*4
  5 unsigned integer*8
*/
hid_t hdf5_type[6];

/*
  Initialise HDF5
*/
void init_hdf5(void)
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
      hdf5_type[4] = H5T_STD_U32BE;
      hdf5_type[5] = H5T_STD_U64BE;
    }
  else
    {
      /* Little endian */
      hdf5_type[0] = H5T_STD_I32LE;
      hdf5_type[1] = H5T_STD_I64LE;
      hdf5_type[2] = H5T_IEEE_F32LE;
      hdf5_type[3] = H5T_IEEE_F64LE;
      hdf5_type[4] = H5T_STD_U32LE;
      hdf5_type[5] = H5T_STD_U64LE;
    }
  /* Don't print hdf5 errors */
  h5_errors_off;

  need_init = 0;
}
