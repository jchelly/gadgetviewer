#include <hdf5.h>

extern hid_t hdf5_type[6];

void init_hdf5(void);

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
