#include <stdio.h>
#include <string.h>
#include "read_eagle.h"
#include "../../config.h"

/*
  Fortran wrappers for read_eagle C functions.

  Intended to be called via read_eagle_fortran.f90.

  Here we assume that strings have been null terminated
  by the fortran caller. Everything else is passed by
  reference.

  Underscores are removed from names to reduce name mangling
  problems.
*/


#define GETERRORF_F90 FC_FUNC (geterrorf, GETERRORF)
void GETERRORF_F90(int *len, char *str)
{
  strncpy(str, get_error(), (size_t) *len);
}

#define ABORTONERRORF_F90 FC_FUNC (abortonerrorf, ABORTONERRORF)
void ABORTONERRORF_F90(int *flag)
{
  abort_on_error(*flag);
}

#define OPENSNAPSHOTF_F90 FC_FUNC (opensnapshotf, OPENSNAPSHOTF)
void OPENSNAPSHOTF_F90(EagleSnapshot **snap, char *fname, double *boxsize,
		       long long *numpart_total, int *numfiles, int *hashbits)
{
  int i;

  *snap = open_snapshot(fname);
  if(*snap)
    {
      *boxsize = (*snap)->boxsize;
      *numfiles = (*snap)->numfiles;
      *hashbits = (*snap)->hashbits;
      for(i=0;i<6;i+=1)
	numpart_total[i] = (*snap)->numpart_total[i];
    }
}

#define CLOSESNAPSHOTF_F90 FC_FUNC (closesnapshotf, CLOSESNAPSHOTF)
void CLOSESNAPSHOTF_F90(EagleSnapshot **snap)
{
  close_snapshot(*snap);
}

#define SELECTREGIONF_F90 FC_FUNC (selectregionf, SELECTREGIONF)
void SELECTREGIONF_F90(EagleSnapshot **snap, 
		       double *xmin, double *xmax,
		       double *ymin, double *ymax,
		       double *zmin, double *zmax)
{
  select_region(*snap,
		*xmin, *xmax,
		*ymin, *ymax,
		*zmin, *zmax);
}

#define CLEARSELECTIONF_F90 FC_FUNC (clearselectionf, CLEARSELECTIONF)
void CLEARSELECTIONF_F90(EagleSnapshot **snap)
{
  clear_selection(*snap);
}

#define COUNTPARTICLESF_F90 FC_FUNC (countparticlesf, COUNTPARTICLESF)
void COUNTPARTICLESF_F90(long long *n, EagleSnapshot **snap, int *itype)
{
  *n = count_particles(*snap, *itype);
}

#define READDATASETF_F90 FC_FUNC (readdatasetf, READDATASETF)
void READDATASETF_F90(long long *nread, EagleSnapshot **snap, int *itype, int *typecode, void *buf, long long *n, char *name)
{
  hid_t dtype_id;
  if(*typecode==0)
    dtype_id = H5T_NATIVE_INT;
  else if(*typecode==1)
    dtype_id = H5T_NATIVE_LLONG;
  else if(*typecode==2)
    dtype_id = H5T_NATIVE_FLOAT;
  else if(*typecode==3)
    dtype_id = H5T_NATIVE_DOUBLE;
  
  *nread = read_dataset(*snap, *itype, name, dtype_id, buf, (size_t) *n);
}

