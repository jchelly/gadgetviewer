AUTOMAKE_OPTIONS = foreign

if COND_HDF5
  MAYBE_HDF5  = hdf5
  MAYBE_READ_EAGLE = read_eagle
endif
SUBDIRS = f90_util $(MAYBE_HDF5) read_gadget $(MAYBE_READ_EAGLE) drawing f90_gui main conf doc

EXTRA_DIST = README
ACLOCAL_AMFLAGS = -I m4
