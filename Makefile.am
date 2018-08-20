AUTOMAKE_OPTIONS = foreign

if COND_HDF5
  MAYBE_HDF5  = hdf5
  MAYBE_READ_EAGLE = read_eagle
endif
SUBDIRS = f90_util $(MAYBE_HDF5) $(MAYBE_READ_EAGLE) drawing f90_gui main conf doc

EXTRA_DIST = README autogen.sh mod-deps-drawing mod-deps-f90_gui mod-deps-f90_util mod-deps-hdf5 mod-deps-main mod-deps-read_eagle
ACLOCAL_AMFLAGS = -I m4
