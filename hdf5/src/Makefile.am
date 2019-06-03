AUTOMAKE_OPTIONS = foreign
AM_CFLAGS        = @HDF5_CFLAGS@
AM_FCFLAGS       = $(ax_cv_f90_modflag)../../f90_util/src/

@ax_mod_deps_hdf5@

lib_LTLIBRARIES = libreadhdf5.la

libreadhdf5_la_SOURCES = read_hdf5_c.c read_hdf5.F90
libreadhdf5_la_LIBADD  = @HDF5_LIBS@ ../../f90_util/src/libf90_util.la

nodist_include_HEADERS = read_hdf5.$(ax_cv_f90_modext)
