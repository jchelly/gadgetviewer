AUTOMAKE_OPTIONS = foreign
AM_CFLAGS        = @HDF5_CFLAGS@
AM_FCFLAGS       = $(ax_cv_f90_modflag)../../f90_util/src/

@ax_mod_deps_hdf5@

nodist_include_HEADERS = read_hdf5.$(ax_cv_f90_modext)

lib_LTLIBRARIES = libhdf5_util.la libreadhdf5.la

libhdf5_util_la_SOURCES = hdf5_util.c
libhdf5_util_la_LIBADD  = @HDF5_LIBS@

libreadhdf5_la_SOURCES = read_hdf5_c.c read_hdf5.F90
libreadhdf5_la_LIBADD   = @HDF5_LIBS@ libhdf5_util.la ../../f90_util/src/libf90_util.la

bin_PROGRAMS = gv_hdf5_reader
gv_hdf5_reader_SOURCES = gv_hdf5_reader.c
gv_hdf5_reader_LDADD   = @HDF5_LIBS@ ./libhdf5_util.la
