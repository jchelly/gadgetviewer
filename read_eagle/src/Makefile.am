AUTOMAKE_OPTIONS = foreign

AM_CFLAGS        = @HDF5_CFLAGS@

@ax_mod_deps_read_eagle@

lib_LTLIBRARIES = libreadeagle.la

libreadeagle_la_SOURCES = read_eagle.h read_eagle.c read_eagle_f.c read_eagle_fortran.f90
libreadeagle_la_LIBADD  = @HDF5_LIBS@

