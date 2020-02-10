AUTOMAKE_OPTIONS = foreign
AM_CFLAGS        = @LIBPNG_CFLAGS@ @GLIB_CFLAGS@

noinst_LTLIBRARIES = libf90_util.la

@ax_mod_deps_f90_util@

libf90_util_la_SOURCES = terminate_c.c swap_endian.c c_to_fortran.c glob.c getcurrentdir.c file_parser.c write_png.c binary_io.c c_types.F90 terminate.f90 key_file.F90 get_arguments.F90 f90_util.F90 byteswap.f90
libf90_util_la_LIBADD = @LIBPNG_LIBS@ @GLIB_LIBS@

noinst_HEADERS = c_to_fortran.h

#nodist_include_HEADERS = c_types.$(ax_cv_f90_modext) f90_util.$(ax_cv_f90_modext) get_argumentsmod.$(ax_cv_f90_modext) key_file.$(ax_cv_f90_modext) byteswapper.$(ax_cv_f90_modext)
