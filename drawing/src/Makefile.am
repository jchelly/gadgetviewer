AUTOMAKE_OPTIONS = foreign

AM_CFLAGS  = @PLPLOT_CFLAGS@ @CAIRO_CFLAGS@
AM_FCFLAGS = $(ax_cv_f90_modflag)../../f90_util/src

@ax_mod_deps_drawing@

noinst_LTLIBRARIES = libdrawing.la

libdrawing_la_SOURCES = tick_interval.c drawing_cairo.c drawing_plplot.c drawing_c.c drawing.F90
libdrawing_la_LIBADD  = @PLPLOT_LIBS@ @CAIRO_LIBS@ ../../f90_util/src/libf90_util.la

noinst_HEADERS = drawing_cairo.h drawing_plplot.h tick_interval.h




