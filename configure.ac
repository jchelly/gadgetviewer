AC_INIT(f90_gui/src/f90_gui.F90)
AC_CONFIG_AUX_DIR(./conf/)
AC_CONFIG_MACRO_DIR([m4])

VERSION="1.1.1"
AC_SUBST(VERSION)

AM_INIT_AUTOMAKE(gadgetviewer,${VERSION})
LT_INIT
AM_CONFIG_HEADER(./config.h)

AC_PROG_MAKE_SET
AC_PROG_INSTALL
AC_PROG_CC
AC_PROG_FC

AM_MAINTAINER_MODE([enable])

#
# See if we have python so we can update module dependencies
#
AM_PATH_PYTHON([2.0],, [:])

#
# Test for C compiler features we need
#
AC_LANG(C)
AC_HEADER_STDC
AC_CHECK_SIZEOF(int)
AC_CHECK_SIZEOF(long long)
AC_CHECK_SIZEOF(void *)
AC_CHECK_SIZEOF(double)
AC_CHECK_LIB(m, fabs, [], [${LIBS}="-lm ${LIBS}"])
AC_C_BIGENDIAN

#
# See if we have zlib, because HDF5 may need it. Don't do anything if we
# don't find it because if HDF5 was configured correctly it should be picked
# up automatically where necessary.
#
AC_ARG_WITH(z,AC_HELP_STRING([--with-zlib=PATH],[specify location of zlib]),
  [LDFLAGS="${LDFLAGS} -L${withval}/lib/" ])
AC_CHECK_LIB(z, inflate, [], [], [])

# Find Gtk
AM_PATH_GTK_2_0(2.0.0,[],[AC_MSG_ERROR(GTK+ 2.0 or later not found)])

# Find GLib
GV_FIND_LIBRARY([libglib-2.0], [GLIB], [glib-2.0], [glib-2.0], [g_main_loop_new])

# Find libpng
GV_FIND_LIBRARY([libpng], [LIBPNG], [libpng], [png], [png_create_write_struct])

# Find PLPlot
GV_FIND_LIBRARY([plplot], [PLPLOT], [plplotd], [plplotd], [c_plgver])

# Find Cairo
GV_FIND_LIBRARY([cairo],  [CAIRO],  [cairo],   [cairo],   [cairo_version_string])

# Find HDF5
GV_FIND_LIBRARY([hdf5],  [HDF5],  [hdf5],   [hdf5],   [H5open])

# Check if GLib has the config file parser
have_keyfile="no"
AC_CHECK_LIB(glib-2.0, g_key_file_new, [have_keyfile="yes"], [], [])
if test "${have_keyfile}" == "yes" ; then
  AC_DEFINE([HAVE_KEYFILE],[],[Defined if we have GLib config file parser])
fi

# Only build hdf5 wrapper library if we have HDF5
AM_CONDITIONAL([COND_HDF5], [test "$USE_HDF5" = yes])

#
# Figure out how to use the Fortran compiler
#
AC_LANG(Fortran)
AC_FC_WRAPPERS
AC_FC_SRCEXT(f90)
AX_F90_MODULE_FLAG
AC_SUBST(ax_cv_f90_modflag)
AX_F90_MODULE_EXTENSION
if test "x$ax_cv_f90_modext" = "xunknown" ; then
  AC_MSG_ERROR([unable to find f90 modules extension])
fi
AC_SUBST(ax_cv_f90_modext)

# Check for OpenMP support
AC_OPENMP

#
# Avoid stack overflows by not allocating large arrays on the stack
#
extra_fcflags=""
AC_ARG_ENABLE([heap-arrays],
	[AS_HELP_STRING([--enable-heap-arrays], [prevents stack overflows (default=yes)])],
	[with_heap_arrays="${enableval}"],
	[with_heap_arrays="yes"]
)
if test "$with_heap_arrays" = "yes"; then
  # Intel ifort
  AX_CHECK_COMPILE_FLAG([-heap-arrays],     [extra_fcflags=-heap-arrays], [], [], [])
fi

#
# Try to add some optimization flags
#
AC_ARG_ENABLE([optimization],
	[AS_HELP_STRING([--enable-optimization], [add optimization flags (default=yes)])],
	[with_optimization="${enableval}"],
	[with_optimization="yes"]
)
if test "$with_optimization" = "yes"; then
  # Use -O3 by default
  AX_CHECK_COMPILE_FLAG([-O3], [extra_fcflags="${extra_fcflags} -O3"], [], [], [])
  # Intel
  AX_CHECK_COMPILE_FLAG([-ip],              [extra_fcflags="${extra_fcflags} -ip"],              [], [], [])
  AX_CHECK_COMPILE_FLAG([-no-prec-div],     [extra_fcflags="${extra_fcflags} -no-prec-div"],     [], [], [])
  AX_CHECK_COMPILE_FLAG([-fp-model fast=2], [extra_fcflags="${extra_fcflags} -fp-model fast=2"], [], [], [])
fi

#
# Try to enable compiler run time checks if requested
#
AC_ARG_ENABLE([checks],
	[AS_HELP_STRING([--enable-checks], [enable compiler runtime checks (default=no)])],
	[with_checks="${enableval}"],
	[with_checks="no"]
)
if test "$with_checks" = "yes"; then
# Need to treat warnings as errors for these tests to work correctly with Intel compiler
  AC_LANG_WERROR
# GNU
  AX_CHECK_COMPILE_FLAG([-fcheck=all,no-array-temps], [extra_fcflags="${extra_fcflags} -fcheck=all,no-array-temps"],             [], [], [])
  AX_CHECK_COMPILE_FLAG([-finit-real=snan],           [extra_fcflags="${extra_fcflags} -finit-real=snan"],                       [], [], [])
  AX_CHECK_COMPILE_FLAG([-finit-integer=-10000000],   [extra_fcflags="${extra_fcflags} -finit-integer=-10000000"],               [], [], [])
  AX_CHECK_COMPILE_FLAG([-ffpe-trap=invalid,zero,overflow], [extra_fcflags="${extra_fcflags} -ffpe-trap=invalid,zero,overflow"], [], [], [])
# Intel
  AX_CHECK_COMPILE_FLAG([-check all,noarg_temp_created], [extra_fcflags="${extra_fcflags} -check all,noarg_temp_created"],       [], [], [])
fi
AC_SUBST(extra_fcflags)

# Newer gfortrans don't like calling C functions with different argument types.
# Try to disable this check for now.
AX_CHECK_COMPILE_FLAG([-fallow-argument-mismatch], [extra_fcflags="${extra_fcflags} -fallow-argument-mismatch"], [], [], [])

#
# Check we can read command line arguments from Fortran
#
AC_MSG_CHECKING([if we have Fortran 2003 command line functions])
AC_LINK_IFELSE([AC_LANG_PROGRAM([], [
  implicit none
  integer :: i
  character(len=500) :: str
  i = command_argument_count()
  call get_command_argument(1,str)
])],
AC_MSG_RESULT([yes])
AC_DEFINE([F2003_CMDLINE],[],[Defined if we have F2003 command line]),
AC_MSG_RESULT([no]))

AC_MSG_CHECKING([if we can use getarg() and iargc()])
AC_LINK_IFELSE([AC_LANG_PROGRAM([], [
  implicit none
  integer :: i
  character(len=500) :: str
  i = iargc()
  call getarg(1,str)
])], 
AC_MSG_RESULT([yes])
AC_DEFINE([IARGC_CMDLINE],[],[Defined if iargc() and getarg() can be used]),
AC_MSG_RESULT([no]))

AC_MSG_CHECKING([if we can use getarg() and iargc() if iargc is declared])
AC_LINK_IFELSE([AC_LANG_PROGRAM([], [
  implicit none
  integer :: i
  integer, external :: iargc
  character(len=500) :: str
  i = iargc()
  call getarg(1,str)
])], 
AC_MSG_RESULT([yes])
AC_DEFINE([IARGC_DECLARE],[],
          [Defined if iargc/getarg present but iargc needs to be declared]),
AC_MSG_RESULT([no]))

#
# Check if the shape intrinsic has the optional kind parameter -
# we need this F2003 feature to be able to safely read very large 
# HDF5 datasets.
#
AC_MSG_CHECKING([if the shape() intrinsic has the optional kind parameter])
AC_LINK_IFELSE([AC_LANG_PROGRAM([], [
  implicit none
  integer, dimension(10) :: arr
  integer, dimension(1)  :: arr_shape
  arr_shape = shape(arr, kind=kind(arr))
])], 
AC_MSG_RESULT([yes])
AC_DEFINE([SHAPE_HAS_KIND_PARAM],[],
          [Defined if the shape intrinsic takes the kind parameter]),
AC_MSG_RESULT([no]))

#
# Decide which data types to use for various quantities
#

# Positions
AC_ARG_ENABLE([double-pos], AS_HELP_STRING([--enable-double-pos], [store positions in double precision]))
posbytes=4
AS_IF([test "x$enable_double_pos" = "xyes"], [
  posbytes=8
])
AC_DEFINE_UNQUOTED(POSKIND,[$posbytes],[Number of bytes to store one coordinate]) 

# Velocities
AC_ARG_ENABLE([double-vel], AS_HELP_STRING([--enable-double-vel], [store velocities in double precision]))
velbytes=4
AS_IF([test "x$enable_double_vel" = "xyes"], [
  velbytes=8
])
AC_DEFINE_UNQUOTED(VELKIND,[$velbytes],[Number of bytes to store one coordinate]) 

# Real properties
AC_ARG_ENABLE([double-rprop], AS_HELP_STRING([--enable-double-rprop], [store real properties in double precision]))
rpropbytes=4
AS_IF([test "x$enable_double_rprop" = "xyes"], [
  rpropbytes=8
])
AC_DEFINE_UNQUOTED(RPROPKIND,[$rpropbytes],[Number of bytes to store real particle properties]) 

# Integer properties -
# default to 8 bytes so we can handle 8 byte particle IDs
AC_ARG_ENABLE([double-iprop], AS_HELP_STRING([--enable-double-iprop], [store integer properties in double precision @<:@default=yes@:>@]))
ipropbytes=8
AS_IF([test "x$enable_double_iprop" = "xno"], [
  ipropbytes=4
])
AC_DEFINE_UNQUOTED(IPROPKIND,[$ipropbytes],[Number of bytes to store integer particle properties]) 

# Allow >2**31 particles by default.
# Can disable this to save memory for small simulations.
AC_ARG_ENABLE([big-snapshots], AS_HELP_STRING([--enable-big-snapshots], [allow snapshots with more than 2**31 particles @<:@default=yes@:>@]))
indexbytes=8
AS_IF([test "x$enable_big_snapshots" = "xno"], [
  indexbytes=4
])
AC_DEFINE_UNQUOTED(INDEXKIND,[$indexbytes],[Number of bytes to store indexes in particle arrays]) 


#
# Determine module dependencies
#
if test "$PYTHON" != ":" ; then
  ax_mod_deps_main=./mod-deps-main
  ax_mod_deps_f90_util=./mod-deps-f90_util
  ax_mod_deps_f90_gui=./mod-deps-f90_gui
  ax_mod_deps_hdf5=./mod-deps-hdf5
  ax_mod_deps_drawing=./mod-deps-drawing
  ax_mod_deps_read_eagle=./mod-deps-read_eagle
  ${PYTHON} ${srcdir}/conf/find_module_dependencies.py ${srcdir}/main/src/     ./mod-deps-main
  ${PYTHON} ${srcdir}/conf/find_module_dependencies.py ${srcdir}/f90_util/src/ ./mod-deps-f90_util
  ${PYTHON} ${srcdir}/conf/find_module_dependencies.py ${srcdir}/f90_gui/src/  ./mod-deps-f90_gui
  ${PYTHON} ${srcdir}/conf/find_module_dependencies.py ${srcdir}/hdf5/src/     ./mod-deps-hdf5
  ${PYTHON} ${srcdir}/conf/find_module_dependencies.py ${srcdir}/drawing/src/  ./mod-deps-drawing
  ${PYTHON} ${srcdir}/conf/find_module_dependencies.py ${srcdir}/read_eagle/src/  ./mod-deps-read_eagle
else
  ax_mod_deps_main=/dev/null
  ax_mod_deps_f90_util=/dev/null
  ax_mod_deps_f90_gui=/dev/null
  ax_mod_deps_hdf5=/dev/null
  ax_mod_deps_drawing=/dev/null
  ax_mod_deps_read_eagle=/dev/null
fi
AC_SUBST_FILE(ax_mod_deps_main)
AC_SUBST_FILE(ax_mod_deps_f90_util)
AC_SUBST_FILE(ax_mod_deps_f90_gui)
AC_SUBST_FILE(ax_mod_deps_hdf5)
AC_SUBST_FILE(ax_mod_deps_drawing)
AC_SUBST_FILE(ax_mod_deps_read_eagle)

#
# Output the Makefiles
#
AC_OUTPUT(Makefile drawing/Makefile drawing/src/Makefile main/Makefile main/src/Makefile f90_gui/Makefile f90_gui/src/Makefile f90_util/Makefile f90_util/src/Makefile hdf5/Makefile hdf5/src/Makefile conf/Makefile read_eagle/Makefile read_eagle/src/Makefile doc/Makefile)

#
# Configuration summary
#
AC_MSG_RESULT([
  Gadgetviewer v${VERSION} configuration
  --------------------------------------

  Compilers

    Fortran compiler: ${FC}
    Fortran flags   : ${OPENMP_FCFLAGS} ${extra_fcflags} ${FCFLAGS}
    C compiler      : ${CC}
    C flags         : ${CFLAGS}

  Plotting library (only need one of these)

    PLPlot          : ${USE_PLPLOT}
    Cairo           : ${USE_CAIRO}

  Other libraries

    HDF5            : ${USE_HDF5}
    PNG             : ${USE_LIBPNG}

  Internal precision of particle data

    Positions                     : $posbytes bytes
    Velocities                    : $velbytes bytes
    Floating point properties     : $rpropbytes bytes
    Integer properties (e.g. IDs) : $ipropbytes bytes
    Particle array indexes        : $indexbytes bytes

])
