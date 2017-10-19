AUTOMAKE_OPTIONS = foreign

if COND_HDF5
  MAYBE_HDF5_MODULE = $(ax_cv_f90_modflag)../../hdf5/src
  MAYBE_HDF5_LIB    = ../../hdf5/src/libreadhdf5.la
endif

LDADD            = ../../f90_util/src/libf90_util.la $(MAYBE_HDF5_LIB) 
AM_FCFLAGS       = $(ax_cv_f90_modflag)../../f90_util/src/ $(MAYBE_HDF5_MODULE)

lib_LTLIBRARIES = libread_gadget.la

@ax_mod_deps_read_gadget@

libread_gadget_la_SOURCES = read_gadget_definitions.F90 read_gadget_hdf5.F90 read_gadget_binary1.f90 read_gadget_binary2.f90 read_gadget.F90
libread_gadget_la_LIBADD  = ../../f90_util/src/libf90_util.la $(MAYBE_HDF5_LIB) 

nodist_include_HEADERS = read_gadget_definitions.$(ax_cv_f90_modext) read_gadget_hdf5.$(ax_cv_f90_modext) read_gadget_binary1.$(ax_cv_f90_modext) read_gadget_binary2.$(ax_cv_f90_modext) read_gadget.$(ax_cv_f90_modext)
