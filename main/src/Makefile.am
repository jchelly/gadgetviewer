AUTOMAKE_OPTIONS = foreign

if COND_HDF5
  MAYBE_HDF5_MODULE = $(ax_cv_f90_modflag)../../hdf5/src
  MAYBE_HDF5_LIB    = ../../hdf5/src/libreadhdf5.la
  MAYBE_EAGLE_MODULE = $(ax_cv_f90_modflag)../../read_eagle/src
  MAYBE_EAGLE_LIB    = ../../read_eagle/src/libreadeagle.la
endif

AM_FCFLAGS = $(ax_cv_f90_modflag)../../f90_gui/src/ $(ax_cv_f90_modflag)../../drawing/src/ $(ax_cv_f90_modflag)../../f90_util/src $(MAYBE_HDF5_MODULE) $(MAYBE_EAGLE_MODULE) $(OPENMP_FCFLAGS)

LDADD = ../../drawing/src/libdrawing.la ../../f90_gui/src/libf90_gui.la ../../f90_util/src/libf90_util.la $(MAYBE_HDF5_LIB) $(MAYBE_EAGLE_LIB) @GTK_LIBS@ @HDF5_LIBS@ @PLPLOT_LIBS@ @CAIRO_LIBS@ @LIBPNG_LIBS@

bin_PROGRAMS = gadgetviewer

@ax_mod_deps_main@

gadgetviewer_SOURCES = data_types.F90 shrink_array.f90 math.F90 partial_read_info.f90 data_array.f90 sort.F90 reorder.f90 shuffle.f90 file_units.f90 return_status.f90 transform.f90 octree.F90 percentiles.f90 string.f90 gadget_path.f90  stereo.f90 colour_table.f90 progress_bar.f90 particle_store.F90 auxiliary_file_list.f90 catalogue_data.f90 additional_data.F90 read_gadget_groups.f90 group_catalogue.f90 summary.f90 view_parameters.f90 project.f90 selection.F90 graph.F90 colour_bar.F90 sampling.f90 overlay.F90 select_point.f90  dotplot.f90 density2d.F90 property_plot.f90 fancyplot.F90 smoothed_property_plot.F90 gadget_binary_reader.F90 gadget_binary_type2_reader.F90 gadget_hdf5_reader.F90 gadget_eagle_reader.F90 dummy_reader.F90 plotter.f90 mouse_handler.f90 snapshot_reader.f90 movie_parameters.f90 save_settings.F90 threads.F90 movie.f90 screenshot.f90 read_partial.f90 info_window.f90  main_window.F90 configuration.f90 command_line.f90 gadgetviewer.F90
