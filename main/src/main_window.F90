module main_window
!
! Module to create and maintain the main window for the
! GadgetViewer program
! 
#include "../../config.h"
  use f90_gui
  use string_module
  use return_status
  use snapshot_reader
  use read_partial
  use particle_store
  use mouse_handler
  use view_parameters
  use transform
  use plotter
  use progress_bar
  use data_types
  use colour_table
  use stereo
  use sampling
  use select_point
  use selection
  use overlay
  use movie
  use graph
  use f90_util
  use summary
  use additional_data
  use catalogue_data
  use gadget_groups
  use group_catalogue
  use screenshot
  use drawing
  use info_window
  use settings
  use configuration
#ifdef HAVE_HDF5
  use read_hdf5
#endif
  use threads

  implicit none
  private
  save

  public :: main_window_create
  public :: main_window_set_snapshot
  public :: main_window_process_events
  public :: main_window_update_controls
  public :: main_window_redraw
  public :: main_window_finalise
  public :: main_window_update

  ! Version string (added to config.h by autoconf)
  character(len=20) :: version = VERSION

  ! Main window widget
  type (gui_window), target, public :: mainwin 

  ! Top level vbox
  type (gui_box)    :: outer_box
  type (gui_box)    :: toolbar

  ! Graphics window
  type (gui_drawing_area) :: drawing_area

  ! File menu
  type (gui_menu)        :: file_menu, file_formats, file_aux
  type (gui_menu_item)   :: file_open_binary_gadget
  type (gui_menu_item)   :: file_open_type2_gadget
  type (gui_menu_item)   :: file_open_hdf5_gadget
  type (gui_menu_item)   :: file_open_snapshot
  type (gui_menu_item)   :: file_open_partial
  type (gui_menu_item)   :: file_read_additional
  type (gui_menu_item)   :: file_read_catalogue
  type (gui_menu)        :: file_read_groups
  type (gui_menu_item)   :: file_screenshot
  type (gui_menu_item)   :: file_summary
  type (gui_menu)        :: file_selection
  type (gui_menu_item)   :: file_save_selection
  type (gui_menu_item)   :: file_load_selection
  type (gui_menu)        :: file_settings
  type (gui_menu_item)   :: file_save_settings
  type (gui_menu_item)   :: file_load_settings
  type (gui_menu_item)   :: file_save_default
  type (gui_menu_item)   :: file_exit
  type (gui_menu)         :: file_subfind
  type (gui_menu_item), dimension(:), allocatable :: groupformat_item
  type (gui_menu_item)   :: file_read_velociraptor
  type (gui_menu_item)   :: file_read_gadget4

  ! View menu
  type (gui_menu)        :: view_menu
  type (gui_menu_item)   :: view_mono
  type (gui_menu)        :: view_stereo_menu
  type (gui_menu_item)   :: view_stereo
  type (gui_menu_item)   :: view_anaglyph
  type (gui_menu_item)   :: view_view_parameters
  type (gui_menu_item)   :: view_auto_resample
  type (gui_menu_item)   :: view_wrap_sample
  type (gui_menu_item)   :: view_follow
  type (gui_menu)        :: view_annotation
  type (gui_menu_item)   :: view_show_selection
  type (gui_menu)        :: view_show_selection_menu
  type (gui_menu_item), dimension(:), allocatable :: show_selection
  type (gui_menu_item)   :: view_show_scale
  type (gui_menu_item)   :: view_show_crosshair
  type (gui_menu_item)   :: view_show_coords
  type (gui_menu_item)   :: view_show_time
  type (gui_menu_item)   :: view_show_redshift
  type (gui_menu_item)   :: view_show_expansion
  type (gui_menu_item)   :: view_show_fps
  type (gui_menu_item)   :: view_show_points
  type (gui_menu_item)   :: view_selected_only
  type (gui_menu_item)   :: view_perspective

  ! Options menu
  type (gui_menu)        :: options_menu
  type (gui_menu_item)   :: options_edit_colour_tables
  type (gui_menu_item)   :: options_jumpto
  type (gui_menu_item)   :: options_select
  type (gui_menu_item)   :: options_movie
  type (gui_menu_item)   :: options_plot

  ! Help menu
  type (gui_menu)      :: help_menu
  type (gui_menu_item) :: help_doc

  type (gui_menu)                          :: options_parallel
  type (gui_menu_item), dimension(nparmax) :: options_nproc

  ! Boxes for control layout
  type (gui_box)         :: hbox, vbox, button_box
  type (gui_box)         :: rot_trans_box

  ! Controls menu
  type (gui_menu)                          :: controls_menu
  type (gui_menu_item)                     :: controls_scheme1
  type (gui_menu_item)                     :: controls_scheme2
  type (gui_menu),      dimension(nbutton) :: controls_button
  type (gui_menu_item), dimension(nbutton) :: controls_click
  type (gui_menu_item), dimension(nbutton, nfunction) :: &
       controls_button_function
  type (gui_menu)                          :: controls_custom
  type (gui_menu_item)                     :: controls_save

  ! Snapshot selector
  type (gui_box)         :: snapshot_box
  type (gui_label)       :: snapshot_label
  type (gui_spin_button) :: snapshot_spinbox

  ! Plot type controls
  type (gui_box)         :: control_box
  type (gui_combo_box)   :: combo_box
  type (gui_button)      :: settings_button

  ! Image buffer
  character, dimension(:), allocatable :: image
  integer :: width, height

  ! Box with particle type toggles
  type (gui_box) :: togglebox
  type (gui_checkbox), dimension(maxspecies) :: species_checkbox

  ! Buttons
  type (gui_button) :: xy_button, yz_button, xz_button, centre_button
  type (gui_button) :: fullscreen_button
  type (gui_button) :: resample_button
  type (gui_button) :: show_all_button
  type (gui_button) :: show_info_button

  ! Optional extra libraries
  integer, parameter :: nextra = 3
  character(len=100), dimension(nextra) :: extra

  ! Stereo separation control
  logical :: stereo_created = .false.
  type (gui_box)    :: stereo_box
  type (gui_label)  :: label
  type (gui_slider) :: stereo_slider

  ! Last particle type selected for display
  integer :: last_type_selected = 1

  ! Gadget group formats we can read
  integer :: ngroupformat
  character(len=maxlen), dimension(:), allocatable :: groupformat

  ! Plotting library
  character(len=80) :: plot_lib

contains

  subroutine stereo_create()

    implicit none
    real :: sep

    if(stereo_created)return

    ! Add slider for stereo separation
    call gui_create_box(stereo_box, toolbar, gui_horizontal)
    call gui_create_label(label, stereo_box, "Stereo sep. ")
    call gui_create_slider(stereo_slider, stereo_box, &
         range=(/ stereo_min_sep, stereo_max_sep/), &
         step=stereo_sep_step, orientation=gui_horizontal, min_size=100)
    call gui_slider_set_value(stereo_slider,stereo_default_sep)
    call gui_slider_get_value(stereo_slider,sep)
    call stereo_set_separation(sep)
    stereo_created = .true.

    return
  end subroutine stereo_create

!
! Create the main window
!
  subroutine main_window_create()

    implicit none
    integer :: i
    character(len=10) :: lbl
    real              :: sep
    ! HDF5 version info
    character(len=20) :: str
    integer :: ibutton, ifunction, istat
    integer :: nproc

    ! Check which libraries are available
    ! Plotting library
    plot_lib = trim(draw_library())
    extra(1) = "Plotting library: "//trim(plot_lib)
    ! HDF5
#ifdef HAVE_HDF5
    call hdf5_version(str)
    extra(2) = "HDF5: enabled, found version "//trim(str)
#else
    extra(2) = "HDF5: disabled"
#endif
    ! OpenMP status message
    extra(3) = openmp_status

    ! Make a new window
    call gui_create_window(mainwin, dimensions=(/600,550/), &
         title="Gadgetviewer", statusbar=.true., menubar=.true.)

    ! Set widgets to fill all available space
    call gui_packing_mode(expand=.true., fill=.true., spacing=3, &
         position=gui_start)

    ! Top level box that contains all other widgets
    call gui_packing_mode(spacing=0)
    call gui_create_box(outer_box, mainwin, gui_vertical)
    call gui_packing_mode(spacing=3)

    ! Box to contain various buttons
    call gui_packing_mode(expand=.false., fill=.false.)
    call gui_create_box(toolbar, outer_box, gui_horizontal)
    call gui_packing_mode(expand=.true., fill=.true.)

    ! HBox to contain graphics window and buttons
    call gui_packing_mode(spacing=0)
    call gui_create_box(hbox, outer_box, gui_horizontal)
    call gui_packing_mode(spacing=3)

    ! Graphics window
    call gui_packing_mode(spacing=0)
    call gui_create_drawing_area(drawing_area, hbox, width=300,height=300, &
         double_buffered=.true.)
    call gui_packing_mode(spacing=3)

    ! Add hbox for horizontal set of buttons
    call gui_packing_mode(expand=.false., fill=.false., spacing=3, &
         position=gui_start)
    call gui_create_box(button_box, toolbar, gui_horizontal)
    call gui_create_button(fullscreen_button, button_box,"_Full screen")
    call gui_create_button(xy_button, button_box, "_x/y")
    call gui_create_button(yz_button, button_box, "_y/z")
    call gui_create_button(xz_button, button_box, "x/_z")
    call gui_create_button(centre_button, button_box, "_Ctr")
    call gui_create_button(resample_button,button_box,"_Resample")
    call gui_create_button(show_all_button,button_box,"Show full volume")
    call gui_create_button(show_info_button, button_box,"Selected point info")

    ! Add a hbox at the bottom
    call gui_packing_mode(expand=.false., fill=.false., spacing=3, &
         position=gui_end)
    call gui_create_box(hbox, outer_box, gui_horizontal)

    ! Add buttons to toggle particle types
    call gui_packing_mode(expand=.false., fill=.false., spacing=3, &
         position=gui_start)
    call gui_create_box(togglebox, hbox, gui_horizontal, frame=.true., &
         label="Particle types to show")
    do i = 1, maxspecies, 1
       write(lbl,'(i3)')i-1
       call gui_create_checkbox(species_checkbox(i), togglebox, &
            trim(adjustl(lbl)))
    end do

    ! Add controls for selecting the plot type
    call gui_packing_mode(expand=.true., fill=.true., spacing=3, &
         position=gui_start)
    call gui_packing_mode(expand=.false., fill=.false., spacing=3, &
         position=gui_start)
    call gui_create_box(control_box, hbox, gui_horizontal, frame=.TRUE., &
         label="Plot type")
    call gui_create_combo_box(combo_box, control_box, typename(1:nplottypes))

    ! Set plot type 2 (2D density) by default
    call gui_combo_box_set_index(combo_box,2)
    call plotter_set_plot_type(2)

    ! Button to bring up settings window
    call gui_create_button(settings_button, control_box, "_Settings")

    ! Add spinbox for snapshot selection
    call gui_packing_mode(expand=.false., fill=.false., spacing=3, &
         position=gui_start)
    call gui_create_box(snapshot_box, hbox, gui_horizontal, &
         frame=.true., label="Snapshot")
    call gui_create_spin_button(snapshot_spinbox, snapshot_box, (/0,9999/), 1)

    ! Add menu options
    call gui_create_menu(file_menu, mainwin, "File")
    call gui_create_menu_item(file_open_snapshot, file_menu, &
         "Read simulation snapshot...")
    call gui_create_menu_item(file_open_partial, file_menu, &
         "Read part of simulation snapshot...")

    ! Set up list of group formats we can read
    call gui_create_menu(file_read_groups, file_menu, &
         "Read groups...")
    call gui_create_menu_item(file_read_gadget4, file_read_groups, &
         "Gadget-4 fof_subhalo_tab (assuming group sorted snapshot)")
#ifndef HAVE_HDF5
    call gui_set_sensitive(file_read_gadget4, .false.)
#endif
    call gui_create_menu_item(file_read_velociraptor, file_read_groups, &
         "VELOCIraptor HDF5")
#ifndef HAVE_HDF5
    call gui_set_sensitive(file_read_velociraptor,.false.)
#endif
    call gui_create_menu(file_subfind, file_read_groups, "Gadget-2/3 Subfind")
    call gadget_groups_format_list(ngroupformat)
    allocate(groupformat(ngroupformat), groupformat_item(ngroupformat), stat=istat)
    if(istat.ne.0)then
       call terminate("Unable to allocate memory")
    endif
    call gadget_groups_format_list(ngroupformat, groupformat)
    do i = 1, ngroupformat, 1
       call gui_create_menu_item(groupformat_item(i), file_subfind, &
            groupformat(i))
    end do

    call gui_create_menu(file_aux, file_menu, "Auxilliary data")
    call gui_create_menu_item(file_read_additional, file_aux, &
         "Read additional particle properties...")
    call gui_create_menu_item(file_read_catalogue, file_aux, &
         "Read labelled points...")

    call gui_create_menu_item(file_summary, file_menu, &
         "Simulation details", separator=.true.)

    call gui_create_menu_item(file_screenshot, file_menu, &
         "Save image as PNG file...", separator=.true.)

    call gui_create_menu(file_selection, file_menu, "Selected particles", &
         separator=.true.)
    call gui_create_menu_item(file_save_selection, file_selection, &
         "Save current selection(s)...")
    call gui_create_menu_item(file_load_selection, file_selection, &
         "Restore saved selection(s)...")

    call gui_create_menu(file_settings, file_menu, "Plot settings")
    call gui_create_menu_item(file_save_settings, file_settings, &
         "Save current settings...")
    call gui_create_menu_item(file_load_settings, file_settings, &
         "Restore saved settings...")
    call gui_create_menu_item(file_save_default, file_settings, &
         "Make current settings default", separator=.true.)

    ! Quit option
    call gui_create_menu_item(file_exit, file_menu, "Exit", separator=.true.)

    ! View menu
    call gui_create_menu(view_menu, mainwin, "View")

    ! Make submenu with annotation options
    call gui_create_menu(view_annotation, view_menu, "Annotations")

    call gui_create_menu_item(view_show_scale, view_annotation, &
         "Show radius scale", checkbox=.true.)
    call gui_menu_item_add_accelerator(view_show_scale, mainwin, &
         key="R")

    call gui_create_menu_item(view_show_crosshair, view_annotation, &
         "Show crosshair", checkbox=.true.)
    call gui_menu_item_set_state(view_show_crosshair, overlay_show_crosshair)
    call gui_menu_item_add_accelerator(view_show_crosshair, mainwin, &
         key="C")

    call gui_create_menu_item(view_show_coords, view_annotation, &
         "Show coordinates", checkbox=.true.)
    call gui_menu_item_set_state(view_show_coords, overlay_show_coords)
    call gui_menu_item_add_accelerator(view_show_coords, mainwin, &
         key="P")

    call gui_create_menu_item(view_show_time, view_annotation, &
         "Show time", checkbox=.true.)
    call gui_menu_item_set_state(view_show_time, overlay_show_time)
    call gui_menu_item_add_accelerator(view_show_time, mainwin, &
         key="T")

    call gui_create_menu_item(view_show_redshift, view_annotation, &
         "Show redshift", checkbox=.true.)
    call gui_menu_item_set_state(view_show_redshift, overlay_show_redshift)
    call gui_menu_item_add_accelerator(view_show_redshift, mainwin, &
         key="Z")

    call gui_create_menu_item(view_show_expansion, view_annotation, &
         "Show expansion", checkbox=.true.)
    call gui_menu_item_set_state(view_show_expansion, overlay_show_expansion)
    call gui_menu_item_add_accelerator(view_show_expansion, mainwin, &
         key="E")

    call gui_create_menu_item(view_show_fps, view_annotation, &
         "Show frame rate", checkbox=.true.)
    call gui_menu_item_set_state(view_show_fps, overlay_show_fps)
    call gui_menu_item_add_accelerator(view_show_fps, mainwin, &
         key="F")

    call gui_create_menu_item(view_show_points, view_annotation, &
         "Show labelled points", checkbox=.true.)
    call gui_menu_item_set_state(view_show_points, overlay_show_points)
    call gui_menu_item_add_accelerator(view_show_points, mainwin, &
         key="L")

    ! Some of the above options are irrelevant without a plotting library
    if(plot_lib.eq."None")then
       call gui_set_sensitive(view_show_time,.false.)
       call gui_set_sensitive(view_show_redshift,.false.)
       call gui_set_sensitive(view_show_expansion,.false.)
       call gui_set_sensitive(view_show_coords,.false.)
       call gui_set_sensitive(view_show_scale,.false.)
       call gui_set_sensitive(view_show_fps, .false.)
    endif

    ! Show selections menu
    call gui_create_menu(view_show_selection_menu, view_menu, &
         "Selected particles")
    call gui_create_menu_item(view_show_selection, view_show_selection_menu, &
         "Highlight selected particles", checkbox=.true.)
    call gui_menu_item_add_accelerator(view_show_selection, mainwin, &
         key="S")
    call gui_menu_item_set_state(view_show_selection, overlay_show_selection)
    allocate(show_selection(size(display_selection)), stat=istat)
    if(istat.ne.0)then
       call terminate("Unable to allocate memory")
    end if
    do i = 1, size(display_selection), 1
       call gui_create_menu_item(show_selection(i), view_show_selection_menu, &
            "Show selection "//trim(adjustl(string(i))), separator=i.eq.1, &
            checkbox=.true.)
       call gui_menu_item_set_state(show_selection(i), display_selection(i-1))
       call gui_menu_item_add_accelerator(show_selection(i), mainwin, &
            key="F"//trim(adjustl(string(i))))
    end do
    call gui_create_menu_item(view_selected_only, view_show_selection_menu, &
         "Only draw selected particles", checkbox=.true., separator=.true.)
    call gui_menu_item_set_state(view_selected_only, draw_selected_only)
    call gui_menu_item_add_accelerator(view_selected_only, mainwin, &
         key="D")

    ! Stereo options
    call gui_create_menu(view_stereo_menu, view_menu, &
         "Stereo")
    call gui_create_menu_item(view_mono, view_stereo_menu, &
         "Mono view", radiobutton=.true.)
    call gui_create_menu_item(view_stereo, view_stereo_menu, &
         "Side by side stereo",radiobutton=.true.,previous=view_mono)
    call gui_create_menu_item(view_anaglyph, view_stereo_menu, &
         "Anaglyph stereo",radiobutton=.true.,previous=view_stereo)
    call gui_menu_item_set_state(view_mono, .true.)

    ! Perspective projection
    call gui_create_menu_item(view_perspective, view_menu, &
         "Perspective projection",checkbox=.true., separator=.true.)
    call gui_menu_item_set_state(view_perspective, perspective_projection)
    call gui_menu_item_add_accelerator(view_perspective, mainwin, &
         key="P", modifier="CTRL")

    ! Auto resample option
    call gui_create_menu_item(view_auto_resample, view_menu, &
         "Automatic resampling", separator=.true., checkbox=.true.)
    call gui_menu_item_set_state(view_auto_resample, auto_resample)
    call gui_menu_item_add_accelerator(view_auto_resample, mainwin, &
         key="A", modifier="CTRL")
    call gui_create_menu_item(view_wrap_sample, view_menu, &
         "Do periodic wrap when resampling", separator=.true., checkbox=.true.)
    call gui_menu_item_set_state(view_wrap_sample, wrap_sample)
    call gui_menu_item_add_accelerator(view_wrap_sample, mainwin, &
         key="W", modifier="CTRL")

    call gui_create_menu_item(view_follow, view_menu, &
         "Centre on selection on snapshot change", checkbox=.true.)
    call gui_menu_item_set_state(view_follow, follow_selection)
    call gui_create_menu_item(view_view_parameters, view_menu, &
         "Change view parameters...", separator=.true.)
    call gui_menu_item_add_accelerator(view_view_parameters, mainwin, &
         key="V", modifier="CTRL")

    ! Options menu
    call gui_create_menu(options_menu, mainwin, "Options")
    call gui_create_menu_item(options_movie, options_menu, "Make a movie...")
    call gui_create_menu_item(options_plot,  options_menu, "Make a plot...")
    if(plot_lib.eq."None")then
       call gui_set_sensitive(options_plot, .false.)
    endif
    call gui_create_menu_item(options_edit_colour_tables, options_menu, &
         "Edit colour tables...", separator=.true.)
    call gui_create_menu_item(options_jumpto, options_menu, &
         "Jump to coordinates...", separator=.true.)
    call gui_menu_item_add_accelerator(options_jumpto, mainwin, &
         key="J", modifier="CTRL")
    call gui_create_menu_item(options_select, options_menu, &
         "Select particles...")
    call gui_menu_item_add_accelerator(options_select, mainwin, &
         key="S", modifier="CTRL")

    ! Controls menu
    call gui_create_menu(controls_menu, mainwin, "Controls")
    call gui_create_menu_item(controls_scheme1, controls_menu, &
         "Default control scheme")
    call gui_menu_item_add_accelerator(controls_scheme1, mainwin, &
         key="D", modifier="CTRL")
    call gui_create_menu_item(controls_scheme2, controls_menu, &
         "One button scheme")
    call gui_menu_item_add_accelerator(controls_scheme2, mainwin, &
         key="O", modifier="CTRL")
    call gui_create_menu(controls_custom, controls_menu, "Customise")
    call gui_create_menu_item(controls_save, controls_menu, &
         "Save control settings",&
         separator=.true.)

    do ibutton = 1, 3, 1
       write(str,'("Drag button ",i1.1)')ibutton
       call gui_create_menu(controls_button(ibutton), controls_custom, str)
       do ifunction = 1, nfunction, 1
          if(ifunction.eq.1)then
             call gui_create_menu_item( &
                  controls_button_function(ibutton, ifunction), &
                  controls_button(ibutton), &
                  control_function(ifunction), &
                  radiobutton=.true.)
          else
             call gui_create_menu_item( &
                  controls_button_function(ibutton, ifunction), &
                  controls_button(ibutton), &
                  control_function(ifunction), &
                  previous=controls_button_function(ibutton, ifunction-1), &
                  radiobutton=.true.)
          endif
       end do
       do ifunction = 1, nfunction, 1
          call gui_menu_item_set_state( &
               controls_button_function(ibutton, ifunction), &
               button_function(ibutton).eq.ifunction)
       end do
       call gui_create_menu_item(controls_click(ibutton), &
            controls_button(ibutton), "Click to centre on cluster", &
            checkbox=.true., separator=.true.)
       call gui_menu_item_set_state(controls_click(ibutton), &
            button_click_centre(ibutton))
    end do

#ifdef _OPENMP
    call gui_create_menu(options_parallel, options_menu, &
         "OpenMP")
    do i = 1, nparmax, 1
       nproc = 2**(i-1)
       write(str,'(i8)')nproc
       if(i.gt.1)then
          call gui_create_menu_item(options_nproc(i), options_parallel, &
               "Use "//trim(adjustl(str))//" threads", &
               radiobutton=.true., previous=options_nproc(i-1))
       else
          call gui_create_menu_item(options_nproc(i), options_parallel, &
               "Use "//trim(adjustl(str))//" thread", &
               radiobutton=.true.)
       endif
       if(nproc.eq.nthreads)then
          call gui_menu_item_set_state(options_nproc(i), .true.)
       endif
    end do
#endif

    ! Update state of controls at the bottom of the window
    call main_window_update_controls()

    ! Display the window
    call gui_show_window(mainwin)

    ! Clear the drawing area
    call gui_drawing_area_get_size(drawing_area,width,height)
    if(allocated(image))deallocate(image)
    allocate(image(0:3*width*height-1), stat=istat)
    if(istat.ne.0)image = char(0)
    call main_window_redraw()

    is_fullscreen = .false.

    call gui_clear_events()
    call progress_bar_init(mainwin)

    return
  end subroutine main_window_create

!
! Process any pending events for the main window
!
  subroutine main_window_process_events()

    implicit none 
    logical :: ok
    character(len=fname_maxlen) :: fname
    integer            :: isnap
    type(result_type)  :: res
    character(len=10)  :: bt, snapnum
    character(len=100) :: fmt_to_read
    integer            :: i
    integer            :: nsp
    logical            :: redraw
    real               :: sep
    logical            :: need_resample, need_redraw
    integer            :: nshown, ishown
    logical            :: status
    integer            :: ifunction, ibutton
    integer            :: istat
    integer            :: group_type, group_subtype

    ! Ignore events while the progress bar is displayed
    if(progress_bar_displayed())return

    ! Make window full screen if button is clicked
    if(gui_button_clicked(fullscreen_button))then
       call gui_fullscreen(mainwin, .not.is_fullscreen)
       is_fullscreen = .not.is_fullscreen
    endif

    if(particle_store_loaded(pdata)) then

       ! Determine particle types to show
       call particle_store_contents(pdata, get_nspecies=nsp)
       nshown = 0
       do i = 1, nsp, 1
          if(show_species(i))then
             nshown = nshown + 1
             ishown = i
          endif
       end do

       ! If there's only one particle type shown, set it as the last
       ! selected so that if the configuration window is opened it will
       ! display settings for this type.
       if(nshown.eq.1)last_type_selected = ishown

       ! Process mouse clicks, dragging etc in the drawing area
       if(mouse_handler_process_events(mainwin,drawing_area,width,height,&
            show_species))then
          call main_window_redraw()
          return
       endif

       ! Redraw screen if any of the checkboxes are toggled
       redraw = .false.
       do i = 1, nsp, 1
          if(gui_checkbox_changed(species_checkbox(i)))then
             redraw=.true.
             call gui_checkbox_get_state(species_checkbox(i), &
                  show_species(i))
             if(show_species(i)) last_type_selected = i
          endif
       end do
       if(redraw)call main_window_redraw()

       ! Check for clicks in info window
       call info_window_process_events()

       ! Resample button - make a sample containing just the area around
       ! the selected point if this button is clicked
       if(gui_button_clicked(resample_button))then
          res = sample_region()
          if(.not.res%success)then
             bt=gui_display_dialog(mainwin,"error",res%string)
             call particle_store_empty(psample)
             call particle_store_empty(pdata)
          else
             call main_window_update()
          endif
          call info_window_update()
          call main_window_redraw()
       endif

       ! Button to sample whole volume
       if(gui_button_clicked(show_all_button))then
          if(particle_store_loaded(pdata))then
             res = sample_region(whole_volume=.true.)
             if(.not.res%success)then
                bt=gui_display_dialog(mainwin,"error",res%string)
                call particle_store_empty(psample)
                call particle_store_empty(pdata)
             else
                call main_window_update()
             endif
             call info_window_update()
             call main_window_redraw()
          endif
       endif

       ! Button to bring up info window
       if(gui_button_clicked(show_info_button))then
          call info_window_open(mainwin)
       endif

       if(gui_menu_item_clicked(file_read_additional))then
          call additional_data_open(mainwin)
       endif

       ! Save selections
       if(gui_menu_item_clicked(file_save_selection))then
          call gui_select_file(mainwin, &
               "Save selected particle IDs", &
               gui_file_save, ok, fname)
          if(ok)then
             res = selection_dump_state(fname)
             if(res%success)then
                bt=gui_display_dialog(mainwin,"info", &
                     "Saved selection to "//trim(fname))
             else
                bt=gui_display_dialog(mainwin,"error", &
                     "Unable to save selection: "//trim(res%string))
             endif
          endif
       endif

       ! Restore selection
       if(gui_menu_item_clicked(file_load_selection))then
          call gui_select_file(mainwin, &
               "Restore saved selection", &
               gui_file_open, ok, fname)
          if(ok)then
             res = selection_restore_state(fname)
             if(res%success)then
                bt=gui_display_dialog(mainwin,"info", &
                     "Loaded selection from "//trim(fname))
                res = selection_apply_all(psample)
                if(.not.res%success)then
                   call selection_clear_all(psample)
                   bt=gui_display_dialog(mainwin,"error", &
                        "Unable to allocate memory to apply selection")
                endif
                call main_window_redraw()
             else
                bt=gui_display_dialog(mainwin,"error", &
                     "Unable to load selection: "//trim(res%string))
             endif
          endif
       endif

       if(gui_menu_item_clicked(file_read_catalogue))then
          call catalogue_data_open(mainwin)
       endif

    endif

    ! Save plot settings
    if(gui_menu_item_clicked(file_save_settings))then
       call gui_select_file(mainwin, &
            "Save plot settings", &
            gui_file_save, ok, fname)
       if(ok)then
          res = save_settings(fname)
          if(res%success)then
             bt=gui_display_dialog(mainwin,"info", &
                  "Saved settings to "//trim(fname))
          else
             bt=gui_display_dialog(mainwin,"error", &
                  "Unable to save settings: "//trim(res%string))
          endif
       endif
    endif

    ! Save default plot settings
    if(gui_menu_item_clicked(file_save_default))then
       fname = trim(default_file)
       res = save_settings(fname)
       if(res%success)then
          bt=gui_display_dialog(mainwin,"info", &
               "Saved settings to "//trim(fname))
       else
          bt=gui_display_dialog(mainwin,"error", &
               "Unable to save settings: "//trim(res%string))
       endif
    endif

    ! Load plot settings
    if(gui_menu_item_clicked(file_load_settings))then
       call gui_select_file(mainwin, &
            "Restore plot settings", &
            gui_file_open, ok, fname)
       if(ok)then
          res = load_settings(fname)
          if(res%success)then
             bt=gui_display_dialog(mainwin,"info", &
                  "Loaded settings from "//trim(fname))
          else
             bt=gui_display_dialog(mainwin,"error", &
                  "Unable to load settings: "//trim(res%string))
          endif
       endif
       ! Update windows which may be affected by the new settings
       call plotter_settings_close() ! May have changed plot type
       res = sample_region(keep_coords=.true.) ! Region to show may have changed
       if(.not.res%success)then
          bt=gui_display_dialog(mainwin,"error", &
               "Unable to sample region (not enough memory?)")
          call particle_store_empty(psample)
          call particle_store_empty(pdata)
       endif
       call main_window_redraw()
       call view_parameters_update()
       call graph_update()
       call movie_update_window()
       call selection_update_window()
       call main_window_update_controls()
    endif

    ! Check if selection display checkboxes have been changed
    do i = 1, size(display_selection), 1
       if(gui_menu_item_changed(show_selection(i)))then
          call gui_menu_item_get_state(show_selection(i), &
               display_selection(i-1))
          call main_window_redraw()
          call graph_update()
       endif
    end do
    if(gui_menu_item_changed(view_selected_only))then
       call gui_menu_item_get_state(view_selected_only, draw_selected_only)
       call main_window_redraw()
    endif

    ! Check if controls have been changed
    do ibutton = 1, nbutton, 1
       do ifunction = 1, nfunction, 1
          if(gui_menu_item_changed( &
               controls_button_function(ibutton,ifunction)))then
             call gui_menu_item_get_state( &
                  controls_button_function(ibutton,ifunction), status)
             if(status)button_function(ibutton) = ifunction
          endif
       end do
       if(gui_menu_item_clicked(controls_click(ibutton)))then
          call gui_menu_item_get_state(controls_click(ibutton), &
               button_click_centre(ibutton))
       endif
    end do
    if(gui_menu_item_clicked(controls_scheme1))then
       button_function = (/ ROTATE, TRANSLATE_XY, SCALE /)
       button_click_centre = (/ .false., .true., .false. /)
       call set_button_menu_items()
    endif
    if(gui_menu_item_clicked(controls_scheme2))then
       button_function = (/ ROTATE_AND_SCALE, NO_ACTION, NO_ACTION /)
       button_click_centre = (/ .true., .false., .false. /)
       call set_button_menu_items()
    endif
    if(gui_menu_item_clicked(controls_save))then
       call mouse_handler_save_settings()
    endif

#ifdef _OPENMP
    ! Check if number of processors has been changed
    do i = 1, nparmax, 1
       if(gui_menu_item_changed(options_nproc(i)))then
          call gui_menu_item_get_state(options_nproc(i), status)
          if(status)then
             call threads_set_number(2**(i-1))             
          endif
       endif
    end do
#endif

    ! Process events for screenshot window
    if(allocated(image)) &
         call screenshot_process_events(mainwin, width, height, image)

    ! Process events from partial snapshot window
    if(read_partial_process_events())then
       call gui_window_set_statusbar(mainwin,"Snapshot file read")
       call main_window_update_controls()
       call view_parameters_initialise()
       call main_window_redraw()
    endif

    ! Process events from movie making window
    if(movie_process_events(mainwin))then
       ! If we made a movie the snapshot number may have changed
       call main_window_update_controls()
       call main_window_redraw()
    endif

    ! Process events from catalogue data window
    if(catalogue_data_process_events(mainwin))then
       call main_window_update_controls()
       call main_window_redraw()
    endif

    ! Process events from read additional data window
    if(additional_data_process_events(mainwin))then
       ! If this returns true it means an extra property has been
       ! loaded into the full dataset stored in pdata. We need to update
       ! the sample.
       res = sample_region(keep_coords=.true.)
       if(.not.res%success)then
          bt=gui_display_dialog(mainwin,"error",res%string)
          call particle_store_empty(psample)
          call particle_store_empty(pdata)
       else
          call main_window_update()
       endif
       call info_window_update()
       call main_window_update_controls()
       call main_window_redraw()
    endif

    ! Process events from settings window
    if(plotter_process_events())then
       call main_window_redraw()
       return
    endif

    ! Process events from jump to position window
    if(select_point_process_events())then
       call info_window_update()
       call main_window_redraw()
       return
    endif

    ! Process events from the graph window
    call graph_process_events()

    ! Process events from summary window
    call summary_process_events()

    ! Process events from view parameters window
    call view_parameters_process_events(need_resample,need_redraw)
    if(need_resample)then
       res = sample_region(keep_coords=.true.)
       if(.not.res%success)then
          bt=gui_display_dialog(mainwin,"error",res%string)
          call particle_store_empty(psample)
          call particle_store_empty(pdata)
       endif
    endif
    if(need_redraw)then
       call main_window_update()
       call main_window_redraw()
       return
    endif

    ! Process events from the particle selection window
    if(selection_process_events(mainwin))then
       call main_window_redraw()
       call graph_update()
       return
    endif

    ! Process events from colour table editor
    if(colour_table_process_events())then
       ! May need to update list of colour tables if settings window open
       call plotter_update_settings()
       ! Redraw the window with the new colour tables
       call main_window_redraw()
       return
    endif

    ! Open settings window if necessary
    if(gui_button_clicked(settings_button))then
       call plotter_settings_open(mainwin,last_type_selected)
    endif

    ! Reset the view if button clicked
    if(gui_button_clicked(xy_button))then
       call transform_modify(view_transform,reset_rotation=.true.)
       view_transform%axis_aligned = 1
       call main_window_redraw()
    endif

    ! Reset the view if button clicked
    if(gui_button_clicked(yz_button))then
       call transform_modify(view_transform,reset_rotation=.true.)
       call transform_modify(view_transform,&
            rotation=&
            real((/0.0_pos_kind,&
            real(3.14159265358979_real8byte/2, kind=pos_kind),&
            real(3.14159265358979_real8byte/2, kind=pos_kind)/), &
            kind=pos_kind))
       view_transform%axis_aligned = 2
       call main_window_redraw()
    endif

    ! Reset the view if button clicked
    if(gui_button_clicked(xz_button))then
       call transform_modify(view_transform,reset_rotation=.true.)
       call transform_modify(view_transform,rotation=&
       real((/real(3.14159265358979_real8byte/2, kind=pos_kind),&
       0.0_pos_kind,0.0_pos_kind/), &
       kind=pos_kind))
       view_transform%axis_aligned = 3
      call main_window_redraw()
    endif

    ! Reset the view if button clicked
    if(gui_button_clicked(centre_button))then
       call transform_modify(view_transform, reset_rotation=.true., &
            set_scale=scalefac, centre=simcentre)
       call info_window_update()
       call main_window_redraw()
    endif
    
    ! End the program if exit clicked or main window closed
    if(gui_menu_item_clicked(file_exit).or.gui_window_closed(mainwin))then
       call plotter_settings_close()
       call gui_quit()
    endif

    ! Detect if the plot type is changed
    if(gui_combo_box_changed(combo_box))then
       call gui_combo_box_get_index(combo_box,i)
       call plotter_set_plot_type(i)
       ! Settings window will change, so close it if its open
       call main_window_redraw()
       return
    endif

    ! If the snapshot number is changed, read the new snapshot
    if(gui_spin_button_changed(snapshot_spinbox).and. &
         particle_store_loaded(pdata))then
       call gui_spin_button_get_value(snapshot_spinbox,isnap)
       write(snapnum,'(1i10)')isnap
       call gui_window_set_statusbar(mainwin,"Reading snapshot "// &
            trim(adjustl(snapnum))//"...")
       call plotter_settings_close()
       res = snapshot_read(isnap)
       if(.not.res%success)then
          bt=gui_display_dialog(mainwin,"error",res%string)
          call gui_window_set_statusbar(mainwin,"Unable to read snapshot")
          call main_window_update_controls()
          call main_window_redraw()
       else
          call gui_window_set_statusbar(mainwin,"Snapshot file read")
          call main_window_update_controls()
          if(follow_selection)then
             ! Centre view approximately
             call selection_recentre_view()
             ! Resample
             res = sample_region()
             if(.not.res%success)then
                bt=gui_display_dialog(mainwin,"error",res%string)
                call particle_store_empty(psample)
                call particle_store_empty(pdata)
             endif
             ! Get a better estimate of the centre
             call selection_recentre_view()
             ! Resample again
             res = sample_region()
             if(.not.res%success)then
                bt=gui_display_dialog(mainwin,"error",res%string)
                call particle_store_empty(psample)
                call particle_store_empty(pdata)
             endif
          endif
          call main_window_redraw()
       endif
    endif

    ! File/Open menu options
    fmt_to_read = "none"
    if(gui_menu_item_clicked(file_open_snapshot)) &
         fmt_to_read="UNKNOWN"
    if(gui_menu_item_clicked(file_open_partial)) &
         call read_partial_open(mainwin)
    ! If one of the File/Open options has been clicked, load a snapshot
    if(fmt_to_read.ne."none")then
       call gui_select_file(mainwin, &
            "Select a file from the snapshot to read", &
            gui_file_open, ok, fname)
       if(ok)then
          call plotter_settings_close()
          call selection_close()
          call movie_close()
          call graph_close()
          call read_partial_close()
          if(fmt_to_read.ne."UNKNOWN")then
             call snapshot_set_format(fmt_to_read)
             res = snapshot_open(fname,isnap)
          else
             res = snapshot_open_unknown(fname, isnap)
          endif
          if(res%success)then
             res = snapshot_read(isnap)
             if(res%success)then
                call gui_window_set_statusbar(mainwin,"Snapshot file read")
                call main_window_update_controls()
                call view_parameters_initialise()
                call main_window_redraw()
                call read_partial_set_filename(fname)
             else
                bt=gui_display_dialog(mainwin,"error",res%string)
                call gui_window_set_statusbar(mainwin, &
                     "Unable to read snapshot")
             endif
          else
             bt=gui_display_dialog(mainwin,"error",res%string)
             call gui_window_set_statusbar(mainwin, &
                  "Unable to read snapshot")
          endif
       endif
    endif

    ! Open colour table editor if the menu option is selected
    if(gui_menu_item_clicked(options_edit_colour_tables))then
       call colour_table_editor_open(mainwin)
    endif

    ! Open selection window
    if(gui_menu_item_clicked(options_select))then
       call selection_open(mainwin)
    endif


    ! Maintain an image buffer the same size as the drawing area
    if(gui_drawing_area_resized(drawing_area))then
       call gui_drawing_area_get_size(drawing_area,width,height)
       if(allocated(image))deallocate(image)
       allocate(image(0:3*width*height-1), stat=istat)
       if(istat.ne.0)image = char(0)
       call main_window_redraw()
    endif

    ! Menu items for mono/stereo modes
    if(gui_menu_item_changed(view_mono).or.&
         gui_menu_item_changed(view_anaglyph).or.&
         gui_menu_item_changed(view_stereo))then
       call stereo_create()
       call gui_menu_item_get_state(view_stereo, stereo_enabled)
       call gui_menu_item_get_state(view_anaglyph, anaglyph_enabled)
       call gui_set_visible(stereo_box, stereo_enabled.or.anaglyph_enabled)
       call main_window_redraw()
    endif

    ! Perspective projection
    if(gui_menu_item_changed(view_perspective))then
       call gui_menu_item_get_state(view_perspective, &
            perspective_projection)
       call main_window_redraw()
    endif

    ! Automatic resampling menu option
    if(gui_menu_item_clicked(view_auto_resample))then
       call gui_menu_item_get_state(view_auto_resample, auto_resample)
    endif

    ! Periodic box wrap menu option
    ! Immediately resample and redraw if this is clicked.
    if(gui_menu_item_clicked(view_wrap_sample))then
       call gui_menu_item_get_state(view_wrap_sample, wrap_sample)
       res = sample_region()
       if(.not.res%success)then
          bt=gui_display_dialog(mainwin,"error",res%string)
          call particle_store_empty(psample)
          call particle_store_empty(pdata)
       else
          call main_window_update()
       endif
       call info_window_update()
       call main_window_redraw()
       return
    endif

    ! Option to follow selected particles on snapshot change
    if(gui_menu_item_clicked(view_follow))then
       call gui_menu_item_get_state(view_follow, follow_selection)
    endif

    ! Option to display selected particles
    if(gui_menu_item_clicked(view_show_selection))then
       call gui_menu_item_get_state(view_show_selection, &
            overlay_show_selection)
       call main_window_redraw()
    endif

    ! Option to display scale
    if(gui_menu_item_clicked(view_show_scale))then
       call gui_menu_item_get_state(view_show_scale, &
            overlay_show_scale)
       call main_window_redraw()
    endif

    ! Option to display cross
    if(gui_menu_item_clicked(view_show_crosshair))then
       call gui_menu_item_get_state(view_show_crosshair, &
            overlay_show_crosshair)
       call main_window_redraw()
    endif

    ! Option to display time
    if(gui_menu_item_clicked(view_show_time))then
       call gui_menu_item_get_state(view_show_time, &
            overlay_show_time)
       call main_window_redraw()
    endif
    if(gui_menu_item_clicked(view_show_redshift))then
       call gui_menu_item_get_state(view_show_redshift, &
            overlay_show_redshift)
       call main_window_redraw()
    endif
    if(gui_menu_item_clicked(view_show_expansion))then
       call gui_menu_item_get_state(view_show_expansion, &
            overlay_show_expansion)
       call main_window_redraw()
    endif

    ! Option to display fps
    if(gui_menu_item_clicked(view_show_fps))then
       call gui_menu_item_get_state(view_show_fps, &
            overlay_show_fps)
       call main_window_redraw()
    endif

    ! Option to display labelled points
    if(gui_menu_item_clicked(view_show_points))then
       call gui_menu_item_get_state(view_show_points, &
            overlay_show_points)
       call main_window_redraw()
    endif

    ! Option to display coordinates
    if(gui_menu_item_clicked(view_show_coords))then
       call gui_menu_item_get_state(view_show_coords, &
            overlay_show_coords)
       call main_window_redraw()
    endif

    ! View parameters window
    if(gui_menu_item_clicked(view_view_parameters))then
       call view_parameters_open(mainwin)
    endif

    ! Jump to position window
    if(gui_menu_item_clicked(options_jumpto))then
       call select_point_open(mainwin)
    endif

    ! Stereo separation slider
    if(stereo_created)then
       if(gui_slider_changed(stereo_slider))then
          call gui_slider_get_value(stereo_slider, sep)
          call stereo_set_separation(sep)
          call main_window_redraw()
       endif
    endif

    ! Movie option
    if(gui_menu_item_clicked(options_movie))then
       call movie_open(mainwin)
    endif

    ! Plot options
    if(gui_menu_item_clicked(options_plot))then
       call graph_open(mainwin)
    endif

    ! Screenshot option
    if(gui_menu_item_clicked(file_screenshot).and. &
         particle_store_loaded(psample))then
       call screenshot_open(mainwin)
    endif

    ! Simulation summary
    if(gui_menu_item_clicked(file_summary).and.&
         particle_store_loaded(psample))then
       call summary_open(mainwin,s)
    endif

    ! Group catalogue reader
    group_type    = -1
    group_subtype = -1
    do i = 1, ngroupformat, 1
       if(gui_menu_item_clicked(groupformat_item(i)))then
          group_type    = FORMAT_TYPE_SUBFIND
          group_subtype = i
       endif
    end do
    if(gui_menu_item_clicked(file_read_velociraptor))then
       group_type=FORMAT_TYPE_VELOCIRAPTOR
    endif
    if(gui_menu_item_clicked(file_read_gadget4))then
       group_type=FORMAT_TYPE_GADGET4
    endif
    if(group_type.ge.0)then
       call gui_select_file(mainwin, "Select a group file", &
            gui_file_open, ok, fname)
       if(ok)then
          call gui_spin_button_get_value(snapshot_spinbox,isnap)
          res = group_catalogue_add(isnap, group_type, group_subtype, fname, snapshot_get_partial_read_info())
          if(.not.res%success)then
             bt=gui_display_dialog(mainwin,"error", res%string)
          else
             bt=gui_display_dialog(mainwin,"info", &
                  "Finished reading group catalogue. New particle properties have been added.")
             res = sample_region(keep_coords=.true.)
             if(.not.res%success)then
                bt=gui_display_dialog(mainwin,"error",res%string)
                call particle_store_empty(psample)
                call particle_store_empty(pdata)
             else
                call main_window_update()
             endif
             call info_window_update()
             call main_window_redraw()
          endif
       endif
    endif

    return

  contains

    subroutine set_button_menu_items()

      implicit none
      integer :: ibutton

      do ibutton = 1, nbutton, 1
         call gui_menu_item_set_state(&
              controls_button_function(ibutton, button_function(ibutton)), &
              .true.)
         call gui_menu_item_set_state(controls_click(ibutton), &
              button_click_centre(ibutton))
      end do

      return
    end subroutine set_button_menu_items

  end subroutine main_window_process_events

!
! Free any memory used by the main window
!
  subroutine main_window_finalise

    implicit none
    
    if(allocated(image))deallocate(image)
    
    call gui_destroy_window(mainwin)

    return
  end subroutine main_window_finalise

!
! Update particle species checkbox states and grey out the snapshot
! selector and some menu options if no data is loaded. Also updates
! the displayed snapshot number.
!
  subroutine main_window_update_controls()

    implicit none
    integer :: i
    integer(kind=index_kind), dimension(maxspecies) :: np_species
    integer :: nspecies
    logical :: data_loaded
    logical, dimension(:), pointer :: state
    integer :: nproc

    data_loaded = particle_store_loaded(pdata)

    if(.not.data_loaded)then
       do i = 1, maxspecies, 1
          call gui_set_sensitive(species_checkbox(i),.FALSE.)
       end do
       call gui_set_sensitive(snapshot_spinbox,.FALSE.)
       nspecies = 0
    else       
       call particle_store_contents(pdata, get_nspecies=nspecies, &
            get_np=np_species)
       do i = nspecies+1, maxspecies, 1
          call gui_set_sensitive(species_checkbox(i),.FALSE.)
       end do
       do i = 1, nspecies, 1
          call gui_set_sensitive(species_checkbox(i),.TRUE.)
       end do
       call main_window_set_snapshot()
    endif
    
    call gui_set_sensitive(combo_box,       data_loaded)
    call gui_set_sensitive(button_box,      data_loaded)
    call gui_set_sensitive(settings_button, data_loaded)

    ! Some menu options aren't available if no simulation is loaded
    call gui_set_sensitive(file_screenshot, data_loaded)
    call gui_set_sensitive(options_jumpto,  data_loaded)
    call gui_set_sensitive(options_select,  data_loaded)
    call gui_set_sensitive(options_movie,   data_loaded)
    if(plot_lib.ne."None")then
       call gui_set_sensitive(options_plot,    data_loaded)
    endif
    call gui_set_sensitive(file_aux,             data_loaded)
    call gui_set_sensitive(file_read_groups,     data_loaded)
    call gui_set_sensitive(file_summary,         data_loaded)
    call gui_set_sensitive(file_load_selection,  data_loaded)
    call gui_set_sensitive(file_save_selection,  data_loaded)

    ! Set particle type checkboxes
    do i = 1, nspecies, 1
       call gui_checkbox_set_state(species_checkbox(i), show_species(i))
    end do

    ! Set plot type combo box
    call gui_combo_box_set_index(combo_box, plotter_get_plot_type())

    ! Set menu checkboxes
    if(stereo_enabled)then
       if(stereo_created)call gui_set_visible(stereo_box,.true.)
       call gui_menu_item_set_state(view_stereo, .true.)
    else if(anaglyph_enabled)then
       if(stereo_created)call gui_set_visible(stereo_box,.true.)
       call gui_menu_item_set_state(view_anaglyph, .true.)
    else
       if(stereo_created)call gui_set_visible(stereo_box,.false.)
       call gui_menu_item_set_state(view_mono, .true.)
    endif
    call gui_menu_item_set_state(view_auto_resample,  auto_resample)
    call gui_menu_item_set_state(view_wrap_sample,  wrap_sample)
    call gui_menu_item_set_state(view_follow,         follow_selection)
    call gui_menu_item_set_state(view_show_selection, overlay_show_selection)
    do i = 1, size(show_selection), 1
       call gui_menu_item_set_state(show_selection(i), display_selection(i-1))
    end do
    call gui_menu_item_set_state(view_show_scale, overlay_show_scale)
    call gui_menu_item_set_state(view_show_crosshair, overlay_show_crosshair)
    call gui_menu_item_set_state(view_show_coords, overlay_show_coords)
    call gui_menu_item_set_state(view_show_time, overlay_show_time)
    call gui_menu_item_set_state(view_show_redshift, overlay_show_redshift)
    call gui_menu_item_set_state(view_show_expansion, overlay_show_expansion)
    call gui_menu_item_set_state(view_show_fps, overlay_show_fps)
    call gui_menu_item_set_state(view_show_points, overlay_show_points)
    call gui_menu_item_set_state(view_selected_only, draw_selected_only)
    call gui_menu_item_set_state(view_perspective, perspective_projection)

    ! OpenMP settings
#ifdef _OPENMP
    do i = 1, nparmax, 1
       nproc = 2**(i-1)
       if(nproc.eq.nthreads)then
          call gui_menu_item_set_state(options_nproc(i), .true.)
       endif
    end do
#endif

    return
  end subroutine main_window_update_controls


  subroutine main_window_redraw()
!
! Redraw the graphics window
!
    implicit none
    integer :: i, iy
    integer :: nspecies
    type (result_type) :: res
    character(len=10)  :: bt
    logical            :: did_sample
    real               :: x_offset, y_offset

    if(.not.particle_store_loaded(pdata))then
       if(allocated(image))then
          image = char(0)
          x_offset = 10.0/width
          y_offset = 25.0/height
          call draw_init_mem(image, width, height)
          call draw_text("Gadgetviewer v"//trim(version), x_offset, 1.0-y_offset)

          do i = 1, nextra, 1
             iy = (25*i)+40
             call draw_text(trim(extra(i)), x_offset, 1.0-((i+2)*y_offset))
          end do
          call draw_text("No data loaded", x_offset, 2.0*y_offset)
          call draw_end()
          call gui_draw_image(drawing_area,image,width,height,0,0)
          call gui_drawing_area_redraw(drawing_area)
       endif
    else

       ! Check if particles need resampling first
       if(auto_resample)then
          res = sample_auto_resample(did_sample)
          if(.not.res%success)then
             ! Resampling may fail if we run out of memory
             bt=gui_display_dialog(mainwin,"error",res%string)
             call particle_store_empty(pdata)
             call particle_store_empty(psample)
             return
          else if(did_sample)then
             call info_window_update()
             call main_window_update()
          endif
       endif

       ! Make the image
       if(allocated(image))then
          call plotter_make_image(width, height, view_transform, image)
          call gui_draw_image(drawing_area,image,width,height,0,0)
          call gui_drawing_area_redraw(drawing_area)
       endif

    endif

    return
  end subroutine main_window_redraw


  subroutine main_window_set_snapshot()
!
! Set the snapshot number
!
    implicit none
    integer :: isnap

    if(particle_store_loaded(pdata))then
       call particle_store_contents(pdata, get_isnap=isnap)
    else
       isnap = -1
    endif

    if(isnap.ne.-1)call gui_spin_button_set_value(snapshot_spinbox,isnap)
    call gui_set_sensitive(snapshot_spinbox, isnap.ne.-1)

    return
  end subroutine main_window_set_snapshot


  subroutine main_window_update()
!
! Display the number of particles plotted in the status bar
!
    implicit none
    integer(kind=index_kind) :: ntot, nshown
    integer                        :: nspecies
    integer(kind=index_kind), dimension(maxspecies) :: np
    integer :: i
    character(len=500) :: str
    real :: fpart

    if(.not.particle_store_loaded(pdata))return

    ! Count particles of the displayed types
    ntot = 0
    call particle_store_contents(pdata, get_np=np, get_nspecies=nspecies)
    do i = 1, nspecies, 1
       ntot = ntot + np(i)
    end do

    ! Count particles in the sample of the displayed types
    nshown = 0
    call particle_store_contents(psample, get_np=np, get_nspecies=nspecies, &
         get_fsample=fpart)
    do i = 1, nspecies, 1
       nshown = nshown + np(i)
    end do

    ! Display the message
    str = "Displaying "//trim(string(nshown))//" particles ("// &
         trim(string(100.0*fpart,fmt='(1f5.1)'))//&
         "% of particles in this region)"

    call gui_window_set_statusbar(mainwin,str)

    return
  end subroutine main_window_update

end module main_window
