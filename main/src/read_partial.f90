module read_partial
!
! Provides an interface for reading only part of a snapshot
!
  use f90_gui
  use particle_store
  use partial_read_info
  use snapshot_reader
  use return_status
  use plotter
  use selection
  use movie
  use graph

  implicit none
  private
  save

  public :: read_partial_open
  public :: read_partial_process_events
  public :: read_partial_close
  public :: read_partial_set_filename

  ! Widgets for the window
  type (gui_window)   :: window
  logical             :: is_open = .false.
  type (gui_entrybox) :: fname_entry
  type (gui_button)   :: browse_button
  type (gui_radio_button) :: all_button
  type (gui_radio_button) :: rate_button
  type (gui_radio_button) :: nmax_button
  type (gui_entrybox) :: rate_box
  type (gui_entrybox) :: nmax_box
  type (gui_entrybox) :: x_box
  type (gui_entrybox) :: y_box
  type (gui_entrybox) :: z_box
  type (gui_entrybox) :: r_box
  type (gui_radio_button) :: thisfile_button
  type (gui_radio_button) :: allfiles_button
  type (gui_radio_button) :: index_button
  type (gui_checkbox) :: spatial_box
  type (gui_checkbox) :: ignore_missing_masses_box
  type (gui_button)   :: ok_button, cancel_button
  type (gui_window), pointer :: mainwin

  ! Widget states
  character(len=500) :: file_name          = ""
  logical            :: read_one_file      = .false.
  logical            :: read_all_files     = .true.
  logical            :: use_index          = .false.
  logical            :: read_all_particles = .true.
  logical            :: read_sphere        = .false.
  character(len=50)  :: xtext, ytext, ztext, rtext, srtext
  logical            :: ignore_missing_masses = .false.

contains
  
  subroutine read_partial_open(mainwin_in)
!
! Open the window
!
    implicit none
    type(gui_window), target   :: mainwin_in
    type(gui_box)      :: outer_vbox, notebook_vbox
    type(gui_box)      :: hbox, vbox
    type(gui_label)    :: label
    integer            :: i
    character(len=50)  :: str
    logical, parameter :: use_expanders = .false.

    if(is_open)return

    mainwin => mainwin_in

    ! Create the window and widgets
    call gui_create_window(window, "Read a snapshot", &
         parent=mainwin, resize=.false.)
    call gui_packing_mode(expand=.false., fill=.false., spacing=3, &
         position=gui_start)

    call gui_create_box(outer_vbox, window, gui_vertical)

    ! Filename box
    call gui_create_box(vbox, outer_vbox, gui_vertical, frame=.true., &
         label="Name of one file from the snapshot")
    call gui_create_box(hbox, vbox, gui_horizontal)
    call gui_create_label(label, hbox, "Filename: ")
    call gui_packing_mode(expand=.true., fill=.true.)
    call gui_create_entrybox(fname_entry, hbox, length=40)
    call gui_packing_mode(expand=.false., fill=.false.)
    call gui_create_button(browse_button, hbox, "Browse")
    call gui_create_box(hbox, vbox, gui_horizontal)

    call gui_create_radio_button(allfiles_button, hbox, "Read all files")
    call gui_create_radio_button(thisfile_button, hbox, "Only read this file",&
         previous=allfiles_button)
    call gui_create_radio_button(index_button,    hbox, "Use spatial index",&
         previous=thisfile_button)

    call gui_create_box(notebook_vbox, outer_vbox, gui_vertical)

    ! Sampling options
    call gui_create_box(vbox, notebook_vbox, gui_vertical, frame=.true., &
         label="Random sampling options", expander=use_expanders)

    call gui_create_box(hbox, vbox, gui_horizontal)
    call gui_create_radio_button(all_button, hbox, "Read all particles")
    call gui_create_box(hbox, vbox, gui_horizontal)
    call gui_create_radio_button(rate_button, hbox, "Set sampling rate: ", &
         previous=all_button)
    call gui_packing_mode(position=gui_end)
    call gui_create_entrybox(rate_box, hbox)
    call gui_packing_mode(position=gui_start)

    ! Spatial selection options
    call gui_create_box(vbox, notebook_vbox, gui_vertical, frame=.true., &
         label="Spatial selection", expander=use_expanders)
    call gui_create_box(hbox, vbox, gui_horizontal)
    call gui_create_checkbox(spatial_box, hbox, &
         "Read particles within distance")
    call gui_create_entrybox(r_box, hbox, length=12)
    call gui_create_label(label, hbox, " of coordinates")
    call gui_create_box(hbox, vbox, gui_horizontal)
    call gui_packing_mode(position=gui_end)
    call gui_create_entrybox(z_box, hbox, length=12)
    call gui_create_label(label, hbox, "Z: ")
    call gui_create_entrybox(y_box, hbox, length=12)
    call gui_create_label(label, hbox, "Y: ")
    call gui_create_entrybox(x_box, hbox, length=12)
    call gui_create_label(label, hbox, "X: ")
    call gui_packing_mode(position=gui_start)

    ! Missing mass option
    call gui_create_box(hbox, vbox, gui_horizontal, frame=.true.)
    call gui_create_checkbox(ignore_missing_masses_box, hbox, "Ignore missing mass datasets (HDF5 only)")

    ! Buttons
    call gui_create_box(vbox, outer_vbox, gui_vertical)
    call gui_create_box(hbox, vbox, gui_horizontal)
    call gui_create_button(ok_button, hbox, "Read particles")
    call gui_packing_mode(position=gui_end)
    call gui_create_button(cancel_button, hbox, "Cancel")
    call gui_packing_mode(position=gui_start)

    ! Display the window
    call gui_show_window(window)
    is_open = .true.

    call set_state()
    call update_window()

    return
  end subroutine read_partial_open
  
  
  logical function read_partial_process_events()
!
! Process events in the window
!
    implicit none
    logical :: ok
    character(len=500)     :: fname, buf
    type (read_info)       :: ri
    integer                :: ios
    character(len=10)      :: bt
    type (result_type)     :: res
    integer                :: isnap

    read_partial_process_events = .false.
    if(.not.is_open)return

    ! Close window if necessary
    if(gui_window_closed(window).or.gui_button_clicked(cancel_button))then
       call read_partial_close()
    endif

    ! Pop up file selector if browse button is clicked
    if(gui_button_clicked(browse_button))then
       call gui_select_file(mainwin, &
            "Select file to read", &
            gui_file_open, ok, fname)
       if(ok)then
          call gui_entrybox_set_text(fname_entry, trim(fname))
       endif
    endif

    ! Grey out entry boxes where necessary
    if(gui_radio_button_changed(rate_button).or. &
         gui_checkbox_changed(spatial_box))then
       call get_state()
       call update_window()
    endif

    ! Respond to read button
    if(gui_button_clicked(ok_button))then
       ! Store window state
       call get_state()
       ! Get file name
       call gui_entrybox_get_text(fname_entry, fname)
       if(len_trim(fname).lt.1)then
          bt = gui_display_dialog(mainwin,"error", &
               "Please specify a file name")
          return
       endif
       ! Get whether to read all files
       call gui_radio_button_get_state(thisfile_button, ri%just_this_file)
       call gui_radio_button_get_state(index_button, ri%use_index)
       ! Get sampling parameters
       call gui_radio_button_get_state(rate_button,     ri%do_sampling)
       if(ri%do_sampling)then
          call gui_entrybox_get_text(rate_box, buf)
          read(buf, *, iostat=ios)ri%sample_rate
          if(ios.ne.0)then
             bt = gui_display_dialog(mainwin,"error", &
                  "Unable to interpret sampling rate")
             return
          endif
          if(ri%sample_rate.le.0.0.or.ri%sample_rate.gt.1.0)then
             bt = gui_display_dialog(mainwin,"error", &
                  "Sample rate must be in range 0-1")
             return
          endif
       else
          ri%sample_rate = 1.0
       endif
       ! Get spatial selection parameters
       call gui_checkbox_get_state(spatial_box, ri%do_sphere)
       if(ri%do_sphere)then
          ri%radius = 0.0
          ri%pos    = 0.0
          ! Radius
          call gui_entrybox_get_text(r_box, buf)
          read(buf, *, iostat=ios)ri%radius
          if(ios.ne.0)then
             bt = gui_display_dialog(mainwin,"error", &
                  "Unable to interpret radius")
             return
          endif
          ! X
          call gui_entrybox_get_text(x_box, buf)
          read(buf, *, iostat=ios)ri%pos(1)
          if(ios.ne.0)then
             bt = gui_display_dialog(mainwin,"error", &
                  "Unable to interpret x coordinate")
             return
          endif
          ! Y
          call gui_entrybox_get_text(y_box, buf)
          read(buf, *, iostat=ios)ri%pos(2)
          if(ios.ne.0)then
             bt = gui_display_dialog(mainwin,"error", &
                  "Unable to interpret y coordinate")
             return
          endif
          ! Z
          call gui_entrybox_get_text(z_box, buf)
          read(buf, *, iostat=ios)ri%pos(3)
          if(ios.ne.0)then
             bt = gui_display_dialog(mainwin,"error", &
                  "Unable to interpret z coordinate")
             return
          endif
       endif

       ! Missing mass option
       call gui_checkbox_get_state(ignore_missing_masses_box, ri%ignore_missing_mass)

       ! Parameters look ok so try to read the data
       call plotter_settings_close()
       call selection_close()
       call movie_close()
       call graph_close()
       res = snapshot_open_unknown(fname, isnap, ri)
       if(.not.res%success)then
          bt = gui_display_dialog(mainwin,"error", &
               "Unable to open snapshot: "//trim(res%string))
          return
       endif
       res = snapshot_read(isnap)
       if(.not.res%success)then
          bt = gui_display_dialog(mainwin,"error", &
               "Unable to read snapshot: "//trim(res%string))
          return
       endif
       ! Need to update the main window so return true
       call read_partial_close()
       read_partial_process_events = .true.
    endif

    if(gui_radio_button_changed(all_button).or. &
         gui_entrybox_changed(fname_entry).or. &
         gui_checkbox_changed(spatial_box).or. &
         gui_radio_button_changed(thisfile_button).or. &
         gui_radio_button_changed(allfiles_button).or. &
         gui_radio_button_changed(index_button).or. &
         gui_entrybox_changed(x_box).or. &
         gui_entrybox_changed(y_box).or. &
         gui_entrybox_changed(z_box).or. &
         gui_entrybox_changed(r_box).or. &
         gui_entrybox_changed(rate_box) &
         )then
       call get_state()
       call update_window()
    endif

    return
  end function read_partial_process_events
  

  subroutine read_partial_close()
!
! Close the window
!
    implicit none

    if(is_open)then
       call gui_destroy_window(window)
       is_open = .false.
    endif

    return
  end subroutine read_partial_close


  subroutine update_window()
!
! Update widgets in the window
!
    implicit none
    logical :: state

    call gui_checkbox_get_state(spatial_box, state)
    call gui_set_sensitive(x_box, state)
    call gui_set_sensitive(y_box, state)
    call gui_set_sensitive(z_box, state)
    call gui_set_sensitive(r_box, state)

    call gui_radio_button_get_state(rate_button, state)
    call gui_set_sensitive(rate_box, state)

    return
  end subroutine update_window


  subroutine get_state()

    implicit none

    call gui_entrybox_get_text(fname_entry, file_name)
    call gui_radio_button_get_state(thisfile_button, read_one_file)
    call gui_radio_button_get_state(allfiles_button, read_all_files)
    call gui_radio_button_get_state(index_button, use_index)
    call gui_radio_button_get_state(all_button, read_all_particles)
    call gui_checkbox_get_state(spatial_box, read_sphere)
    call gui_entrybox_get_text(x_box, xtext)
    call gui_entrybox_get_text(y_box, ytext)
    call gui_entrybox_get_text(z_box, ztext)
    call gui_entrybox_get_text(r_box, rtext)
    call gui_entrybox_get_text(rate_box, srtext)
    call gui_checkbox_get_state(ignore_missing_masses_box, ignore_missing_masses)

    return
  end subroutine get_state


  subroutine set_state()

    implicit none

    call gui_entrybox_set_text(fname_entry, file_name)
    call gui_radio_button_set_state(thisfile_button, read_one_file)
    call gui_radio_button_set_state(allfiles_button, read_all_files)
    call gui_radio_button_set_state(index_button, use_index)
    if(read_all_particles)then
       call gui_radio_button_set_state(all_button, .true.)
    else
       call gui_radio_button_set_state(rate_button, .true.)
    endif
    call gui_checkbox_set_state(spatial_box, read_sphere)
    call gui_entrybox_set_text(x_box, xtext)
    call gui_entrybox_set_text(y_box, ytext)
    call gui_entrybox_set_text(z_box, ztext)
    call gui_entrybox_set_text(r_box, rtext)
    call gui_entrybox_set_text(rate_box, srtext)
    call gui_checkbox_set_state(ignore_missing_masses_box, ignore_missing_masses)
    call update_window()

    return
  end subroutine set_state


  subroutine read_partial_set_filename(fname, rinfo)

    implicit none
    character(len=*) :: fname
    type (read_info), optional :: rinfo

    file_name = trim(fname)
    if(present(rinfo))then
       read_one_file  = rinfo%just_this_file
       read_all_files = .not.rinfo%just_this_file
       use_index      = rinfo%use_index
       read_all_particles = .not.rinfo%do_sampling
       read_sphere = rinfo%do_sphere
       if(rinfo%do_sphere)then
          write(xtext,"(1es12.4)")rinfo%pos(1)
          write(ytext,"(1es12.4)")rinfo%pos(2)
          write(ztext,"(1es12.4)")rinfo%pos(3)
          write(rtext,"(1es12.4)")rinfo%radius
       endif
       if(rinfo%do_sampling)&
            write(srtext,"(1es12.4)")rinfo%sample_rate
    endif
    if(is_open)then
       call set_state()
       call update_window()
    endif

    return
  end subroutine read_partial_set_filename

end module read_partial
