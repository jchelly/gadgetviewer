module movie
!
! This module provides movie making facilities
!
  use f90_gui
  use f90_util
  use particle_store
  use snapshot_reader
  use view_parameters
  use selection
  use colour_table
  use transform
  use plotter
  use select_point
  use selection
  use return_status
  use progress_bar
  use string_module
  use graph
  use data_types
  use sampling
  use plotter
  use movie_parameters
  use settings

  implicit none
  private
  save

  public :: movie_open
  public :: movie_close
  public :: movie_process_events
  public :: movie_set_keys
  public :: movie_get_keys
  public :: movie_update_window
  public :: movie_make

  ! Movie settings window
  type (gui_window) :: window
  logical           :: is_open = .false.

  ! Widgets
  type (gui_box)          :: rotate_vbox, evolve_vbox
  type (gui_radio_button) :: evolve_button
  type (gui_radio_button) :: rotate_button
  type (gui_entrybox)     :: nframes_box, angle_box
  type (gui_entrybox)     :: firstsnap_box, lastsnap_box
  type (gui_checkbox)     :: follow_box
  type (gui_entrybox)     :: fname_box
  type (gui_button)       :: browse_button, ok_button
  type (gui_button)       :: save_button
  type (gui_entrybox)     :: nx_box, ny_box
  type (gui_entrybox)     :: nsmoothmax_box, npmax_box
  type (gui_checkbox)     :: output_graph_box
  type (gui_combo_box)    :: graph_position_box
  type (gui_checkbox)     :: ignore_box

  ! Text for the graph position combo box
  character(len=18), dimension(4) :: pos_text = (/ &
       "as separate file ", &
       "alongside image  ", &
       "as inset         ", &
       "as inset (opaque)"  &
       /)

  ! Inset sizes - these are expressed as a fraction of
  ! the smallest dimension of the output frame.
  real, parameter :: INSET_X = 0.45
  real, parameter :: INSET_Y = 0.40

contains

  subroutine movie_open(mainwin)
!
! Open a window to get parameters for movie making
!
    implicit none
    type (gui_window) :: mainwin
    type (gui_box)    :: outer_hbox, vbox, hbox
    type (gui_box)    :: fname_vbox
    type (gui_label)  :: label
    integer           :: isnap

    if(is_open)return

    if(lastsnap.lt.0)then
       call particle_store_contents(pdata, get_isnap=isnap)
       lastsnap  = isnap
       firstsnap = 0
    endif

    ! Create the window
    call gui_create_window(window, "Make a movie", &
         parent=mainwin, resize=.false.)
    
    ! Top level boxes
    call gui_packing_mode(expand=.false., fill=.false., spacing=3, &
         position=gui_start)
    call gui_create_box(outer_hbox, window, gui_horizontal)
    call gui_create_box(vbox, outer_hbox,   gui_vertical)

    ! Settings for a rotating movie
    call gui_create_box(rotate_vbox, vbox, gui_vertical, frame=.true.)
    call gui_create_box(hbox, rotate_vbox, gui_horizontal)
    call gui_create_radio_button(rotate_button, hbox, "Rotating movie")
    call gui_create_box(hbox, rotate_vbox, gui_horizontal)
    call gui_packing_mode(position=gui_end)
    call gui_create_entrybox(nframes_box, hbox, 8)
    call gui_create_label(label, hbox, "Number of frames to generate: ")
    call gui_create_box(hbox, rotate_vbox, gui_horizontal)
    call gui_create_entrybox(angle_box, hbox, 8)
    call gui_create_label(label, hbox, "Angle to rotate through: ")
    call gui_packing_mode(position=gui_start)
    ! Settings for an evolving movie
    call gui_create_box(evolve_vbox, vbox, gui_vertical, frame=.true.)
    call gui_create_box(hbox, evolve_vbox, gui_horizontal)
    call gui_create_radio_button(evolve_button, hbox, "Evolving movie", &
         previous=rotate_button)
    call gui_packing_mode(position=gui_end)
    call gui_create_box(hbox, evolve_vbox, gui_horizontal)
    call gui_create_entrybox(nsmoothmax_box, hbox, 5)
    call gui_create_label(label,hbox,"Snapshots to smooth path over:")
    call gui_create_box(hbox, evolve_vbox, gui_horizontal)
    call gui_create_checkbox(follow_box, hbox, &
         "Centre frames on selected particles")
    call gui_create_box(hbox, evolve_vbox, gui_horizontal)
    call gui_create_combo_box(graph_position_box, hbox, pos_text)
    call gui_combo_box_set_index(graph_position_box, graph_pos)
    call gui_create_checkbox(output_graph_box, hbox, &
         "Output graph")
    call gui_create_box(hbox, evolve_vbox, gui_horizontal)
    call gui_create_checkbox(ignore_box, hbox, &
         "Ignore unreadable snapshots")
    call gui_checkbox_set_state(ignore_box, ignore_unreadable)
    call gui_create_box(hbox, evolve_vbox, gui_horizontal)
    call gui_create_entrybox(lastsnap_box,  hbox, 5)
    call gui_create_label(label, hbox, " to ")
    call gui_create_entrybox(firstsnap_box, hbox, 5)
    call gui_create_label(label, hbox, "Use snapshots from ")
    call gui_packing_mode(position=gui_start)

    ! Image size
    call gui_create_box(fname_vbox, vbox, gui_vertical, frame=.true.)
    call gui_create_box(hbox, fname_vbox, gui_horizontal)
    call gui_create_label(label, hbox, "Movie dimensions: ")
    call gui_packing_mode(position=gui_end)
    call gui_create_entrybox(ny_box, hbox, 5)
    call gui_create_label(label, hbox, " by ")    
    call gui_create_entrybox(nx_box, hbox, 5)
    call gui_packing_mode(position=gui_start)

    ! Max particles
    call gui_create_box(hbox, fname_vbox, gui_horizontal)
    call gui_packing_mode(position=gui_end)
    call gui_create_entrybox(npmax_box, hbox, 8)
    call gui_packing_mode(position=gui_start)
    call gui_create_label(label, hbox, "Maximum particles to use")

    ! Filename
    call gui_create_box(hbox, fname_vbox, gui_horizontal)
    call gui_create_label(label, hbox, "Path and base name for movie frames:")
    call gui_create_box(hbox, fname_vbox, gui_horizontal)
    call gui_packing_mode(expand=.true.,fill=.true.)
    call gui_create_entrybox(fname_box, hbox)
    call gui_packing_mode(expand=.false.,fill=.false.)
    call gui_create_button(browse_button, hbox, "Browse")

    ! Ok button
    call gui_create_box(hbox, vbox, gui_horizontal)
    call gui_packing_mode(position=gui_end)
    call gui_create_button(ok_button, hbox, "Make movie")
    call gui_packing_mode(position=gui_start)
    call gui_create_button(save_button, hbox, "Save movie parameters")

    ! Set values from module variables
    call gui_entrybox_set_text(nframes_box,string(nframes,fmt='(1i8)'))
    call gui_entrybox_set_text(angle_box,  string(angle,fmt='(1f8.1)'))
    call gui_entrybox_set_text(firstsnap_box, string(firstsnap,fmt='(1i5)'))
    call gui_entrybox_set_text(lastsnap_box,  string(lastsnap, fmt='(1i5)'))
    call gui_checkbox_set_state(follow_box, recentre)
    call gui_entrybox_set_text(nx_box,  string(nx, fmt='(1i5)'))
    call gui_entrybox_set_text(ny_box,  string(ny, fmt='(1i5)'))
    call gui_entrybox_set_text(fname_box,trim(basename))
    call gui_entrybox_set_text(nsmoothmax_box, string(nsmoothmax,fmt='(1i5)'))
    call gui_entrybox_set_text(npmax_box, string(movie_npmax,fmt='(1i8)'))

    ! Display the window
    call gui_show_window(window)
    is_open = .true.

    call movie_update_window()

    return
  end subroutine movie_open


  subroutine movie_close()
!
! Close the movie window
!
    implicit none

    if(is_open)then
       call gui_destroy_window(window)
       is_open = .false.
    endif

    return
  end subroutine movie_close


  logical function movie_process_events(mainwin)
!
! Deal with events in the movie window    
!
    implicit none
    logical :: ok
    character(len=500) :: fname
    type(gui_window)   :: mainwin
    character(len=500) :: str
    integer :: ios
    character(len=20)  :: bt
    logical :: state
    type(result_type)  :: res
    integer            :: ndisplay
    integer            :: i
    real               :: r

    ! Will return true if mian window needs to be updated
    movie_process_events = .false.

    ! Just return if window isn't open
    if(.not.is_open)return

    ! Close window if necessary
    if(gui_window_closed(window))call movie_close()

    ! Pop up file selector if browse button is clicked
    if(gui_button_clicked(browse_button))then
       call gui_select_file(mainwin, &
            "Choose base name for output images", &
            gui_file_save, ok, fname)
       if(ok)then
          call gui_entrybox_set_text(fname_box, trim(fname))
       endif
    endif

    ! Enable/disable widgets depending on movie type
    if(gui_radio_button_changed(rotate_button)) then
       ! Determine movie type
       call gui_radio_button_get_state(rotate_button, state)
       if(state)then
          movie_type = ROTATING
       else
          movie_type = EVOLVING
       endif
       call movie_update_window()
    endif

    ! Rotating movie settings
    if(gui_entrybox_changed(nframes_box))then
       call gui_entrybox_get_text(nframes_box, str)
       read(str,*,iostat=ios)i
       if(ios.eq.0.and.i.gt.0)then
          nframes = i
       else
          call movie_update_window()
       endif
    endif
    if(gui_entrybox_changed(angle_box))then
       call gui_entrybox_get_text(angle_box, str)
       read(str,*,iostat=ios)r
       if(ios.eq.0)then
          angle = r
       else
          call movie_update_window()
       endif
    endif

    ! Evolving movie settings
    if(gui_entrybox_changed(firstsnap_box))then
       call gui_entrybox_get_text(firstsnap_box, str)
       read(str,*,iostat=ios)i
       if(ios.eq.0.and.i.gt.0)then
          firstsnap = i
       else
          call movie_update_window()
       endif
    endif
    if(gui_entrybox_changed(lastsnap_box))then
       call gui_entrybox_get_text(lastsnap_box, str)
       read(str,*,iostat=ios)i
       if(ios.eq.0.and.i.gt.0)then
          lastsnap = i
       else
          call movie_update_window()
       endif
    endif
    if(gui_checkbox_changed(follow_box))&
         call gui_checkbox_get_state(follow_box,recentre)
    if(gui_checkbox_changed(ignore_box))&
         call gui_checkbox_get_state(ignore_box,ignore_unreadable)
    if(gui_checkbox_changed(output_graph_box))then
       call gui_checkbox_get_state(output_graph_box, output_graph)
       call movie_update_window()
    endif
    if(gui_combo_box_changed(graph_position_box)) &
         call gui_combo_box_get_index(graph_position_box, graph_pos)

    ! Common settings
    if(gui_entrybox_changed(nx_box))then
       call gui_entrybox_get_text(nx_box, str)
       read(str,*,iostat=ios)i
       if(ios.eq.0.and.i.gt.0)then
          nx = i
       else
          call movie_update_window()
       endif
    endif
    if(gui_entrybox_changed(ny_box))then
       call gui_entrybox_get_text(ny_box, str)
       read(str,*,iostat=ios)i
       if(ios.eq.0.and.i.gt.0)then
          ny = i
       else
          call movie_update_window()
       endif
    endif
    if(gui_entrybox_changed(fname_box))&
         call gui_entrybox_get_text(fname_box, basename)
    if(gui_entrybox_changed(nsmoothmax_box))then
       call gui_entrybox_get_text(nsmoothmax_box, str)
       read(str,*,iostat=ios)i
       if(ios.eq.0.and.i.gt.0)then
          nsmoothmax = i
       else
          call movie_update_window()
       endif
    endif
    if(gui_entrybox_changed(npmax_box))then
       call gui_entrybox_get_text(npmax_box, str)
       read(str,*,iostat=ios)i
       if(ios.eq.0.and.i.gt.0)then
          movie_npmax = i
       else
          call movie_update_window()
       endif
    endif

    ! Save settings
    if(gui_button_clicked(save_button))then
       call gui_select_file(mainwin, &
            "Save movie settings", &
            gui_file_save, ok, fname)
       if(ok)then
          ! Update module variables from GUI
          call movie_get_parameters()
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

    ! Ok button
    if(gui_button_clicked(ok_button).and.particle_store_loaded(pdata))then
       call movie_get_parameters()
       ! Determine movie type
       select case(movie_type)
       case(ROTATING)
          ! Number of frames
          if(nframes.lt.1)then
             bt=gui_display_dialog(window,"error",&
                  "Number of frames must be positive")
             return
          endif
       case(EVOLVING)
          if(firstsnap.gt.lastsnap)then
             bt=gui_display_dialog(window,"error", &
                  "First snapshot must be before last snapshot")
             return
          endif
          if(firstsnap.lt.0.or.lastsnap.lt.0)then
             bt=gui_display_dialog(window,"error", &
                  "Snapshot numbers must be positive")
             return
          endif
       end select
       ! Parameters which apply to all movies
       ! Dimensions of the images
       if(nx.lt.100.or.ny.lt.100.or.nx.gt.10000.or.ny.gt.10000)then
          bt=gui_display_dialog(window,"error", &
               "Image dimensions must be in range 100-10000")
          return
       endif
       ! Check path is not blank
       if(len_trim(basename).eq.0)then
          bt=gui_display_dialog(window,"error", &
               "You need to specify a path to write the images to")
          return
       endif
       ! Number of snapshots to smooth path over       
       if(nsmoothmax.lt.0.or.nsmoothmax.gt.1000)then
          bt=gui_display_dialog(window,"error", &
               "Number of frames to smooth path over"//&
               " must be in range 0-1000")
          return
       endif
       ! Number of particles to use
       if(movie_npmax.le.0)then
          bt=gui_display_dialog(window,"error", &
               "Maximum number of particles must be > 0")
          return
       endif

       ! If we get this far, we can try to make a movie
       call movie_close_windows()
       res = movie_make()
       
       ! Will probably need a redraw now 
       movie_process_events = .true.

       ! Report whether it worked
       if(res%success)then
          bt=gui_display_dialog(mainwin,"info", &
               "Finished writing movie frames")
       else
          bt=gui_display_dialog(mainwin,"error", &
               "Movie making failed: "//trim(res%string))
       endif

       ! Sample the particles at the original sampling rate
       res = sample_region(keep_coords=.true.)
       if(.not.res%success)then
          call particle_store_empty(psample)
          call particle_store_empty(pdata)
          bt=gui_display_dialog(mainwin,"error", &
               "Resampling failed: "//trim(res%string))
          return
       endif
       
       return
    endif

    return
  end function movie_process_events


  type (result_type) function movie_make() result(res)
!
! Make a movie!
!
    implicit none
    integer :: ndisplay

    call progress_bar_display("Making movie...", cancel=.true.)

    ! Reset number of particles to show
    ndisplay      = npmax_display
    npmax_display = movie_npmax

    ! Sample the particles at the new sampling rate
    res = sample_region(keep_coords=.true.)
    if(.not.res%success)then
       call particle_store_empty(psample)
       call particle_store_empty(pdata)
       return
    endif

    ! Make the movie
    select case(movie_type)
    case(ROTATING)
       res = movie_make_rotating()
    case(EVOLVING)
       res = movie_make_evolving()
    end select
    call progress_bar_close()

    ! Put particles to display back to original value
    npmax_display = ndisplay

    return
  end function movie_make


  subroutine movie_update_window()
!
! Update the state of the widgets in the movie window
!
    implicit none
    logical :: is_set
    character(len=500) :: str

    if(.not.is_open)return
   
    call movie_get_parameters()

    call gui_checkbox_get_state(output_graph_box, is_set)
    call gui_set_sensitive(graph_position_box, is_set)
 
    ! Set values in widgets
    if(movie_type.eq.EVOLVING)then
       call gui_radio_button_set_state(evolve_button, .true.)
    else
       call gui_radio_button_set_state(rotate_button, .true.)
    endif
    write(str,*)nframes
    call gui_entrybox_set_text(nframes_box, trim(adjustl(str)))
    write(str,*)angle
    call gui_entrybox_set_text(angle_box, trim(adjustl(str)))    
    write(str,*)firstsnap
    call gui_entrybox_set_text(firstsnap_box, trim(adjustl(str)))    
    write(str,*)lastsnap
    call gui_entrybox_set_text(lastsnap_box, trim(adjustl(str)))    
    call gui_checkbox_set_state(follow_box, recentre)
    call gui_entrybox_set_text(fname_box, trim(adjustl(basename)))    
    write(str,*)nx
    call gui_entrybox_set_text(nx_box, trim(adjustl(str)))
    write(str,*)ny
    call gui_entrybox_set_text(ny_box, trim(adjustl(str)))
    write(str,*)nsmoothmax
    call gui_entrybox_set_text(nsmoothmax_box, trim(adjustl(str)))  
    write(str,*)movie_npmax
    call gui_entrybox_set_text(npmax_box, trim(adjustl(str))) 
    call gui_checkbox_set_state(output_graph_box, output_graph)
    call gui_checkbox_set_state(ignore_box, ignore_unreadable)
    call gui_combo_box_set_index(graph_position_box, graph_pos)

    call gui_radio_button_get_state(rotate_button, is_set)
    if(is_set)then
       call gui_set_sensitive(nframes_box,   .true.)
       call gui_set_sensitive(angle_box,     .true.)
       call gui_set_sensitive(firstsnap_box, .false.)
       call gui_set_sensitive(lastsnap_box,  .false.)
       call gui_set_sensitive(follow_box,    .false.)
       call gui_set_sensitive(nsmoothmax_box,.false.)
       call gui_set_sensitive(output_graph_box, .false.)
       call gui_set_sensitive(graph_position_box, .false.)
       call gui_set_sensitive(ignore_box, .false.)
    else
       call gui_set_sensitive(nframes_box,   .false.)
       call gui_set_sensitive(angle_box,     .false.)
       call gui_set_sensitive(firstsnap_box, .true.)
       call gui_set_sensitive(lastsnap_box,  .true.)
       call gui_set_sensitive(follow_box,    .true.)
       call gui_set_sensitive(nsmoothmax_box,.true.)
       call gui_set_sensitive(output_graph_box, .true.)
       call gui_set_sensitive(graph_position_box, .true.)
       call gui_set_sensitive(ignore_box, .true.)
    endif

    return
  end subroutine movie_update_window



  type (result_type) function movie_make_rotating()
!
! Make a movie rotating about the selected point
!
    implicit none
    double precision                            :: dtheta
    character(len=1), dimension(:), allocatable :: image
    integer                                     :: iframe
    type(transform_type)                        :: trans
    integer :: nspecies
    character(len=500)                          :: fname
    integer                                     :: stat

    ! Determine rotation to apply between frames and convert to radians
    dtheta = dble(angle) / dble(nframes)
    dtheta = dtheta * (3.14159/180.0)

    ! Allocate image array
    allocate(image(0:3*nx*ny-1), stat=stat)
    if(stat.ne.0)then
       movie_make_rotating%string="Unable to allocate memory for movie frames"
       movie_make_rotating%success = .false.
       return
    endif
    
    ! Make a copy of the view transform so we don't affect the
    ! view in the main window (makes it easier to make the same movie
    ! again, e.g. with different resolutions, colour maps etc)
    trans = view_transform

    call particle_store_contents(psample, get_nspecies=nspecies)
    call progress_bar_update(0.0)

    ! Loop over frames
    do iframe = 0, nframes-1, 1

       ! Make an image
       call plotter_make_image(nx, ny, trans, image)

       ! Save the image as a png file. Give up and return if this fails.
       write(fname,'(a,"_",i5.5,".png")')trim(basename),iframe
       if(.not.write_png(fname,nx,ny,image))then
          movie_make_rotating%success = .false.
          movie_make_rotating%string="Unable to write PNG file "//trim(fname)
          deallocate(image)
          return
       endif

       ! Apply rotation for the next frame
       call transform_modify(trans, real((/ 0.0_pos_kind,&
       real(dtheta, kind=pos_kind),&
       0.0_pos_kind /),&
       kind=pos_kind))

       ! Update the progress bar
       if(nframes.gt.1) &
            call progress_bar_update(real(iframe+1)/real(nframes))

       ! Check if the cancel button has been pressed
       if(progress_bar_cancelled())then
          movie_make_rotating%success = .false.
          movie_make_rotating%string="Cancelled by user"
          deallocate(image)
          return
       endif

    end do

    ! Return successfully
    movie_make_rotating%success = .true.

    deallocate(image)

    return
  end function movie_make_rotating


  subroutine movie_close_windows()
!
! Close windows while making movie to avoid interfering with settings
!
    implicit none

    call view_parameters_close()
    call plotter_settings_close()
    call colour_table_editor_close()
    call select_point_close()
    call selection_close()
    call movie_close()
    call graph_close()

    return
  end subroutine movie_close_windows


  type (result_type) function movie_make_evolving()
!
! Make an evolving movie
!
    implicit none
    character(len=1), dimension(:), allocatable :: image
    character(len=1), dimension(:), pointer     :: plot_image => null()
    character(len=1), dimension(:), allocatable :: combined_image
    integer               :: iframe, nframes
    integer               :: nspecies
    type(result_type)     :: res
    integer               :: stat
    character(len=500)    :: fname
    real(kind=pos_kind), dimension(:,:), allocatable :: movie_centre
    logical, dimension(:), allocatable :: centre_ok
    integer               :: jframe, nfound
    integer               :: i, j, k
    integer               :: gx, gy
    integer               :: nsnap

    ! Allocate image array
    allocate(image(0:3*nx*ny-1), plot_image(0:3*nx*ny-1), & !stat
         combined_image(0:6*nx*ny-1), movie_centre(3,firstsnap:lastsnap), &
         centre_ok(firstsnap:lastsnap), stat=stat)
    if(stat.ne.0)then
       movie_make_evolving%string="Unable to allocate memory for movie frames"
       movie_make_evolving%success = .false.
       return
    endif
    centre_ok = .false.

    call particle_store_contents(psample, get_nspecies=nspecies)
    call progress_bar_update(0.0)

    ! Loop over frames. In this case the frame number is the snapshot
    ! number
    nsnap     = 0 ! Number of frames made
    nframes   = lastsnap-firstsnap+1
    do iframe = firstsnap, lastsnap, 1

       ! Read in the appropriate snapshot
       res = snapshot_read(iframe)
       if(.not.res%success)then
          ! If we can't read the snapshot either abort or just go on
          ! to the next one
          if(ignore_unreadable)then
             ! Skip this snapshot
             cycle
          else
             ! Abort and return
             movie_make_evolving = res
             deallocate(image)
             return
          endif
       endif
       
       ! May want to recenter on selected particles
       if(recentre)then
          call selection_recentre_view()
          ! Resample particles
          res = sample_region()
          if(.not.res%success)then
             ! Ran out of memory making the sample
             deallocate(image)
             movie_make_evolving = res
             return
          endif
          ! May be able to find more of the selected particles 
          ! in the sample now
          call selection_recentre_view()
          ! Resample with new centre
          res = sample_region()
          if(.not.res%success)then
             ! Ran out of memory making the sample
             deallocate(image)
             movie_make_evolving = res
             return
          endif
       endif

       ! Smooth path of centre over a number of frames, omitting any
       ! frames we couldn't read
       movie_centre(1:3,iframe) = view_transform%centre
       centre_ok(iframe)        = .true.
       if(nsmoothmax.gt.0)then
          view_transform%centre(1:3) = 0.0
          jframe = iframe
          nfound = 0
          do while(jframe.ge.firstsnap.and.nfound.lt.nsmoothmax)
             if(centre_ok(jframe))then
                view_transform%centre(1:3) = view_transform%centre(1:3) + &
                     movie_centre(1:3,jframe)
                nfound = nfound + 1
             endif
             jframe = jframe - 1
          end do
          if(nfound.gt.0) &
               view_transform%centre(1:3) = view_transform%centre(1:3) / nfound
       endif

       ! Make the image(s)
       call plotter_make_image(nx, ny, view_transform, image)
                 
       ! Save the image as a png file. Give up and return if this fails.
       write(fname,'(a,"_",i5.5,".image.png")')trim(basename),iframe
       if(.not.write_png(fname,nx,ny,image))then
          movie_make_evolving%success = .false.
          movie_make_evolving%string="Unable to write PNG file "//trim(fname)
          deallocate(image,plot_image, combined_image, movie_centre, centre_ok)
          return
       endif

       ! Output the graph if requested
       if(output_graph)then
          select case(graph_pos)
          case(SEPARATE)
             call graph_plot(plot_image,nx,ny)
             write(fname,'(a,"_",i5.5,".plot.png")')trim(basename),iframe
             if(.not.write_png(fname,nx,ny,plot_image))then
                movie_make_evolving%success = .false.
                movie_make_evolving%string="Unable to write PNG file "// &
                     trim(fname)
                deallocate(image,plot_image, combined_image, movie_centre, &
                     centre_ok)
                return
             endif
          case(ALONGSIDE)
             ! Make a file with the image and plot side by side
             call graph_plot(plot_image,nx,ny)
             ! Make the combined image
             do j = 0, ny-1, 1
                do i = 0, nx-1, 1
                   do k = 0, 2, 1
                      combined_image(k+3*i+(6*nx*j)) = image(k+3*i+3*nx*j)
                      combined_image(k+3*(i+nx)+(6*nx*j)) = &
                           plot_image(k+3*i+3*nx*j)
                   end do
                end do
             end do
             write(fname,'(a,"_",i5.5,".both.png")')trim(basename),iframe
             if(.not.write_png(fname,2*nx,ny,combined_image))then
                movie_make_evolving%success = .false.
                movie_make_evolving%string="Unable to write PNG file "// &
                     trim(fname)
                deallocate(image,plot_image, combined_image, movie_centre, &
                     centre_ok)
                return
             endif
          case(INSET:INSET_OPAQUE)
             ! Make a file with the graph embedded in the main image
             gx = floor(min(nx,ny)*INSET_X)
             gy = floor(min(nx,ny)*INSET_Y)
             call graph_plot(plot_image,gx,gy)
             combined_image(0:3*nx*ny-1) = image(0:3*nx*ny-1)
             call inset_image(combined_image,nx,ny,plot_image,gx,gy, &
                  nx-gx-1, ny-gy-1, transparent=(graph_pos.eq.INSET))
             write(fname,'(a,"_",i5.5,".inset.png")')trim(basename),iframe
             if(.not.write_png(fname,nx,ny,combined_image))then
                movie_make_evolving%success = .false.
                movie_make_evolving%string="Unable to write PNG file "// &
                     trim(fname)
                deallocate(image,plot_image, combined_image, movie_centre, &
                     centre_ok)
                return
             endif
          end select
       endif

       ! Update the progress bar
       if(nframes.gt.1) &
            call progress_bar_update(real(iframe-firstsnap+1)/real(nframes))

       nsnap = nsnap + 1

       ! Check if the cancel button has been pressed
       if(progress_bar_cancelled())then
          movie_make_evolving%success = .false.
          movie_make_evolving%string="Cancelled by user"
          deallocate(image,plot_image, combined_image, movie_centre, &
               centre_ok)
          return
       endif

    end do
    
    deallocate(image,plot_image, combined_image, movie_centre, centre_ok)

    ! Report an error if we couldn't read any snapshots
    if(nsnap.eq.0)then
       movie_make_evolving%success = .false.
       movie_make_evolving%string="Unable to read any snapshots"
    else
       movie_make_evolving%success = .true.
    endif

    return
  end function movie_make_evolving


  subroutine inset_image(image1,width1,height1,image2,width2,height2,x,y,&
       transparent)
!
! Insert image2 into image1 at the specified coordinates. Crop if necessary.
! If transparent=.true. black pixels in image2 are not copied.
!
    implicit none
    integer :: width1, height1
    character(len=1), dimension(0:3*width1*height1-1) :: image1
    integer :: width2, height2
    character(len=1), dimension(0:3*width2*height2-1) :: image2
    integer :: x, y
    logical :: transparent
    integer :: i1, j1
    integer :: i2, j2
    integer :: k
    
    do j2 = 0, height2-1, 1
       do i2 = 0, width2-1, 1
          i1 = x + i2
          j1 = y + j2
          if(i1.ge.0.and.i1.lt.width1.and.j1.ge.0.and.j1.lt.height1)then
             do k = 0, 2, 1
                if(.not.transparent.or.image2(k+3*i2+3*j2*width2).ne.char(0))&
                     image1(k+3*i1+3*j1*width1) = image2(k+3*i2+3*j2*width2)
             end do
          endif
       end do
    end do

    return
  end subroutine inset_image


  subroutine movie_get_parameters()
!
! Retreive values from widgets
!
    implicit none
    logical :: state
    character(len=500) :: str
    integer :: ios

    if(.not.is_open)return

    ! Determine movie type
    call gui_radio_button_get_state(rotate_button, state)
    if(state)then
       movie_type = ROTATING
    else
       movie_type = EVOLVING
    endif
    ! Number of frames
    call gui_entrybox_get_text(nframes_box, str)
    read(str,*,iostat=ios)nframes
    ! Angle to rotate through
    call gui_entrybox_get_text(angle_box, str)
    read(str,*,iostat=ios)angle
    ! Range of snapshots
    call gui_entrybox_get_text(firstsnap_box, str)
    read(str,*,iostat=ios)firstsnap
    call gui_entrybox_get_text(lastsnap_box, str)
    read(str,*,iostat=ios)lastsnap
    call gui_checkbox_get_state(follow_box,recentre)
    call gui_checkbox_get_state(output_graph_box, output_graph)
    call gui_combo_box_get_index(graph_position_box, graph_pos)
    call gui_checkbox_get_state(ignore_box,ignore_unreadable)
    ! Parameters which apply to all movies
    ! Dimensions of the images
    call gui_entrybox_get_text(nx_box, str)
    read(str,*,iostat=ios)nx
    call gui_entrybox_get_text(ny_box, str)
    read(str,*,iostat=ios)ny
    ! Get path to write frames to
    call gui_entrybox_get_text(fname_box, basename)
    ! Get number of snapshots to smooth path over       
    call gui_entrybox_get_text(nsmoothmax_box, str)
    read(str,*,iostat=ios)nsmoothmax
    ! Get number of particles to use
    call gui_entrybox_get_text(npmax_box, str)
    read(str,*,iostat=ios)movie_npmax
    
    return
  end subroutine movie_get_parameters

end module movie
