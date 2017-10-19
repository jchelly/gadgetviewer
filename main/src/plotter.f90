module plotter
!
! This module provides routines for generating images of the particle
! distribution
!
  use transform
  use f90_gui
  use colour_table
  use view_parameters
  use dotplot
  use density2d
  use property_plot
  use fancyplot
  use smoothed_property_plot
  use overlay
  use stereo
  use key_file
  use particle_store, only : maxspecies

  implicit none
  private
  save

  public :: plotter_init
  public :: plotter_make_image
  public :: plotter_set_plot_type
  public :: plotter_get_plot_type
  public :: plotter_settings_open
  public :: plotter_settings_close
  public :: plotter_process_events
  public :: plotter_update_settings
  public :: plotter_set_keys
  public :: plotter_get_keys

  ! Array sizes
  integer, parameter :: type_name_maxlen = 30

  ! Currently selected plot type
  integer :: iplotter

  ! Particle types to show
  logical, dimension(maxspecies), public :: show_species = .true.

  ! Names of all the plot types
  integer, parameter, public :: nplottypes = 5
  character(len=type_name_maxlen), dimension(nplottypes), public :: &
       typename = (/ &
       "Dotplot                      ", &
       "Colour by projected density  ", &
       "Colour by property           ", &
       "Smoothed projected density   ", &
       "Smoothed property            "  &
       /)

  ! Handle for the settings window
  type(gui_window) :: settings_window
  logical :: settings_open = .false.

contains

  
  subroutine plotter_set_plot_type(i)
!
! This sets the plot type
!
    implicit none
    integer :: i

    call plotter_settings_close()

    if(i.lt.1.or.i.gt.nplottypes)stop'Plot type index out of range!'

    iplotter = i

    return
  end subroutine plotter_set_plot_type


  integer function plotter_get_plot_type() result(res)

    implicit none

    res = iplotter

    return
  end function plotter_get_plot_type

  subroutine plotter_init(new_snapshot)
!
! Set default plotter configuration(s)
!
    implicit none
    logical :: new_snapshot

    call dotplot_init(new_snapshot)
    call density2d_init(new_snapshot)
    call property_plot_init(new_snapshot)
    call fancyplot_init(new_snapshot)
    call smoothed_property_plot_init(new_snapshot)

    return
  end subroutine plotter_init


  subroutine plotter_make_image(width, height, trans, image)
!
! Make an image using the selected plot type. This just calls a plot
! routine from another module. It also combines pairs of images to make
! stereo images.
!
    implicit none
    integer :: width, height
    character, dimension(0:3*width*height-1) :: image
    character, dimension(0:3*width*height-1) :: buf
    type(transform_type)                     :: trans
    integer                                  :: nx, ny
    integer                                  :: nimage
    integer                                  :: i, j, offset

    ! Set width and height of image to make
    if(stereo_enabled)then
       ! Side by side stereo
       nx = width/2
       ny = height
       nimage = 2
    else if(anaglyph_enabled)then
       ! Anaglyph stereo
       nx = width
       ny = height
       nimage = 2
    else
       ! Standard mono view
       nx = width
       ny = height
       nimage = 1
    endif
    call view_parameters_init_view(nx,ny)

    do i = 0, nimage-1, 1
       ! Stereo setup
       if(stereo_enabled.or.anaglyph_enabled)then
          if(i.eq.0)then
             call stereo_left()
          else
             call stereo_right()
          endif
       else
          call stereo_off()
       endif
       ! Make the image
       select case(iplotter)
       case(1)
          call dotplot_make_image(      nx, ny, trans, buf, show_species)
       case(2)
          call density2d_make_image(    nx, ny, trans, buf, show_species)
       case(3)
          call property_plot_make_image(nx, ny, trans, buf, show_species)
       case(4)
          call fancyplot_make_image(    nx, ny, trans, buf, show_species)
       case(5)
          call smoothed_property_plot_make_image(nx, ny, trans, buf, show_species)
       case default
          stop'Invalid plot type index in plotter_make_image()'
       end select
       ! Add an 'overlay' to the image with any annotations we want
       call overlay_add_to_image(nx, ny, trans, buf, show_species)
       ! Put the image(s) in the output array
       if(stereo_enabled)then
          ! Copy this image into one half of the output array
          offset = 3*nx*i
          do j = 0, height-1, 1
             image(j*3*width+offset:j*3*width+3*nx+offset-1) = &
                  buf(j*3*nx+offset:j*3*nx+3*nx+offset-1)
          end do
       else if(anaglyph_enabled)then
          if(i.eq.0)then
             ! Red channel
             image(0:3*width*height-1:3) = buf(0:3*width*height-1:3)
          else
             ! Green and blue channels
             image(1:3*width*height-1:3) = buf(1:3*width*height-1:3)
             image(2:3*width*height-1:3) = buf(2:3*width*height-1:3)
          endif
       else
          ! Mono view - just copy the whole image
          image(0:3*width*height-1) = buf(0:3*width*height-1)
       endif
       ! Next image
    end do

    return
  end subroutine plotter_make_image


  subroutine plotter_settings_open(mainwin,last_type_selected)
!
! Open the configuration window for the selected plotter
!
    implicit none
    type (gui_window) :: mainwin
    integer :: last_type_selected

    ! Close the old window
    if(settings_open)return

    select case(iplotter)
    case(1)
       call dotplot_settings_open(mainwin,settings_window)
    case(2)
       call density2d_settings_open(mainwin,settings_window,last_type_selected)
    case(3)
       call property_plot_settings_open(mainwin,settings_window,&
            last_type_selected)
    case(4)
       call fancyplot_settings_open(mainwin,settings_window,last_type_selected)
    case(5)
       call smoothed_property_plot_settings_open(mainwin,settings_window,last_type_selected)
    case default
       stop'Invalid plot type index in plotter_make_image()'
    end select

    ! Move the settings window to the LHS of the main window -
    ! this is less confusing in side by side stereo mode
    if(stereo_enabled.and.is_fullscreen)then
       call gui_window_move(settings_window,0.1,0.1)
    endif

    settings_open = .true.

    return
  end subroutine plotter_settings_open


  subroutine plotter_settings_close()
!
! Close the configuration window for the selected plotter
!
    implicit none

    if(settings_open)then
       call gui_destroy_window(settings_window)
       settings_open = .false.
    endif

    return
  end subroutine plotter_settings_close


  logical function plotter_process_events()
!
! Process events in the plot settings window
!
    implicit none
    logical :: res

    plotter_process_events = .false.

    ! Do nothing if the window isn't open
    if(.not.settings_open)return

    ! Close the window if necessary
    if(gui_window_closed(settings_window))then
       settings_open = .false.
       call gui_destroy_window(settings_window)
       return
    endif

    ! Go to plotter specific event handler
    select case(iplotter)
    case(1)
       res = dotplot_process_events()
    case(2)
       res = density2d_process_events()
    case(3)
       res = property_plot_process_events()
    case(4)
       res = fancyplot_process_events()
    case(5)
       res = smoothed_property_plot_process_events()
    case default
       stop'Invalid plot type index in plotter_make_image()'
    end select

    plotter_process_events = res

    return
  end function plotter_process_events


  subroutine plotter_update_settings()
!
! Update the configuration window - for example, if the colour tables
! have been changed we'll need to update the list shown in the settings
! window.
!
    implicit none

    ! Ignore if window not open
    if(.not.settings_open)return

    select case(iplotter)
    case(1)
       call dotplot_update_settings()
    case(2)
       call density2d_update_settings()
    case(3)
       call property_plot_update_settings()
    case(4)
       call fancyplot_update_settings()
    case(5)
       call smoothed_property_plot_update_settings()
    case default
       stop'Invalid plot type index in plotter_update_settings()'
    end select

    return
  end subroutine plotter_update_settings


  subroutine plotter_set_keys()
!
! Set keys for plot settings
!
    implicit none

    call set_key("Plotter","Plot Type", iplotter)
    call set_key("Plotter","Show Particle Types", show_species)
    call dotplot_set_keys()
    call density2d_set_keys()
    call property_plot_set_keys()
    call fancyplot_set_keys()
    call smoothed_property_plot_set_keys()

    return
  end subroutine plotter_set_keys


  subroutine plotter_get_keys()
!
! Get keys for plot settings
!
    implicit none

    call get_key("Plotter","Plot Type", iplotter)
    call get_key("Plotter","Show Particle Types", show_species)
    call dotplot_get_keys()
    call density2d_get_keys()
    call property_plot_get_keys()
    call fancyplot_get_keys()
    call smoothed_property_plot_get_keys()

    return
  end subroutine plotter_get_keys

end module plotter
