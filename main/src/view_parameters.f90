module view_parameters

  use transform
  use particle_store
  use f90_gui
  use string_module
  use return_status
  use stereo
  use data_types

  implicit none
  private
  save
  
  public :: view_parameters_initialise
  public :: view_parameters_init_view
  public :: view_parameters_open
  public :: view_parameters_close
  public :: view_parameters_process_events
  public :: view_parameters_set_keys
  public :: view_parameters_get_keys
  public :: view_parameters_update

  real, parameter :: pi = 4.0*atan(1.0)

  ! Extent of the particle distribution
  real, dimension(3)         :: posmin, posmax
  real, dimension(3), public :: simcentre
  real, public               :: scalefac

  ! Transform to convert from simulation coordinates to view coordinates
  type (transform_type), public :: view_transform

  ! Default values
  real, parameter    :: fov_max_default  = 1.0
  real, parameter    :: nearclip_default = -0.75
  real, parameter    :: farclip_default  = 0.75
  real, parameter    :: z_offset_default = 0.1
  integer, parameter :: npmax_display_default = 10000000

  ! Perspective
  logical, public :: perspective_projection = .true.

  ! Maximum field of view in x or y
  real :: fov_max = fov_max_default

  ! Field of view to be used
  real, public :: fov_x, fov_y

  ! Size of a pixel in the plane of the centre of rotation
  real, public :: pixelsize_x, pixelsize_y

  ! Near and far clipping planes in the transformed coordinate system
  real, public :: nearclip = nearclip_default, farclip = farclip_default,&
       z_offset = z_offset_default

  ! Maximum particles to display
  integer, public :: npmax_display = npmax_display_default

  ! Fraction of particles which are being displayed
  real, public :: fdisplay

  ! Scale factor change that will trigger auto-resampling
  real, public :: ar_zoomfac = 2.0

  ! Window to allow adjustment of parameters
  type(gui_window) :: window

  type(gui_entrybox) :: nearclip_entry
  type(gui_entrybox) :: farclip_entry
  type(gui_entrybox) :: zoffset_entry
  type(gui_entrybox) :: npmax_entry
  type(gui_entrybox) :: fov_entry
  type(gui_button)   :: resample_button
  type(gui_button)   :: default_button
  type(gui_entrybox) :: autosample_entry

  logical :: window_open = .false.

contains

  subroutine view_parameters_initialise()
!
! Find the extent of the particle distribution and set the initial
! view transform. To be called when a new simulation is loaded.
!
    implicit none
    integer :: nspecies
    integer(kind=index_kind), dimension(maxspecies) :: np
    real(pos_kind), pointer, dimension(:,:) :: pos
    integer :: i

    if(.not.particle_store_loaded(pdata))return

    call particle_store_contents(pdata, get_np=np, get_nspecies=nspecies)
    
    ! Find range of coordinates
    posmin =  1.0e36
    posmax = -1.0e36
    do i = 1, nspecies, 1
       if(np(i).gt.0)then
          call particle_store_species(pdata, i, get_pos=pos)
          posmin(1:3) = min(posmin(1:3), minval(pos(1:3,:),2))
          posmax(1:3) = max(posmax(1:3), maxval(pos(1:3,:),2))
       endif
    end do

    ! Set centre to half way between the most extreme positions
    simcentre(1:3) = (posmax+posmin)*0.5

    ! Choose scale such that particles lie in range -0.5 to +0.5
    scalefac = 0.25 / maxval(posmax(1:3)-simcentre(1:3))

    ! Set initial view transform
    call transform_initialise(view_transform)
    call transform_modify(view_transform, centre=simcentre, &
         set_scale=scalefac)

    return
  end subroutine view_parameters_initialise


  subroutine view_parameters_init_view(width, height)
!
! Prepare view parameters for an image of the specified size.
! Will be called from plotter_make_image whenever an image is to
! be generated.
!
! - Determine the field of view to use based on the
!   dimensions of the graphics window
!
! - Determine what fraction of the particles are to be displayed
!   This can be used to scale the density so that the image is
!   independent of the sampling rate, apart from being noisier at
!   low sampling rates.
!
    implicit none
    integer, intent(in) :: width, height

    ! If the image is wider than it is high, fov_max is the horizontal field
    ! of view. Otherwise its the vertical field of view.
    if(width.gt.height)then
       fov_x = fov_max
       fov_y = fov_x * (real(height)/real(width))
    else
       fov_y = fov_max
       fov_x = fov_y * (real(width)/real(height))
    endif

    pixelsize_x = fov_x / real(width)
    pixelsize_y = fov_y / real(height)

    call particle_store_contents(psample,get_fsample=fdisplay)

    return
  end subroutine view_parameters_init_view


  subroutine view_parameters_open(mainwin)
!
! Open the settings window
!
    implicit none
    type(gui_window) :: mainwin
    type(gui_box)    :: hbox, vbox, outer_box
    type(gui_label)  :: label

    if(window_open)then
       return
    else
       window_open = .true.
    end if

    call gui_create_window(window,"View parameters",parent=mainwin,&
         resize=.false.)
    call gui_packing_mode(expand=.false., fill=.false., spacing=3, &
         position=gui_start)

    call gui_create_box(hbox, window, gui_horizontal)
    call gui_create_box(outer_box, hbox, gui_vertical)
    call gui_create_box(vbox, outer_box, gui_vertical, frame=.true.,&
         label="View coordinate system")

    ! Maximum field of view
    call gui_create_box(hbox, vbox, gui_horizontal)
    call gui_packing_mode(position=gui_start)
    call gui_create_label(label, hbox, "Maximum field of view (degrees): ")
    call gui_packing_mode(position=gui_end)
    call gui_create_entrybox(fov_entry, hbox, 8) 

    ! Clipping planes
    call gui_packing_mode(position=gui_start)
    call gui_create_box(hbox, vbox, gui_horizontal)
    call gui_create_label(label, hbox, "Near clipping plane: ")
    call gui_packing_mode(position=gui_end)
    call gui_create_entrybox(nearclip_entry, hbox, 8)
    call gui_packing_mode(position=gui_start)
    call gui_create_box(hbox, vbox, gui_horizontal)
    call gui_create_label(label, hbox, "Far clipping plane: ")
    call gui_packing_mode(position=gui_end)
    call gui_create_entrybox(farclip_entry, hbox, 8) 

    ! z-offset
    call gui_packing_mode(position=gui_start)
    call gui_create_box(hbox, vbox, gui_horizontal)
    call gui_create_label(label, hbox, &
         "z coord. at which to display selected point: ")
    call gui_packing_mode(position=gui_end)
    call gui_create_entrybox(zoffset_entry, hbox, 8) 

    ! Button to restore defaults
    call gui_packing_mode(position=gui_start)
    call gui_create_box(hbox, vbox, gui_horizontal)
    call gui_packing_mode(position=gui_end)
    call gui_create_button(default_button, hbox, " Restore default view ")

    ! Maximum number of particles to show
    call gui_create_box(vbox, outer_box, gui_vertical, frame=.true.,&
         label="Particles to display")
    call gui_packing_mode(position=gui_start)
    call gui_create_box(hbox, vbox, gui_horizontal)
    call gui_create_label(label, hbox, &
         "Maximum particles to display: ")
    call gui_packing_mode(position=gui_end)
    call gui_create_entrybox(npmax_entry, hbox, 10) 

    ! Auto resampling scale factor
    call gui_packing_mode(position=gui_start)
    call gui_create_box(hbox, vbox, gui_horizontal)
    call gui_create_label(label, hbox, &
         "Scale change to trigger auto-resample: ")
    call gui_packing_mode(position=gui_end)
    call gui_create_entrybox(autosample_entry, hbox, 10) 

    call gui_packing_mode(position=gui_start)
    call gui_create_box(hbox, vbox, gui_horizontal)
    call gui_packing_mode(position=gui_end)
    call gui_create_button(resample_button, hbox, " Resample particles now ")

    call view_parameters_update()

    ! Display the window
    call gui_show_window(window)

    if(stereo_enabled.and.is_fullscreen)then
       call gui_window_move(window,0.1,0.1)
    endif

    call gui_packing_mode(expand=.false., fill=.false., spacing=3, &
         position=gui_start)

    return
  end subroutine view_parameters_open


  subroutine view_parameters_update()
!
! Update the entry boxes in the settings window
!    
    implicit none
    character(len=8), parameter :: rfmt = "(1f8.3)", ifmt="(1i10)"
    real :: fov_degrees

    if(.not.window_open)return

    call gui_entrybox_set_text(nearclip_entry, trim(string(nearclip,fmt=rfmt)))
    call gui_entrybox_set_text(farclip_entry,  trim(string(farclip,fmt=rfmt)))
    call gui_entrybox_set_text(zoffset_entry,  trim(string(z_offset,fmt=rfmt)))
    call gui_entrybox_set_text(npmax_entry, &
         trim(string(npmax_display,fmt=ifmt)))
    fov_degrees = (2*atan(fov_max/2.0)) * (180.0/pi)
    call gui_entrybox_set_text(fov_entry,      trim(string(fov_degrees,fmt=rfmt)))
    call gui_entrybox_set_text(autosample_entry, &
         trim(string(ar_zoomfac,fmt=rfmt)))

    return
  end subroutine view_parameters_update


  subroutine view_parameters_apply()
!
! Get values from the entry boxes
!    
    implicit none
    integer            :: ios
    real               :: rtmp
    integer            :: itmp

    call gui_entrybox_get_value(nearclip_entry, rtmp, ios)
    if(ios.eq.0)then
       nearclip = min(1.0,max(-0.99,rtmp))
    endif

    call gui_entrybox_get_value(farclip_entry, rtmp, ios)
    if(ios.eq.0)then
       farclip = max(rtmp,nearclip)
    endif

    call gui_entrybox_get_value(zoffset_entry, rtmp, ios)
    if(ios.eq.0)z_offset = min(max(rtmp,-1.0),1.0)

    call gui_entrybox_get_value(npmax_entry, itmp, ios)
    if(ios.eq.0)then
       npmax_display = max(1000,itmp)
    endif

    call gui_entrybox_get_value(fov_entry, rtmp, ios)
    if(ios.eq.0)then
       ! Only allow range 1-120 degrees
       rtmp = max(min(rtmp, 120.0), 1.0)
       ! Convert to radians
       rtmp = rtmp / 180.0 * pi
       ! Compute linear FoV
       fov_max = 2.0*tan(rtmp/2.0)
    endif

    call gui_entrybox_get_value(autosample_entry, rtmp, ios)
    if(ios.eq.0)then
       if(rtmp.gt.1)ar_zoomfac=max(0.01,rtmp)
    endif

    call view_parameters_update()

    return
  end subroutine view_parameters_apply


  subroutine view_parameters_process_events(need_resample,need_redraw)
!
! Deal with events in the view parameters window
!
    implicit none
    logical           :: need_resample,need_redraw

    need_redraw   = .false.
    need_resample = .false.

    if(.not.window_open)return

    ! Close the window if necessary
    if(gui_window_closed(window))then
       call gui_destroy_window(window)
       window_open = .false.
       return
    endif

    ! Update if values are changed
    if(gui_entrybox_changed(fov_entry).or. &
         gui_entrybox_changed(nearclip_entry).or. &
         gui_entrybox_changed(farclip_entry).or. &
         gui_entrybox_changed(zoffset_entry).or. &
         gui_entrybox_changed(npmax_entry).or. &
         gui_entrybox_changed(fov_entry).or. &
         gui_entrybox_changed(autosample_entry))then
       need_redraw=.true.
       call view_parameters_apply()
    endif

    ! Restore defaults
    if(gui_button_clicked(default_button))then
       fov_max = fov_max_default
       nearclip = nearclip_default
       farclip = farclip_default
       z_offset = z_offset_default
       need_redraw=.true.
       call view_parameters_update()
    endif

    ! Resample particles
    if(gui_button_clicked(resample_button))then
       if(particle_store_loaded(pdata))then
          call view_parameters_apply()
          need_redraw   = .true.
          need_resample = .true.
       endif
    endif

    return
  end subroutine view_parameters_process_events


  subroutine view_parameters_close()
!
! Close the window
!
    implicit none
    
    if(window_open)then
       call gui_destroy_window(window)
       window_open = .false.
       return
    endif

    return
  end subroutine view_parameters_close

  
  subroutine view_parameters_set_keys()

    use key_file
    implicit none

    call set_key("View Parameters", "Max. Field of View", fov_max)
    call set_key("View Parameters", "Near Clipping Plane", nearclip)
    call set_key("View Parameters", "Far Clipping Plane", farclip)
    call set_key("View Parameters", "Particles to Display", npmax_display)
    call set_key("View Parameters", "Auto Resample Factor", ar_zoomfac)
    call set_key("View Parameters", "Z Offset",             z_offset)
    call set_key("View Parameters", "Perspective", perspective_projection)

    return
  end subroutine view_parameters_set_keys


  subroutine view_parameters_get_keys()

    use key_file
    implicit none

    call get_key("View Parameters", "Max. Field of View", fov_max)
    call get_key("View Parameters", "Near Clipping Plane", nearclip)
    call get_key("View Parameters", "Far Clipping Plane", farclip)
    call get_key("View Parameters", "Particles to Display", npmax_display)
    call get_key("View Parameters", "Auto Resample Factor", ar_zoomfac)
    call get_key("View Parameters", "Z Offset",             z_offset)
    call get_key("View Parameters", "Perspective", perspective_projection)

    return
  end subroutine view_parameters_get_keys


end module view_parameters
