module mouse_handler
!
! Process mouse events in the graphics window
!
  use f90_gui
  use view_parameters
  use transform
  use percentiles
  use stereo
  use data_types
  use info_window

  implicit none
  private
  save

  public :: mouse_handler_process_events
  public :: mouse_handler_save_settings
  public :: mouse_handler_read_conf

  ! Mouse dragging state
  logical, dimension(5)       :: bstat, dragged
  integer                     :: mx, my, dx, dy
  logical, dimension(5)       :: old_bstat
  integer                     :: old_mx, old_my

  ! Rotation speed in radians per pixel
  real(kind=pos_kind), parameter :: rot_fac = &
  real(1.0*3.141592659_real8byte/180.0, kind=pos_kind)

  ! Zoom speed
  real(kind=pos_kind), parameter :: zm_fac  = 1.05_pos_kind

  ! Translation speed
  real, parameter :: trans_fac = 1.0e-2

  ! Radius to search for particles when selecting a blob
  integer, parameter :: iradius = 10

  ! Moving mouse wheel does zoom equivalent to dragging this many pixels
  integer, parameter :: wheel_step = 5

  ! Functions that can be bound to mouse buttons
  integer, parameter, public :: nbutton   = 3
  integer, parameter, public :: nfunction = 6
  character(len=24), dimension(nfunction), public :: control_function = (/ &
       "Rotate                  ", &
       "Scale                   ", &
       "Translate (X/Y)         ", &
       "Translate (X/Z)         ", &
       "Rotate about y and scale", &
       "No action               "  &
       /)
  integer, parameter, public  :: ROTATE           = 1
  integer, parameter, public  :: SCALE            = 2
  integer, parameter, public  :: TRANSLATE_XY     = 3
  integer, parameter, public  :: TRANSLATE_XZ     = 4
  integer, parameter, public  :: ROTATE_AND_SCALE = 5
  integer, parameter, public  :: NO_ACTION        = 6

  ! Default functions
  integer, dimension(nbutton), public :: button_function = (/ &
       ROTATE,         &
       TRANSLATE_XY,   &
       SCALE           &
       /)
  logical, dimension(nbutton), public :: button_click_centre = &
       (/ .false., .true., .false. /)

  ! Name of the file with button assignments
  character(len=500) :: mouse_settings_file

contains

  logical function mouse_handler_process_events(mainwin,drawing_area, &
       width,height,show_species)
!
! Check for clicks/dragging in the draw area. Returns true if the window needs
! to be redrawn.
!
    implicit none
    type (gui_drawing_area) :: drawing_area
    type (gui_window)       :: mainwin
    logical, dimension(:)   :: show_species
    integer, intent(in)     :: width, height
    real(kind=pos_kind)     :: zf
    logical                 :: moved
    integer                 :: ibutton
    type (transform_type)   :: inv_trans
    real(kind=pos_kind), dimension(3) :: vec, trans_vec
    integer                 :: j

    moved = .false.

    ! Detect mouse dragging in the graphics window
    call gui_drawing_area_mouse_state(drawing_area,bstat,mx,my)
    dx = mx - old_mx
    dy = my - old_my
    dragged(1:5) = (bstat(1:5).and.old_bstat(1:5).and.(dx.ne.0.or.dy.ne.0)) 
    old_mx    = mx
    old_my    = my
    old_bstat = bstat

    do ibutton = 1, nbutton, 1

       if(button_click_centre(ibutton))then
          ! Button selects a point
          if(gui_drawing_area_mouse_clicked(drawing_area,ibutton))then  
             if(.not.stereo_enabled)then
                call mouse_handler_select_point(mainwin,mx,my,width,height,&
                     show_species)
             else
                ! In stereo mode need to check which side was clicked
                if(mx.le.width/2)then
                   call mouse_handler_select_point(mainwin,mx,my,width/2,&
                        height, show_species)
                else
                   call mouse_handler_select_point(mainwin,mx-width/2,my, &
                        width/2,height,show_species)
                endif
             endif
             moved = .true.
          endif
       endif

       select case(button_function(ibutton))
       case(ROTATE)
          ! Button rotates the view
          if(dragged(ibutton))then
             call transform_modify(view_transform, &
                  rotation=real((/dy*rot_fac, dx*rot_fac, 0.0_pos_kind /),&
                  kind=pos_kind))
             moved = .true.
          endif
       case(SCALE)
          ! Vertical drag zooms in/out
          if(dragged(ibutton))then
             zf = zm_fac**dy
             call transform_modify(view_transform, scale=zf)
             moved = .true.
          endif
       case(ROTATE_AND_SCALE)
          ! Vertical drag zooms in/out, horizontal drag rotates
          if(dragged(ibutton))then
             zf = zm_fac**dy
             call transform_modify(view_transform, scale=zf, &
                  rotation=real((/0.0_pos_kind, dx*rot_fac, 0.0_pos_kind /),&
                  kind=pos_kind))
             moved = .true.
          endif
       case(TRANSLATE_XY)
          if(dragged(ibutton))then
             ! Button translates in the plane of the screen
             inv_trans = transform_inverse(view_transform)
             vec = (/ -dx*trans_fac, dy*trans_fac, 0.0 /)
             do j = 1, 3, 1
                trans_vec(j) = &
                     dot_product(vec(1:3)-inv_trans%centre(1:3), &
                     inv_trans%matrix(j,1:3)) * inv_trans%scale
             end do
             call transform_modify(view_transform, translation=trans_vec)
             moved = .true.
          endif
       case(TRANSLATE_XZ)
          if(dragged(ibutton))then
             ! Button translates in the plane of the screen
             inv_trans = transform_inverse(view_transform)
             vec = (/ -dx*trans_fac, 0.0, dy*trans_fac /)
             do j = 1, 3, 1
                trans_vec(j) = &
                     dot_product(vec(1:3)-inv_trans%centre(1:3), &
                     inv_trans%matrix(j,1:3)) * inv_trans%scale
             end do
             call transform_modify(view_transform, translation=trans_vec)
             moved = .true.
          endif
       end select
    end do

    ! Mouse wheel zooms in/out
    if(gui_drawing_area_mouse_clicked(drawing_area,4))then
       zf = zm_fac**wheel_step
       call transform_modify(view_transform, scale=zf)
       moved = .true.
    endif

    if(gui_drawing_area_mouse_clicked(drawing_area,5))then
       zf = 1.0/(zm_fac**wheel_step)
       call transform_modify(view_transform, scale=zf)
       moved = .true.
    endif

    mouse_handler_process_events = moved

    return
  end function mouse_handler_process_events
  

  subroutine mouse_handler_select_point(mainwin, mx, my, width, height, &
       show_species)
    !
    ! Set the centre of rotation to the selected point by looking for
    ! nearby particles
    !
    use particle_store
    use data_types
    use transform
    use view_parameters
    use projection

    implicit none
    ! Parameters
    type (gui_window) :: mainwin
    integer, intent(in) :: mx, my, width, height
    logical, dimension(:)   :: show_species
    ! Internal
    integer :: i, j
    integer :: ntot
    integer :: nspecies
    integer(kind=index_kind), dimension(maxspecies) :: np
    real(kind=pos_kind), dimension(:,:), allocatable :: pos_tmp
    integer, dimension(2) :: ipos
    integer :: istat
    character(len=10) :: bt
    real(kind=pos_kind), dimension(:,:), pointer :: pos
    integer, dimension(:), allocatable :: idx
    real(kind=pos_kind), dimension(3) :: new_centre

    call particle_store_contents(psample,get_nspecies=nspecies,get_np=np)

    ! Count particles near the mouse pointer
    ntot = 0
    do i = 1, nspecies, 1
       if(show_species(i))then
          call particle_store_species(psample,i,get_pos=pos)
          do j = 1, np(i), 1
             ipos = project(pos(1:3,j),view_transform,width,height,fov_x,fov_y)
             if((mx-ipos(1))**2+(my-ipos(2))**2.lt.iradius**2)then
                ntot = ntot + 1
             endif
          end do
       endif
    end do

    ! Just return if we didn't find any particles
    if(ntot.eq.0)return

    ! Allocate storage for the particles
    allocate(pos_tmp(3,ntot), idx(ntot), stat=istat)
    if(istat.ne.0)then
       bt=gui_display_dialog(mainwin,"error", &
            "Unable to allocate memory for select operation")
       return
    endif

    ! Get the positions of the particles
    ntot = 0
    do i = 1, nspecies, 1
       if(show_species(i))then
          call particle_store_species(psample,i,get_pos=pos)
          do j = 1, np(i), 1
             ipos = project(pos(1:3,j),view_transform,width,height,fov_x,fov_y)
             if((mx-ipos(1))**2+(my-ipos(2))**2.lt.iradius**2)then
                ntot = ntot + 1
                pos_tmp(1:3,ntot) = pos(1:3,j)
             endif
          end do
       endif
    end do

    ! Find the median coordinates of these particles
    new_centre(1) = percentile(0.5, ntot, pos_tmp(1,1:ntot))
    new_centre(2) = percentile(0.5, ntot, pos_tmp(2,1:ntot))
    new_centre(3) = percentile(0.5, ntot, pos_tmp(3,1:ntot))

    ! Set this as the new centre
    call transform_modify(view_transform,centre=new_centre(1:3))

    ! Deallocate the temporary array
    deallocate(pos_tmp, idx)

    call info_window_update()

    return
  end subroutine mouse_handler_select_point


  subroutine mouse_handler_read_conf(dir)

    use key_file
    implicit none
    character(len=*) :: dir
    integer          :: i

    mouse_settings_file = trim(dir)//"/mouse_buttons"

    call read_key_file(mouse_settings_file)
    call get_key("Mouse", "Button functions", button_function)
    call get_key("Mouse", "Click to centre",  button_click_centre)
    call close_key_file()

    ! Sanity check
    do i = 1, nbutton, 1
       if(button_function(i).lt.1)button_function(i) = 1
       if(button_function(i).gt.6)button_function(i) = 6
    end do

    return
  end subroutine mouse_handler_read_conf

  
  subroutine mouse_handler_save_settings()

    use key_file
    implicit none
    integer          :: ios

    call read_key_file(mouse_settings_file)
    call set_key("Mouse", "Button functions", button_function)
    call set_key("Mouse", "Click to centre",  button_click_centre)
    call write_key_file(mouse_settings_file)
    call close_key_file()

    return
  end subroutine mouse_handler_save_settings


end module mouse_handler
