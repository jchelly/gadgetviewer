module projection
!
! This module defines the projection used to generate images
!
  use data_types
  use transform
  use view_parameters
  use stereo

  implicit none

contains

  function project(pos,trans,width,height,fov_x,fov_y,z)
    
    implicit none
    ! Parameters
    real(kind=pos_kind), dimension(3), intent(in) :: pos
    type (transform_type), intent(in) :: trans
    integer, dimension(2) :: project
    integer, intent(in) :: width, height
    real, intent(in) :: fov_x, fov_y
    real, optional :: z
    ! Internal
    integer :: j
    real(kind=pos_kind), dimension(3) :: pos_trans

    ! Get the coordinates of this particle in the view
    ! coordinate system
    do j = 1, 3, 1
       pos_trans(j) = &
            dot_product(pos(1:3)-trans%centre(1:3), &
            trans%matrix(j,1:3)) * trans%scale
    end do

    !  Shift particles in stereo mode
    pos_trans(1) = pos_trans(1) + stereo_shift
    pos_trans(3) = pos_trans(3) + z_offset

    ! Project onto the screen
    if(pos_trans(3).gt.nearclip.and.pos_trans(3).lt.farclip)then
       if(perspective_projection)then
          project(1) = floor((pos_trans(1)/(1.0+pos_trans(3))-stereo_shift)/ &
               fov_x*width) + width/2
          project(2) = floor((-pos_trans(2)/(1.0+pos_trans(3)))/fov_y*height)+&
               height/2
       else
          project(1) = floor((pos_trans(1)-stereo_shift)/ &
               fov_x*width) + width/2
          project(2) = floor((-pos_trans(2))/fov_y*height)+&
               height/2
       endif
    else
       project(1:2) = (/-1000,-1000/)
    endif

    ! Return z coordinate if requested
    if(present(z))z = pos_trans(3)

    return
  end function project


  function project_radius(pos,radius,trans,width,fov_x)
    
    implicit none
    ! Parameters
    real(kind=pos_kind), dimension(3), intent(in) :: pos
    real(kind=pos_kind), intent(in)               :: radius
    type (transform_type), intent(in) :: trans
    real :: project_radius
    integer, intent(in) :: width
    real, intent(in) :: fov_x
    ! Internal
    integer :: j
    real(kind=pos_kind), dimension(3) :: pos_trans
    real(kind=pos_kind) :: rad_trans

    ! Get the coordinates of this particle in the view
    ! coordinate system
    do j = 1, 3, 1
       pos_trans(j) = &
            dot_product(pos(1:3)-trans%centre(1:3), &
            trans%matrix(j,1:3)) * trans%scale
    end do
    rad_trans = radius * trans%scale

    !  Shift particles
    pos_trans(3) = pos_trans(3) + z_offset

    ! Project onto the screen
    if(perspective_projection)then
       project_radius = (rad_trans/(1.0+pos_trans(3)))/ &
            fov_x*width
    else
       project_radius = rad_trans/fov_x*width
    endif

    return
  end function project_radius


end module projection
