module transform
!
! Module to create and store coordinate transforms intended to convert
! simulation coordinates to display coordinates
!
  use data_types

  implicit none
  private
  save

  ! Callable routines in this module
  public :: transform_initialise
  public :: transform_modify  
  public :: transform_inverse
  public :: transform_set_keys
  public :: transform_get_keys

  public :: transform_type
  type transform_type
     real(kind=real8byte)                 :: scale
     real(kind=real8byte), dimension(3)   :: centre
     real(kind=real8byte), dimension(3,3) :: matrix
     integer                              :: axis_aligned
  end type transform_type

  ! Allow use of .eq. with transforms - trans1.eq.trans2 is
  ! true if all the components are the same.
  interface operator(.eq.)
     module procedure transforms_equal
  end interface
  public :: operator(.eq.)

  ! Allow use of .ne. with transforms
  interface operator(.ne.)
     module procedure transforms_not_equal
  end interface
  public :: operator(.ne.)

contains

  subroutine transform_initialise(trans)
!
! Initialise a transform with default values
!
    implicit none
    type (transform_type) :: trans
    integer :: i

    trans%scale  = 1.0_real8byte
    trans%centre = (/ 0.0_real8byte,  0.0_real8byte,  0.0_real8byte /)
    trans%matrix = 0.0_real8byte
    do i = 1, 3, 1
       trans%matrix(i,i) = 1.0_real8byte
    end do
    trans%axis_aligned = 0

    return
  end subroutine transform_initialise

  subroutine transform_modify(trans, rotation, scale, centre, translation, &
       set_scale, reset_rotation)
!
! Apply a rotation, scale or translation to a transform
!
! The point centre(1:3) will be the centre of rotation.
! Input values are default reals, although the transform is
! maintained in double precision.
!
! All parameters are changes relative to the current value, apart
! from 'centre', which sets an absolute value for the centre of 
! rotation.
!
    implicit none
    type (transform_type) :: trans
    real(kind=pos_kind), dimension(3), optional :: rotation, centre, translation
    real(kind=pos_kind), optional :: scale
    real(kind=int8byte) :: sina, cosa
    real(kind=int8byte), dimension(3,3) :: rot
    real(kind=pos_kind), optional :: set_scale
    logical, optional :: reset_rotation
    integer :: i

    ! Scaling
    if(present(scale))then
       trans%scale = trans%scale * scale
    endif
    if(present(set_scale))then
       trans%scale = set_scale
    endif

    ! Translation (translations are specified in the original coordinate
    ! system)
    if(present(translation))then
       trans%centre = trans%centre + translation
    endif

    ! Setting the centre directly in the original coordinate system
    if(present(centre))then
       trans%centre = centre
    endif

    ! Reset rotation matrix
    if(present(reset_rotation))then
       if(reset_rotation)then
          trans%matrix = 0.0
          do i = 1, 3, 1
             trans%matrix(i,i) = 1.0_real8byte
          end do
       endif
    endif

    ! Rotation
    ! (rotations are applied in order - about x, y then z)
    if(present(rotation))then

       trans%axis_aligned = 0

       ! Rotation about x
       sina = sin(rotation(1))
       cosa = cos(rotation(1))
       rot(1,1:3) = (/ 1.0_real8byte, 0.0_real8byte, 0.0_real8byte /)
       rot(2,1:3) = (/ 0.0_real8byte, cosa         , sina          /)
       rot(3,1:3) = (/ 0.0_real8byte, -sina        , cosa          /)
       trans%matrix = matmul(rot,trans%matrix)

       ! Rotation about y
       sina = sin(rotation(2))
       cosa = cos(rotation(2))
       rot(1,1:3) = (/ cosa         , 0.0_real8byte, -sina         /)
       rot(2,1:3) = (/ 0.0_real8byte, 1.0_real8byte, 0.0_real8byte /)
       rot(3,1:3) = (/ sina         , 0.0_real8byte, cosa          /)
       trans%matrix = matmul(rot,trans%matrix)

       ! Rotation about z
       sina = sin(rotation(3))
       cosa = cos(rotation(3))
       rot(1,1:3) = (/ cosa         , sina         , 0.0_real8byte /)
       rot(2,1:3) = (/ -sina        , cosa         , 0.0_real8byte /)
       rot(3,1:3) = (/ 0.0_real8byte, 0.0_real8byte, 1.0_real8byte /)
       trans%matrix = matmul(rot,trans%matrix)

    endif

    return
  end subroutine transform_modify



  logical function transforms_equal(trans1, trans2)
!
! Return true if the two transforms are the same
!
    implicit none
    type(transform_type), intent(in) :: trans1, trans2
    
    if(trans1%scale.ne.trans2%scale.or. &
         any(trans1%centre.ne.trans2%centre).or. &
         any(trans1%matrix.ne.trans2%matrix))then
       transforms_equal = .false.
    else
       transforms_equal = .true.
    endif

    return
  end function transforms_equal


  logical function transforms_not_equal(trans1, trans2)
!
! Return true if the two transforms are not the same
!
    implicit none
    type(transform_type), intent(in) :: trans1, trans2
    
    if(trans1%scale.ne.trans2%scale.or. &
         any(trans1%centre.ne.trans2%centre).or. &
         any(trans1%matrix.ne.trans2%matrix))then
       transforms_not_equal = .true.
    else
       transforms_not_equal = .false.
    endif

    return
  end function transforms_not_equal


  type (transform_type) function transform_inverse(trans) result (res)
!
! Return a transform with the inverse rotation matrix and scale factor
! of the input transform. This is useful for mapping directions on the
! screen into directions in the simulation coordinate system.
!
    implicit none
    type (transform_type) :: trans
    integer               :: i, j
    
    res%scale  = 1.0 / trans%scale
    res%centre = real((/ 0.0, 0.0, 0.0 /), kind=real8byte)

    do i = 1, 3, 1
       do j = 1, 3, 1
          res%matrix(i,j) = trans%matrix(j,i)
       end do
    end do

    return
  end function transform_inverse


  subroutine transform_set_keys(trans)

    use key_file
    implicit none
    type (transform_type) :: trans
    real, dimension(9) :: matrix

    matrix = reshape(trans%matrix, (/9/))
    call set_key("Transform", "Scale",  real(trans%scale))
    call set_key("Transform", "Centre", real(trans%centre))
    call set_key("Transform", "Matrix", matrix)
    call set_key("Transform", "Axis Aligned", trans%axis_aligned)

    return
  end subroutine transform_set_keys


  subroutine transform_get_keys(trans)

    use key_file
    implicit none
    type (transform_type) :: trans
    real, dimension(9) :: matrix
    real, dimension(3) :: centre
    real               :: scale

    call get_key("Transform", "Scale",  scale)
    call get_key("Transform", "Centre", centre)
    call get_key("Transform", "Matrix", matrix)
    call get_key("Transform", "Axis Aligned", trans%axis_aligned)

    trans%scale  = scale
    trans%centre = centre
    trans%matrix = reshape(matrix, (/3,3/))

    return
  end subroutine transform_get_keys


end module transform
