module stereo
!
! Module to store parameters related to stereo 3d display
!
  implicit none
  private
  save

  public :: stereo_off
  public :: stereo_right
  public :: stereo_left
  public :: stereo_set_separation
  public :: stereo_get_keys
  public :: stereo_set_keys

  ! Whether stereo mode is active
  logical, public :: stereo_enabled, anaglyph_enabled

  ! Shift to apply to view points in the transformed coordinate system
  ! when using stereo mode
  real :: stereo_offset

  ! Plot routines use this as the amount to shift by
  real, public :: stereo_shift

  ! Range of values for the slider
  real, parameter, public  :: stereo_min_sep  = 0.005
  real, parameter, public  :: stereo_max_sep  = 0.1
  real, parameter, public  :: stereo_sep_step = 0.005
  real, parameter, public  :: stereo_default_sep = 0.05  

  ! Whether the window is full screen
  logical, public :: is_fullscreen

contains
!
! Subroutines for switching stereo modes
!
  subroutine stereo_left()

    implicit none

    stereo_shift = stereo_offset

    return
  end subroutine stereo_left

  subroutine stereo_right()

    implicit none

    stereo_shift = -stereo_offset

    return
  end subroutine stereo_right

  subroutine stereo_off()

    implicit none

    stereo_shift = 0.0

    return
  end subroutine stereo_off

!
! Routine to set the separation
!
  subroutine stereo_set_separation(sep)

    implicit none
    real :: sep

    stereo_offset = sep

    return
  end subroutine stereo_set_separation

  
  subroutine stereo_set_keys()
    
    use key_file
    implicit none

    call set_key("Stereo","Stereo Enabled",   stereo_enabled)
    call set_key("Stereo","Anaglyph Enabled", anaglyph_enabled)
    call set_key("Stereo","Stereo Offset",    stereo_offset)

    return
  end subroutine stereo_set_keys


  subroutine stereo_get_keys()
    
    use key_file
    implicit none

    call get_key("Stereo","Stereo Enabled",   stereo_enabled)
    call get_key("Stereo","Anaglyph Enabled", anaglyph_enabled)
    call get_key("Stereo","Stereo Offset",    stereo_offset)

    return
  end subroutine stereo_get_keys

end module stereo
