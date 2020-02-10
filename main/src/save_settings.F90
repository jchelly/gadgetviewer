module settings
!
! Module to handle saving and restoring Gadgetviewer plot settings
!
  use key_file
  use return_status
  use plotter
  use view_parameters
  use graph
  use overlay
  use stereo
  use transform
  use sampling
  use selection
  use movie_parameters
  use threads

  implicit none
  private
  save

  public :: save_settings
  public :: load_settings

contains

  type (result_type) function save_settings(fname) result(res)
!
! Save current settings to a file
!
    implicit none
    character(len=*), intent(in) :: fname
    integer :: ios

    ! Make a new key file
    call new_key_file()

    ! Set keys from current settings
    call threads_set_keys()
    call view_parameters_set_keys()
    call transform_set_keys(view_transform)
    call sample_set_keys()
    call graph_set_keys()
    call overlay_set_keys()
    call stereo_set_keys()
    call selection_set_keys()
    call movie_set_keys()
    call plotter_set_keys()

    ! Write out the key file
    call write_key_file(fname, ios)
    if(ios.ne.0)then
       res%success = .false.
       res%string = "Unable to write file: "//trim(fname)
       call close_key_file()
       return
    endif
    call close_key_file()

    res%success = .true.
    return
  end function save_settings


  type (result_type) function load_settings(fname) result(res)
!
! Load settings from a file
!
    implicit none
    character(len=*), intent(in) :: fname
    integer :: ios

    ! Read in the key file
    call read_key_file(fname, ios)
    if(ios.ne.0)then
       res%success = .false.
       res%string = "Unable to read file: "//trim(fname)
       call close_key_file()
       return
    endif

    ! Update settings from file
    call threads_get_keys()
    call view_parameters_get_keys()
    call transform_get_keys(view_transform)
    call sample_get_keys()
    call graph_get_keys()
    call overlay_get_keys()
    call stereo_get_keys()
    call selection_get_keys()
    call movie_get_keys()
    call plotter_get_keys()

    ! Close the file
    call close_key_file()
    res%success = .true.
    return
  end function load_settings

end module settings
