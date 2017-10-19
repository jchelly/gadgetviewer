module movie_parameters

  implicit none
  public

  ! Movie settings
  integer :: nframes             = 200
  real    :: angle               = 360.0   
  integer :: firstsnap           = -1
  integer :: lastsnap            = -1
  logical :: recentre            = .true.
  integer :: nx                  = 640
  integer :: ny                  = 480
  character(len=500) :: basename = "./frame"
  integer :: nsmoothmax          = 10
  integer :: movie_npmax         = 1000000
  logical :: output_graph        = .false.
  logical :: ignore_unreadable   = .true.

  integer :: movie_type  ! 1=rotating, 2=evolving
  integer, parameter :: ROTATING = 1, EVOLVING = 2
  
  ! Where to put the plot if it is to be output
  integer, parameter :: SEPARATE = 1, ALONGSIDE = 2
  integer, parameter :: INSET = 3, INSET_OPAQUE = 4
  integer            :: graph_pos = SEPARATE

contains

  subroutine movie_set_keys()

    use key_file
    implicit none

    call set_key("Movie", "Movie Type",       movie_type)
    call set_key("Movie", "Number of Frames", nframes)
    call set_key("Movie", "Rotation Angle",   angle)
    call set_key("Movie", "First Snapshot",   firstsnap)
    call set_key("Movie", "Last Snapshot",    lastsnap)
    call set_key("Movie", "Recentre",         recentre)
    call set_key("Movie", "Width",            nx)
    call set_key("Movie", "Height",           ny)
    call set_key("Movie", "Base Name",        basename)
    call set_key("Movie", "Smooth Path",      nsmoothmax)
    call set_key("Movie", "Max. Particles",   movie_npmax)
    call set_key("Movie", "Output Graph",     output_graph)
    call set_key("Movie", "Ignore Unreadable Snapshots", ignore_unreadable)

    return
  end subroutine movie_set_keys


  subroutine movie_get_keys()

    use key_file
    implicit none

    call get_key("Movie", "Movie Type",       movie_type)
    call get_key("Movie", "Number of Frames", nframes)
    call get_key("Movie", "Rotation Angle",   angle)
    call get_key("Movie", "First Snapshot",   firstsnap)
    call get_key("Movie", "Last Snapshot",    lastsnap)
    call get_key("Movie", "Recentre",         recentre)
    call get_key("Movie", "Width",            nx)
    call get_key("Movie", "Height",           ny)
    call get_key("Movie", "Base Name",        basename)
    call get_key("Movie", "Smooth Path",      nsmoothmax)
    call get_key("Movie", "Max. Particles",   movie_npmax)
    call get_key("Movie", "Output Graph",     output_graph)
    call get_key("Movie", "Ignore Unreadable Snapshots", ignore_unreadable)

    return
  end subroutine movie_get_keys

end module movie_parameters
