module command_line

  use f90_util

  implicit none
  private

  public :: command_line_options
  
contains

  subroutine command_line_options(snapshot_file, config_file, batch_mode, &
       selection_file, partial, rinfo)

    use partial_read_info
    implicit none
    character(len=*)   :: snapshot_file, config_file, selection_file
    logical            :: batch_mode
    integer            :: nargs, iarg
    character(len=500) :: arg
    integer            :: nsnapfile
    logical            :: ok
    logical            :: have_config, have_movie
    logical            :: partial
    type(read_info)    :: rinfo
    integer            :: iostat
    real, dimension(4) :: params

    batch_mode = .false.
    snapshot_file = ""
    config_file   = ""
    selection_file = ""
    nsnapfile     = 0
    ok            = .true.
    have_config = .false.
    have_movie  = .false.
    partial     = .false.

    ! Partial read info
    rinfo%do_sampling    = .false.
    rinfo%do_sphere      = .false.
    rinfo%just_this_file = .false.
    rinfo%ignore_missing_mass = .false.
    rinfo%extra_dataset_names = ""

    nargs = get_nargs()
    do iarg = 1, nargs, 1
       call get_arg(iarg, arg)
       if(index(arg, "--config=").eq.1)then
          ! Config file name
          config_file = trim(arg(10:))
          if(len_trim(config_file).eq.0)ok = .false.
          have_config = .true.
       else if(index(arg, "--movie=").eq.1)then
          ! Movie file name
          config_file = trim(arg(9:))
          if(len_trim(config_file).eq.0)ok = .false.
          batch_mode = .true.
          have_movie = .true.
       else if(index(arg, "--help").eq.1)then
          ! Help flag
          ok = .false.
       else if(index(arg, "--selection=").eq.1)then
          ! Selection file name
          selection_file = trim(arg(13:))
          if(len_trim(selection_file).eq.0)ok = .false.
       else if(index(arg, "--partial").eq.1)then
          ! Read partial snap flag
          partial = .true.
       else if(index(arg, "--single-file").eq.1)then
          ! Read single file
          rinfo%just_this_file = .true.
       else if(index(arg, "--region").eq.1)then
          ! Specify region to read
          rinfo%do_sphere = .true.
          call extract_params(arg, params(1:4), iostat)
          if(iostat.ne.0)then
             call terminate("Unable to interpret region specification!")
          else
             rinfo%pos    = params(1:3)
             rinfo%radius = params(4)
          endif
       else if(index(arg, "--sample-rate").eq.1)then
          ! Specify sampling rate
          rinfo%do_sampling = .true.
          call extract_params(arg, params(1:1), iostat)
          if(iostat.ne.0)then
             call terminate("Unable to interpret sampling rate as real!")
          else
             rinfo%sample_rate = params(1)
          endif
       else if(index(arg, "--ignore-missing-mass").eq.1)then
          ! Set missing masses to 1
          rinfo%ignore_missing_mass = .true.
       else if(index(arg, "--datasets=").eq.1)then
          ! Store extra datasets string
          rinfo%extra_dataset_names = arg(12:)
       else
          ! Snapshot filename
          snapshot_file = trim(arg)
          nsnapfile = nsnapfile + 1
          if(nsnapfile.gt.1)ok = .false.
       endif
    end do

    if(len_trim(snapshot_file).eq.0)then
       if(batch_mode)then
          write(0,*)""
          write(0,*)"If the --movie flag is specified then a snapshot file"
          write(0,*)"must be specified."
          write(0,*)""
          call terminate()
       endif
       if(len_trim(selection_file).gt.0)then
          write(0,*)""
          write(0,*)"If the --selection flag is specified then a snapshot file"
          write(0,*)"must be specified."
          write(0,*)""
       endif
    endif

    if(partial)then
       if(batch_mode)then
          write(0,*)""
          write(0,*)"The --partial and --move flags must not both be specified"
          write(0,*)""
       endif
    endif

    if(have_config.and.have_movie)then
       write(0,*)""
       write(0,*)"The --movie and --config flags must not both be specified"
       write(0,*)""
       call terminate()
    endif

    ! Help text
    if(.not.ok)then
       write(0,*)""
       write(0,*)"Usage: gadgetviewer [snapshot_file] [options]"
       write(0,*)""
       write(0,*)"Where options can include any of the following:"
       write(0,*)""
       write(0,*)"       --config=<file>      : read configuration from file"
       write(0,*)"       --selection=<file>   : read saved particle selection"
       write(0,*)"       --movie=<file>       : batch mode movie making"
       write(0,*)"       --partial            : open gui to read part of a snapshot"
       write(0,*)"       --single-file        : only read this file"
       write(0,*)"       --sample-rate=s      : random sample the particles with"
       write(0,*)"                              a sampling rate S in range 0-1"
       write(0,*)"       --region=x,y,z,r     : only load particles in a cube"
       write(0,*)"                              centred on x,y,z with side 2*r"
       write(0,*)"                              (reads all files by default)"
       write(0,*)"       --datasets=...       : comma separated list of extra HDF5"
       write(0,*)"                              datasets to read"
       write(0,*)"       --ignore-missing-mass: assume mass=1 if no mass dataset"
       write(0,*)"       --help               : print this message"
       write(0,*)""
       write(0,*)"A snapshot file must be specified for batch mode movie making."
       write(0,*)"For rotating movies the snapshot file must be a file "
       write(0,*)"from the snapshot to be used in the movie. For evolving "
       write(0,*)"movies the snapshot numbers are read from the configuration"
       write(0,*)"file."
       write(0,*)""
       write(0,*)"The configuration file for the --movie option can be created"
       write(0,*)"with the 'Save movie parameters' button in the movie window."
       write(0,*)""
       call terminate()
    endif

    return

  contains

    subroutine extract_params(arg, params, iostat)

      implicit none
      character(len=*)   :: arg
      real, dimension(:) :: params
      integer            :: iostat
      integer            :: iequals
      
      ! Find equals sign
      iequals = index(arg,"=")
      if(iequals.lt.1)then
         iostat = -1
         return
      endif
      
      ! Check we have something after the "="
      if(iequals.ge.len_trim(arg))then
         iostat = -1
         return
      endif

      ! Extract float values from remainder of string
      read(arg(iequals+1:),*,iostat=iostat)params

      return
    end subroutine extract_params

  end subroutine command_line_options

end module command_Line
