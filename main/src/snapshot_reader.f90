module snapshot_reader
!
! This module calls the appropriate routines to read
! a snapshot of the specified type. Its purpose is to
! hide the differences between snapshot formats from
! the calling program.
!
  use return_status
  use particle_store
  use gadget_binary_reader
  use gadget_binary_type2_reader
  use swift_reader
  use gadget_hdf5_reader
  use gadget_eagle_reader
  use dummy_reader
  use progress_bar
  use view_parameters
  use sampling
  use selection
  use summary
  use additional_data
  use group_catalogue
  use catalogue_data
  use plotter
  use info_window
  use partial_read_info
  use f90_util

  implicit none
  save
  private

  public :: snapshot_set_format
  public :: snapshot_open
  public :: snapshot_read
  public :: snapshot_open_unknown

  character(len=100) :: snapformat

  ! Records if this is the first snapshot read from a new simulation
  logical :: first_read = .true.

  ! Format names
  integer, parameter :: nformat = 6
  character(len=20), dimension(nformat) :: format_names = (/ &
       "GADGET_BINARY       ", &
       "GADGET_BINARY_TYPE2 ", &
       "GADGET_EAGLE        ", &
       "SWIFT               ", &
       "GADGET_HDF5         ", &
       "DUMMY               "  & 
       /)

  ! Information about what to read
  logical :: have_read_info
  type (read_info) :: rinfo

contains

!
! Set the format to use
!
  subroutine snapshot_set_format(str)

    implicit none
    character(len=*) :: str

    snapformat = trim(str)
    if(.not.(any(format_names.eq.str)))call terminate("Unrecognised snapshot format!")

    return
  end subroutine snapshot_set_format

!
! Record information needed to read snapshots
!
  type (result_type) function snapshot_open(fname, isnap, ri)

    implicit none
    type (result_type) :: res
    character(len=*) :: fname
    integer :: isnap
    type (read_info), optional :: ri
    type (read_info) :: ri_in
    
    if(present(ri))then
       ri_in = ri
    else
       ri_in%just_this_file = .false.
       ri_in%do_sampling    = .false.
       ri_in%do_sphere      = .false.
       ri_in%use_index      = .false.
    endif

    select case(snapformat)
    case("GADGET_BINARY")
       res = gadget_binary_open(fname,isnap,ri_in)
    case("GADGET_BINARY_TYPE2")
       res = gadget_binary_type2_open(fname,isnap,ri_in)
    case("GADGET_EAGLE")
       res = gadget_eagle_open(fname,isnap,ri_in)
    case("SWIFT")
       res = swift_open(fname,isnap,ri_in)
    case("GADGET_HDF5")
       res = gadget_hdf5_open(fname,isnap,ri_in)
    case("DUMMY")
       res = dummy_open(fname,isnap,ri_in)
    case default
       call terminate("Unrecognised snapshot format!")
    end select
     
    if(res%success)then
       have_read_info = present(ri)
       ! Store partial read info for when we read the snapshot
       rinfo = ri_in
       ! Always initially display the whole volume when we load a new simulation
       call sample_reset()
       first_read = .true.
    endif

    ! Forget about any extra files we had loaded for the previous
    ! simulation
    call additional_data_init()
    call group_catalogue_init()
    call catalogue_data_init()

    snapshot_open = res
    return
  end function snapshot_open

!
! Read a snapshot
!
  type (result_type) function snapshot_read(isnap)
    
    implicit none
    type (result_type) :: res, sample_res, additional_res
    integer :: isnap

    call progress_bar_display("Reading snapshot...")
    call summary_clear(s)
    call summary_close()

    select case(snapformat)
    case("GADGET_BINARY")
       call summary_add_line(s,"Simulation format: Gadget type 1 binary")
       res = gadget_binary_read(isnap, rinfo)
    case("GADGET_BINARY_TYPE2")
       call summary_add_line(s,"Simulation format: Gadget type 2 binary")
       res = gadget_binary_type2_read(isnap, rinfo)
    case("GADGET_EAGLE")
       call summary_add_line(s,"Simulation format: Eagle")
       res = gadget_eagle_read(isnap, rinfo)

    case("SWIFT")
       call summary_add_line(s,"Simulation format: SWIFT")
       res = swift_read(isnap, rinfo)
    case("GADGET_HDF5")
       call summary_add_line(s,"Simulation format: Gadget HDF5")
       res = gadget_hdf5_read(isnap, rinfo)
    case("DUMMY")
       call summary_add_line(s,"Simulation format: DUMMY!")
       res = dummy_read(isnap, rinfo)
    case default
       call terminate("Unrecognised snapshot format!")
    end select

    call progress_bar_close()

    if(res%success)then

       ! Store the snapshot number
       call particle_store_set_snapnum(pdata, isnap)

       ! Consistency check on particle numbers
       call particle_store_verify(pdata)

       ! Build octrees for sampling
       call particle_store_build_trees(pdata)

       ! Attempt to read any supplementary data files
       additional_res = additional_data_read()

       ! Attempt to read any group catalogues
       call group_catalogue_read_all(isnap)

       ! Attempt to read all point catalogues
       call catalogue_data_read_all(isnap)

       ! Random sample the particles
       if(first_read)then
          ! Sample from whole volume if this is a new simulation. Also
          ! discard any selected particles.
          call selection_clear_all(pdata)
          sample_res = sample_region(whole_volume=.true.)
       else
          ! Keep old settings if its just another snapshot in the same
          ! simulation
          sample_res = sample_region(keep_coords=.true.)
       endif
       if(.not.sample_res%success)then
          snapshot_read = sample_res
          call particle_store_empty(psample)
          call particle_store_empty(pdata)
          return
       endif
       
       ! Initialise plotting routines
       call plotter_init(.not.first_read)

       ! Generate summary page showing particle types and properties
       call summary_add_line(s," ")
       call summary_generate(s,pdata)

       ! Update info window
       call info_window_update()

       first_read = .false.

    else
       ! If there's no data loaded (e.g. read failed) discard
       ! any sampled particles.
       if(.not.particle_store_loaded(pdata))then
          call particle_store_empty(psample)
       endif
    endif

    snapshot_read = res
    return
  end function snapshot_read

!
! Open a file of unknown format by trying each read routine in turn
! until one of them works or they've all failed
!
  type(result_type) function snapshot_open_unknown(fname, isnap, ri)

    implicit none
    type(result_type) :: res
    character(len=*)  :: fname
    integer           :: isnap
    logical           :: fexist
    integer           :: i, ios
    character(len=500), dimension(nformat) :: errstr
    type (read_info), optional :: ri

    ! Check if the file exists
    if(fname.ne."DUMMY_SNAPSHOT")then
       inquire(file=fname,exist=fexist,iostat=ios)
       if(ios.eq.0)then
          if(.not.fexist)then
             snapshot_open_unknown%success = .false.
             snapshot_open_unknown%string  = "File does not exist"
             return
          endif
       else
          snapshot_open_unknown%success = .false.
          snapshot_open_unknown%string  = "Unable to access file" 
          return
       endif
    endif

    ! Try all readers in sequence
    do i = 1, nformat, 1
       call snapshot_set_format(format_names(i))
       res = snapshot_open(fname,isnap,ri)
       if(res%success)then
          snapshot_open_unknown%success = .true.
          return
       else
          errstr(i) = trim(res%string)
       endif
    end do
    
    ! If we get to here, we couldn't read the file
    snapshot_open_unknown%success = .false.
    snapshot_open_unknown%string  = "Unable to identify snapshot format"

    ! Write out why each attempt failed
    write(0,*)
    do i = 1, nformat, 1
       write(0,*)"Attempted to open file as ",trim(format_names(i))
       write(0,*)"RESULT: ",trim(errstr(i))
    end do
    write(0,*)

    return
  end function snapshot_open_unknown

  
end module snapshot_reader
