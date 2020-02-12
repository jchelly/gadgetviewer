module gadget_binary_type2_reader
!
! Module to read binary Gadget files in type 2 format. These have a four
! character tag before each data block which can be used to identify it.
!
! TODO: determine size of snapshots with >=2**31 particles
! correctly!
!
#include "../../config.h"
  use data_types
  use byteswapper
  use return_status
  use particle_store
  use file_units
  use progress_bar
  use gadget_path
  use string_module
  use summary
  use key_file
  use partial_read_info

  implicit none
  private
  save

  public :: gadget_binary_type2_open
  public :: gadget_binary_type2_read
  public :: gadget_binary_type2_read_conf

  ! Simulation details
  integer                     :: nfiles
  logical                     :: need_byteswap

  ! List of blocks in the Gadget file
  ! This doesn't include header, pos, vel, id, mass which are always
  ! read.
  type blocktype
     character(len=4)      :: tag
     character(len=maxlen) :: name
     logical, dimension(6) :: flag
     character(len=20)     :: type
  end type blocktype
  integer, parameter :: nblockmax = 100
  integer            :: nblocks
  type (blocktype), dimension(nblockmax+1) :: block

  ! Path information extracted from file name
  type (path_data_type) :: path_data

  character(len=100), parameter :: header_name = "Gadget Type 2 Binary"

contains


  subroutine gadget_binary_type2_read_conf(dir)
!
! Read in the configuration file, or create a default one if it doesn't
! exist
!
    implicit none
    character(len=*)   :: dir
    character(len=500) :: fname
    integer            :: ios
    character(len=500) :: str
    character(len=50), dimension(20) :: strarr
    integer :: i, nstr, j, iblock
    logical :: is_duplicate
    integer :: nblockname
    character(len=maxlen), dimension(nblockmax) :: blockname
    integer, dimension(maxspecies) :: list
    integer :: nlist

    fname = trim(dir)//"/"//"gadget_binary_type2_blocks"
    call read_key_file(fname)
    if(.not.file_has_group(header_name))then
       ! Default settings
       nblocks  = 3
       call set_key(header_name, "NBlocks", nblocks)
       block(1)%tag ="U"
       block(1)%name="InternalEnergy"
       block(1)%flag=(/.true.,.false.,.false.,.false.,.false.,.false./)
       block(1)%type="REAL"
       !
       block(2)%tag ="RHO"
       block(2)%name="Density"
       block(2)%flag=(/.true.,.false.,.false.,.false.,.false.,.false./)
       block(2)%type="REAL"
       !
       block(3)%tag ="HSML"
       block(3)%name="SmoothingLength"
       block(3)%flag=(/.true.,.false.,.false.,.false.,.false.,.false./)
       block(3)%type="REAL"
       !
       !block(4)%tag ="POT"
       !block(4)%name="Potential"
       !block(4)%flag=(/.true.,.false.,.false.,.false.,.false.,.false./)
       !block(4)%type="REAL"
       !
       do i = 1, nblocks, 1
          call set_key(block(i)%tag, "Displayed Name", block(i)%name)
          call flags_to_list(block(i)%flag, list, nlist)
          call set_key(block(i)%tag, "Particle Types", list(1:nlist))
          call set_key(block(i)%tag, "Data Type",      block(i)%type)
       end do
       call write_key_file(fname)
    endif
    call get_group_names(nblockname, blockname)

    nblocks = 0
    do iblock = 1, nblockname, 1
       if(blockname(iblock).eq.header_name)cycle
       block(nblocks+1)%tag  = trim(blockname(iblock))
       block(nblocks+1)%name = ""
       block(nblocks+1)%flag = .false.
       block(nblocks+1)%type = "X"
       call get_key(blockname(iblock), "Displayed Name", block(nblocks+1)%name)
       list = -1
       call get_key(blockname(iblock), "Particle Types", list)
       call list_to_flags(list, block(nblocks+1)%flag)
       call get_key(blockname(iblock), "Data Type",      block(nblocks+1)%type)
       ! Ignore blocks with no name
       if(len_trim(block(nblocks+1)%name).eq.0)cycle
       ! Ignore blocks with invalid data types
       if(block(nblocks+1)%type.ne."REAL".and. &
            block(nblocks+1)%type.ne."INTEGER")cycle
       ! Ignore blocks with all flags false
       if(all(.not.block(nblocks+1)%flag))cycle
       ! Ignore blocks with duplicate names or tags
       is_duplicate = .false.
       do i = 1, nblocks, 1
          if(block(i)%tag.eq.block(nblocks+1)%tag)is_duplicate=.true.
          if(block(i)%name.eq.block(nblocks+1)%name)is_duplicate=.true.
       end do
       if(is_duplicate)cycle
       ! Ignore blocks that have the same tag as things we always read
       if(trim(block(nblocks+1)%tag).eq."POS")cycle
       if(trim(block(nblocks+1)%tag).eq."VEL")cycle
       if(trim(block(nblocks+1)%tag).eq."ID")cycle
       if(trim(block(nblocks+1)%tag).eq."MASS")cycle
       ! Ignore blocks that have the same name as things we always read
       if(trim(block(nblocks+1)%tag).eq."Coordinates")cycle
       if(trim(block(nblocks+1)%tag).eq."Velocity")cycle
       if(trim(block(nblocks+1)%tag).eq."Velocities")cycle
       if(trim(block(nblocks+1)%tag).eq."ID")cycle
       if(trim(block(nblocks+1)%tag).eq."Mass")cycle
       ! If we get this far, keep this block
       nblocks = nblocks + 1
    end do
    call close_key_file()

    return
  end subroutine gadget_binary_type2_read_conf

!
! Scan a simulation directory and store information
! required for reading snapshots
!
  type(result_type) function gadget_binary_type2_open(fname, isnap, rinfo)

    implicit none
    ! Parameters
    character(len=*) :: fname
    integer, intent(out) :: isnap
    type (read_info)     :: rinfo
    ! Internal
    integer :: ios
    logical :: fexist
    integer(kind=int4byte) :: irec
    logical :: swap_endian
    integer(kind=int4byte), dimension(31) :: dummy 
    integer :: jsnap
    type(result_type) :: res
    character(len=4) :: tag
    integer(kind=int4byte) :: nextblock

    gadget_binary_type2_open%success = .false.

    ! Haven't implemented partial read for this format yet
    if(rinfo%do_sampling.or.rinfo%just_this_file.or.rinfo%do_sphere)then
       gadget_binary_type2_open%string=&
            "Partial read not yet implemented for this format"
       return
    endif

    ! Check we haven't been asked to use spatial indexing
    if(rinfo%use_index)then
       gadget_binary_type2_open%string  = &
            "No spatial indexing in Gadget binary-2 snapshot"
       return
    endif

    ! Check if the file exists
    inquire(file=fname,exist=fexist,iostat=ios)
    if(ios.eq.0)then
       if(.not.fexist)then
          gadget_binary_type2_open%success = .false.
          gadget_binary_type2_open%string  = "File does not exist"
          return
       endif
    else
       gadget_binary_type2_open%success = .false.
       gadget_binary_type2_open%string  = "Unable to access file" 
       return
    endif

    ! Open the file
    call open_binary(fname, ios)
    if(ios.ne.0)then
       gadget_binary_type2_open%string="Unable to open file: "//trim(fname)
       return
    endif

    ! Detect endian-ness using the first record marker
    call read_binary(irec, ios)
    if(ios.ne.0)then
       gadget_binary_type2_open%string="Unable to read from file: "// &
            trim(fname)
       call close_binary()
       return
    endif

    ! Record marker should be 8 or 134217728 ( = byteswapped 8)
    if(irec.eq.8)then
       swap_endian = .false.
    else if(irec.eq.134217728)then
       ! Need byte swapping
       swap_endian = .true.
    else
       ! Not a gadget file?
       gadget_binary_type2_open%string= &
            "First tag record length is incorrect: "// &
            trim(fname)
       call close_binary()
       return
    endif

    ! Read the header tag and the two record markers after it
    call read_binary(tag,       ios)
    call read_binary(nextblock, ios)
    call read_binary(irec,      ios)
    call read_binary(irec,      ios)
    if(ios.ne.0)then
       gadget_binary_type2_open%string="Unable to read from file: "// &
            trim(fname)
       call close_binary()
       return
    endif
    if(tag.ne."HEAD")then
       ! Not a gadget file? (assuming header is always first here!)
       gadget_binary_type2_open%string="Header tag record is incorrect: "// &
            trim(fname)
       call close_binary()
       return
    endif

    ! Read the number of files
    ! First skip header info we don't need right now.
    call read_binary(dummy, ios)
    if(ios.ne.0)then
       gadget_binary_type2_open%string=&
            "Unable to read from file: "//trim(fname)
       call close_binary()
       return
    endif
    ! This is the number of files
    call read_binary(irec, ios)
    if(ios.ne.0)then
       gadget_binary_type2_open%string="Unable to read from file: "// &
            trim(fname)
       call close_binary()
       return
    endif
    if(swap_endian)call byteswap(irec)
    call close_binary()
    
    ! Extract information from the path so we can try to infer where the
    ! other snapshots will be
    res = gadget_path_extract(fname, jsnap, path_data)
    if(.not.res%success)then
       ! fname is somehow messed up enough that we can't interpret it,
       ! despite it being a valid filename
       gadget_binary_type2_open = res
       isnap = -1
       return
    endif

    ! Looks ok - store details and return snapshot number
    nfiles        = max(1,irec)
    isnap         = jsnap
    need_byteswap = swap_endian

    gadget_binary_type2_open%success = .true.
    return

  end function gadget_binary_type2_open

!
! Read the specified snapshot, if possible
!
  type (result_type) function gadget_binary_type2_read(isnap, rinfo)

    implicit none
    ! Parameters
    integer :: isnap
    type (read_info) :: rinfo
    ! Internal
    integer :: ifile
    integer :: np, nmass
    integer(kind=int4byte), dimension(6) :: nptot4, npfile4, nptot_hw4
    integer(kind=int8byte), dimension(6) :: nptot, npfile, nptot_hw
    real(kind=real8byte), dimension(6) :: massarr
    real(kind=real8byte) :: time, redshift, boxsize
    integer(kind=int4byte) :: flag_sfr, flag_feedback, flag_cooling, numfiles
    character(len=fname_maxlen) :: fname
    integer(kind=int4byte), dimension(8) :: dummy1
    integer(kind=int4byte), dimension(16) :: dummy2
    integer(kind=int4byte) :: irec
    integer :: ios, i, istat
    ! Temporary storage for particles
    ! Positions
    real(kind=real4byte),    dimension(:,:), allocatable :: pos4
    real(kind=real8byte),    dimension(:,:), allocatable :: pos8
    ! Velocities
#ifdef READ_VEL
    real(kind=real4byte),    dimension(:,:), allocatable :: vel4
    real(kind=real8byte),    dimension(:,:), allocatable :: vel8
#endif
    ! Masses
    real(kind=real4byte),    dimension(:),   allocatable :: mass4
    real(kind=real8byte),    dimension(:),   allocatable :: mass8
    ! IDs
    integer(kind=int4byte),  dimension(:),   allocatable :: id4
    integer(kind=int8byte),  dimension(:),   allocatable :: id8
    ! Additional properties
    real(kind=real4byte),    dimension(:),   allocatable :: rdata4
    real(kind=real8byte),    dimension(:),   allocatable :: rdata8
    integer(kind=int4byte),  dimension(:),   allocatable :: idata4
    integer(kind=int8byte),  dimension(:),   allocatable :: idata8
    ! Particle type names
    character(len=50), dimension(6) :: species_name
    ! Result from function calls
    type (result_type) :: res
    ! Progress bar
    integer(kind=int8byte) :: prog_tot, prog_so_far
    ! Number of bytes used to store one value (position,velocity,id etc)
    integer :: byte_size
    ! String tag for a data block
    character(len=4) :: tag
    ! Record whether memory has been allocated for each bock
    logical, dimension(nblockmax,6) :: block_allocated
    integer :: nbytes, iblock, nextblock
    character, dimension(:), allocatable :: tmp

    gadget_binary_type2_read%success = .false.

    ! Haven't implemented partial read for this format yet
    if(rinfo%do_sampling.or.rinfo%just_this_file.or.rinfo%do_sphere)then
       gadget_binary_type2_read%string=&
            "Partial read not yet implemented for this format"
       return
    endif

    ! Check we haven't been asked to use spatial indexing
    if(rinfo%use_index)then
       gadget_binary_type2_read%success = .false.
       gadget_binary_type2_read%string  = &
            "No spatial indexing in Gadget binary-2 snapshot"
       return
    endif

    call progress_bar_update(0.0)

    ! Read one file to get the total particle number
    call gadget_path_generate(isnap, 0, fname, path_data)

    call open_binary(fname, ios)
    if(ios.ne.0)then
       gadget_binary_type2_read%string="Unable to open file: "//trim(fname)
       return
    endif

    ! Skip header tag
    call read_binary(irec,      ios)
    call read_binary(tag,       ios)
    call read_binary(nextblock, ios)
    call read_binary(irec,      ios)

    ! Read stuff we need from the header
    call read_binary(irec,          ios)
    need_byteswap = (irec.ne.256)
    call read_binary(npfile4,        ios)
    if(need_byteswap)call byteswap(npfile4)
    where(npfile4.ge.0)
       npfile = npfile4
    elsewhere
       npfile = npfile4 + 2_int8byte ** 32
    end where
    call read_binary(massarr,       ios)
    call read_binary(time,          ios)
    call read_binary(redshift,      ios)
    call read_binary(flag_sfr,      ios)
    call read_binary(flag_feedback, ios)
    call read_binary(nptot4,         ios)
    if(need_byteswap)call byteswap(nptot4)
    where(nptot4.ge.0)
       nptot = nptot4
    elsewhere
       nptot = nptot4 + 2_int8byte ** 32
    end where
    call read_binary(flag_cooling,  ios)
    call read_binary(numfiles,      ios)
    call read_binary(boxsize,       ios)
    if(need_byteswap)call byteswap(boxsize)
    call read_binary(dummy1,        ios)
    call read_binary(nptot_hw4,        ios)
    if(need_byteswap)call byteswap(nptot_hw4)
    where(nptot_hw4.ge.0)
       nptot_hw = nptot_hw4
    elsewhere
       nptot_hw = nptot_hw4 + 2_int8byte ** 32
    end where
    nptot = nptot + nptot_hw * 2_int8byte ** 32
    call read_binary(dummy2,        ios)
    call read_binary(irec,          ios)
    if(ios.ne.0)then
       gadget_binary_type2_read%string= &
            "Unable to read from file: "//trim(fname)
       call close_binary()
       return
    endif

    ! At this point we know the file is readable, so deallocate the old
    ! snapshot
    call particle_store_empty(pdata)
    call particle_store_empty(psample)

    if(need_byteswap)then
       call byteswap(numfiles)
       call byteswap(massarr)
       call byteswap(time)
       call byteswap(redshift)
       call byteswap(flag_sfr)
       call byteswap(flag_feedback)
    endif
    call close_binary()

    ! Number of files may have changed
    nfiles = max(1,numfiles)

    ! In single file snapshots nptot may not be set
    if(nfiles.eq.1)nptot=npfile

    ! Now we know how many particles we have, allocate storage
    ! Note that this only allocates positions, velocities, masses and IDs.
    ! Any additional blocks will be allocated as they are found in the
    ! file.
    do i = 1, 6, 1
       select case(i)
       case(1)
          species_name(i)="Type 0 - Gas"
       case(2)
          species_name(i)="Type 1 - Dark matter"
       case(3)
          species_name(i)="Type 2 - Boundary"
       case(4)
          species_name(i)="Type 3 - Boundary"
       case(5)
          species_name(i)="Type 4 - Boundary/stars"
       case(6)
          species_name(i)="Type 5 - Other"
       end select
       !if(nptot(i).gt.0)then
          res = particle_store_new_species(pdata,species_name(i),int(nptot(i),index_kind))
          ! Abort if memory allocation fails
          if(.not.res%success)then
             gadget_binary_type2_read = res ! Pass back return status
             call particle_store_empty(pdata) ! Clean up any memory allocated
             return
          endif
       !endif
    end do

    ! Allocate storage for masses and IDs, since we always need these.
    do i = 1, 6, 1

       res = particle_store_new_property(pdata,species_name(i),"Mass", &
            "REAL", is_mass=.true.)
       if(.not.res%success)then
          gadget_binary_type2_read = res
          call particle_store_empty(pdata)
          return
       endif
       res = particle_store_new_property(pdata,species_name(i),"ID", &
            "INTEGER", is_id=.true.)
       if(.not.res%success)then
          gadget_binary_type2_read = res
          call particle_store_empty(pdata)
          return
       endif
    end do

    ! Work out values for progress bar
    prog_tot    = 8*nfiles
    prog_so_far = 0

    ! Loop over files and read them
    block_allocated = .false.
    do ifile = 0, nfiles-1, 1

       ! Generate filename
       call gadget_path_generate(isnap, ifile, fname, &
            path_data)

       ! Open file
       call open_binary(fname, ios)
       if(ios.ne.0)then
          gadget_binary_type2_read%string="Unable to open file: "//trim(fname)
          call particle_store_empty(pdata)
          return
       endif

       ! Skip header tag
       call read_binary(irec,      ios)
       call read_binary(tag,       ios)
       call read_binary(nextblock, ios)
       call read_binary(irec,      ios)
       
       ! Read header
       call read_binary(irec,          ios)
       call read_binary(npfile4,        ios)
       if(need_byteswap)call byteswap(npfile4)
       where(npfile4.ge.0)
          npfile = npfile4
       elsewhere
          npfile = npfile4 + 2_int8byte ** 32
       end where
       call read_binary(massarr,       ios)
       call read_binary(time,          ios)
       call read_binary(redshift,      ios)
       call read_binary(flag_sfr,      ios)
       call read_binary(flag_feedback, ios)
       call read_binary(nptot4,        ios)
       if(need_byteswap)call byteswap(nptot4)
       where(nptot4.ge.0)
          nptot = nptot4
       elsewhere
          nptot = nptot4 + 2_int8byte ** 32
       end where
       call read_binary(flag_cooling,  ios)
       call read_binary(numfiles,      ios)
       call read_binary(boxsize,       ios)
       if(need_byteswap)call byteswap(boxsize)
       call read_binary(dummy1,        ios)
       call read_binary(nptot_hw4,        ios)
       if(need_byteswap)call byteswap(nptot_hw4)
       where(nptot_hw4.ge.0)
          nptot_hw = nptot_hw4
       elsewhere
          nptot_hw = nptot_hw4 + 2_int8byte ** 32
       end where
       nptot = nptot + nptot_hw * 2_int8byte ** 32
       call read_binary(dummy2,        ios)
       call read_binary(irec,          ios)
       if(ios.ne.0)then
          gadget_binary_type2_read%string= &
               "Unable to read from file: "//trim(fname)
          call particle_store_empty(pdata)
          call close_binary()
          return
       endif
       if(need_byteswap)then
          call byteswap(numfiles)
          call byteswap(massarr)
          call byteswap(time)
          call byteswap(redshift)
          call byteswap(flag_sfr)
          call byteswap(flag_feedback)
       endif

       ! In single file snapshots nptot may not be set
       if(nfiles.eq.1)nptot=npfile

       ! Skip file if there are no particles
       if(sum(npfile).eq.0)then
          call close_binary()
          cycle
       endif

       ! Now, loop over the blocks in this file and read any that are
       ! recognised.
       do while(.true.)

          ! Read the tag for this block
          call read_binary(irec,      ios)
          call read_binary(tag,       ios)
          call read_binary(nextblock, ios)
          call read_binary(irec,      ios)
          if(ios.ne.0)exit

          ! Determine size of the block
          call read_binary(irec, ios)
          if(ios.ne.0)exit
          if(need_byteswap)call byteswap(irec)
          nbytes = irec

          ! Process this block
          select case(tag)
#ifdef READ_VEL
          case("POS","VEL")
#else
          case("POS")
#endif
             ! Read position or mass block
             byte_size = nbytes / (3*sum(npfile))
             do i = 1, 6, 1
                if(npfile(i).gt.0)then
                   select case(byte_size)
                   case(4)
                      ! Data is real*4
                      allocate(pos4(3,npfile(i)),stat=istat)
                      if(istat.gt.0)then
                         call particle_store_empty(pdata)
                         gadget_binary_type2_read%string = &
                              "Unable to allocate buffer for "//tag//" block"
                         call close_binary()
                         return
                      endif
                      call read_binary(pos4, ios)
                      if(need_byteswap)call byteswap(pos4)
                      if(ios.ne.0)then
                         gadget_binary_type2_read%string= &
                              "Unable to read from file: "//trim(fname)
                         call particle_store_empty(pdata)
                         call close_binary()
                         return
                      endif
                      if(tag.eq."POS")then
                         res = particle_store_add_data(pdata, species_name(i),&
                              pos=real(pos4,pos_kind))
                      else
                         res = particle_store_add_data(pdata, species_name(i),&
                              vel=real(pos4,vel_kind))
                      endif
                      deallocate(pos4)
                      if(.not.res%success)then
                         gadget_binary_type2_read%string= &
                              "Unable to allocate memory"
                         call particle_store_empty(pdata)
                         call close_binary()
                      endif
                   case(8)
                      ! Data is real*8
                      allocate(pos8(3,npfile(i)),stat=istat)
                      if(istat.gt.0)then
                         call particle_store_empty(pdata)
                         gadget_binary_type2_read%string = &
                              "Unable to allocate buffer for "//tag//" block"
                         call close_binary()
                         return
                      endif
                      call read_binary(pos8, ios)
                      if(need_byteswap)call byteswap(pos8)
                      if(ios.ne.0)then
                         gadget_binary_type2_read%string= &
                              "Unable to read from file: "//trim(fname)
                         call particle_store_empty(pdata)
                         call close_binary()
                         return
                      endif
                      if(tag.eq."POS")then
                         res = particle_store_add_data(pdata, species_name(i),&
                              pos=real(pos8,pos_kind))
                      else
                         res = particle_store_add_data(pdata, species_name(i),&
                              vel=real(pos8,vel_kind))
                      endif
                      deallocate(pos8)
                      if(.not.res%success)then
                         gadget_binary_type2_read%string= &
                              "Unable to allocate memory"
                         call particle_store_empty(pdata)
                         call close_binary()
                      endif
                   case default
                      ! Data precision is not 4 or 8 bytes (or corrupt file)
                      gadget_binary_type2_read%string= &
                           "Length of "//tag//" record doesn't make sense"
                      call particle_store_empty(pdata)
                      call close_binary()
                      return
                   end select
                endif
                ! Next particle type
             end do
             ! Update progress bar
             prog_so_far = prog_so_far + 3
             call progress_bar_update(real(prog_so_far)/real(prog_tot))
          case("MASS")
             ! Read in particle masses
             ! Figure out precision first
             nmass = 0
             do i = 1, 6, 1
                if(massarr(i).eq.0.0)nmass = nmass + npfile(i)
             end do
             byte_size = nbytes / nmass
             ! Loop over particle types with individual masses
             do i = 1, 6, 1
                if(npfile(i).gt.0.and.massarr(i).eq.0.0)then
                   ! Need to read masses for these particles
                   select case(byte_size)
                   case(4)
                      allocate(mass4(npfile(i)),stat=istat)
                      if(istat.gt.0)then
                         call particle_store_empty(pdata)
                         gadget_binary_type2_read%string = &
                              "Unable to allocate buffer for "//tag//" block"
                         call close_binary()
                         return
                      endif
                      call read_binary(mass4, ios)
                      if(need_byteswap)call byteswap(mass4)
                      if(ios.ne.0)then
                         gadget_binary_type2_read%string= &
                              "Unable to read from file: "//trim(fname)
                         call particle_store_empty(pdata)
                         call close_binary()
                         return
                      endif
                      res = particle_store_add_data(pdata, species_name(i), &
                           prop_name="Mass", rdata=real(mass4,r_prop_kind))
                      deallocate(mass4)
                      if(.not.res%success)then
                         gadget_binary_type2_read%string= &
                              "Unable to allocate memory"
                         call particle_store_empty(pdata)
                         call close_binary()
                      endif
                   case(8)
                      allocate(mass8(npfile(i)),stat=istat)
                      if(istat.gt.0)then
                         call particle_store_empty(pdata)
                         gadget_binary_type2_read%string = &
                              "Unable to allocate buffer for "//tag//" block"
                         call close_binary()
                         return
                      endif
                      call read_binary(mass8, ios)
                      if(need_byteswap)call byteswap(mass8)
                      if(ios.ne.0)then
                         gadget_binary_type2_read%string= &
                              "Unable to read from file: "//trim(fname)
                         call particle_store_empty(pdata)
                         call close_binary()
                         return
                      endif
                      res = particle_store_add_data(pdata, species_name(i), &
                           prop_name="Mass", rdata=real(mass8,r_prop_kind))
                      deallocate(mass8)
                      if(.not.res%success)then
                         gadget_binary_type2_read%string= &
                              "Unable to allocate memory"
                         call particle_store_empty(pdata)
                         call close_binary()
                      endif
                   case default
                      ! Data precision is not 4 or 8 bytes (or corrupt file)
                      gadget_binary_type2_read%string= &
                           "Length of "//tag//" record doesn't make sense"
                      call particle_store_empty(pdata)
                      call close_binary()
                      return
                   end select
                endif
                ! Next particle type
             end do
             ! Update progress bar
             prog_so_far = prog_so_far + 1
             call progress_bar_update(real(prog_so_far)/real(prog_tot))
          case("ID")
             ! Read particle IDs
             byte_size = nbytes / sum(npfile)
             ! Loop over particle types
             do i = 1, 6, 1
                if(npfile(i).gt.0)then
                   select case(byte_size)
                   case(4)
                      call particle_store_set_idsize(pdata,4)
                      allocate(id4(npfile(i)),stat=istat)
                      if(istat.gt.0)then
                         call particle_store_empty(pdata)
                         gadget_binary_type2_read%string = &
                              "Unable to allocate buffer for "//tag//" block"
                         call close_binary()
                         return
                      endif
                      call read_binary(id4, ios)
                      if(need_byteswap)call byteswap(id4)
                      if(ios.ne.0)then
                         gadget_binary_type2_read%string= &
                              "Unable to read from file: "//trim(fname)
                         call particle_store_empty(pdata)
                         call close_binary()
                         return
                      endif
                      res = particle_store_add_data(pdata,species_name(i), &
                           prop_name="ID", idata=int(id4,i_prop_kind))
                      deallocate(id4)
                      if(.not.res%success)then
                         gadget_binary_type2_read%string= &
                              "Unable to allocate memory"
                         call particle_store_empty(pdata)
                         call close_binary()
                      endif
                   case(8)
                      call particle_store_set_idsize(pdata,8)
                      allocate(id8(npfile(i)),stat=istat)
                      if(istat.gt.0)then
                         call particle_store_empty(pdata)
                         gadget_binary_type2_read%string = &
                              "Unable to allocate buffer for "//tag//" block"
                         call close_binary()
                         return
                      endif
                      call read_binary(id8, ios)
                      if(need_byteswap)call byteswap(id8)
                      if(ios.ne.0)then
                         gadget_binary_type2_read%string= &
                              "Unable to read from file: "//trim(fname)
                         call particle_store_empty(pdata)
                         call close_binary()
                         return
                      endif
                      res = particle_store_add_data(pdata,species_name(i), &
                           prop_name="ID", idata=int(id8,i_prop_kind))
                      deallocate(id8)
                      if(.not.res%success)then
                         gadget_binary_type2_read%string= &
                              "Unable to allocate memory"
                         call particle_store_empty(pdata)
                         call close_binary()
                      endif
                   case default
                      ! Data precision is not 4 or 8 bytes (or corrupt file)
                      gadget_binary_type2_read%string= &
                           "Length of "//tag//" record doesn't make sense"
                      call particle_store_empty(pdata)
                      call close_binary()
                      return
                   end select
                endif
                ! Next particle type
             end do
             ! Update progress bar
             prog_so_far = prog_so_far + 1
             call progress_bar_update(real(prog_so_far)/real(prog_tot))
          case default
             ! Read or skip additional blocks. 
             ! First see if this tag is in the list of blocks to read.
             iblock = -1
             do i = 1, nblocks, 1
                if(block(i)%tag.eq.tag)iblock = i
             end do
             if(iblock.gt.0)then
                ! This is a recognised block and we should read it.
                ! Count particles which have this property.
                np = 0
                do i = 1, 6, 1
                   if(block(iblock)%flag(i))np=np+npfile(i)
                end do
                ! Determine precision
                byte_size = nbytes / np
                ! Loop over particle types this block applies to
                do i = 1, 6, 1
                   if(block(iblock)%flag(i))then
                      ! Allocate storage for the new property if necessary
                      ! (only needs to be done the first time we read a block
                      ! of this type)
                      if(.not.block_allocated(iblock,i))then
                         res = particle_store_new_property(pdata,&
                              species_name(i), &
                              block(iblock)%name, block(iblock)%type)
                         if(.not.res%success)then
                            call particle_store_empty(pdata)
                            gadget_binary_type2_read = res
                            call close_binary()
                            return
                         endif
                         block_allocated(iblock,i) = .true.
                      endif
                      ! Read the data
                      select case(byte_size)
                      case(4) ! 4 byte data
                         select case(block(iblock)%type)
                         case("REAL")
                            allocate(rdata4(npfile(i)),stat=istat)
                            if(istat.gt.0)then
                               call particle_store_empty(pdata)
                               gadget_binary_type2_read%string = &
                                    "Unable to allocate buffer for "// &
                                    tag//" block"
                               call close_binary()
                               return
                            endif
                            call read_binary(rdata4, ios)
                            if(need_byteswap)call byteswap(rdata4)
                            if(ios.ne.0)then
                               gadget_binary_type2_read%string= &
                                    "Unable to read from file: "//trim(fname)
                               call particle_store_empty(pdata)
                               call close_binary()
                               return
                            endif
                            res = particle_store_add_data(pdata, &
                                 species_name(i), &
                                 prop_name=block(iblock)%name, &
                                 rdata=real(rdata4,r_prop_kind))
                            deallocate(rdata4)
                            if(.not.res%success)then
                               gadget_binary_type2_read%string= &
                                    "Unable to allocate memory"
                               call particle_store_empty(pdata)
                               call close_binary()
                            endif
                         case("INTEGER")
                            allocate(idata4(npfile(i)),stat=istat)
                            if(istat.gt.0)then
                               call particle_store_empty(pdata)
                               gadget_binary_type2_read%string = &
                                    "Unable to allocate buffer for "// &
                                    tag//" block"
                               call close_binary()
                               return
                            endif
                            call read_binary(idata4, ios)
                            if(need_byteswap)call byteswap(idata4)
                            if(ios.ne.0)then
                               gadget_binary_type2_read%string= &
                                    "Unable to read from file: "//trim(fname)
                               call particle_store_empty(pdata)
                               call close_binary()
                               return
                            endif
                            res = particle_store_add_data(pdata, &
                                 species_name(i), &
                                 prop_name=block(iblock)%name, &
                                 idata=int(idata4,i_prop_kind))
                            deallocate(idata4)
                            if(.not.res%success)then
                               gadget_binary_type2_read%string= &
                                    "Unable to allocate memory"
                               call particle_store_empty(pdata)
                               call close_binary()
                            endif
                         end select
                      case(8) ! 8 byte data
                         select case(block(iblock)%type)
                         case("REAL")
                            allocate(rdata8(npfile(i)),stat=istat)
                            if(istat.gt.0)then
                               call particle_store_empty(pdata)
                               gadget_binary_type2_read%string = &
                                    "Unable to allocate buffer for "// &
                                    tag//" block"
                               call close_binary()
                               return
                            endif
                            call read_binary(rdata8, ios)
                            if(need_byteswap)call byteswap(rdata8)
                            if(ios.ne.0)then
                               gadget_binary_type2_read%string= &
                                    "Unable to read from file: "//trim(fname)
                               call particle_store_empty(pdata)
                               call close_binary()
                               return
                            endif
                            res = particle_store_add_data(pdata, &
                                 species_name(i), &
                                 prop_name=block(iblock)%name, &
                                 rdata=real(rdata8,r_prop_kind))
                            deallocate(rdata8)
                            if(.not.res%success)then
                               gadget_binary_type2_read%string= &
                                    "Unable to allocate memory"
                               call particle_store_empty(pdata)
                               call close_binary()
                            endif
                         case("INTEGER")
                            allocate(idata8(npfile(i)),stat=istat)
                            if(istat.gt.0)then
                               call particle_store_empty(pdata)
                               gadget_binary_type2_read%string = &
                                    "Unable to allocate buffer for "// &
                                    tag//" block"
                               call close_binary()
                               return
                            endif
                            call read_binary(idata8, ios)
                            if(need_byteswap)call byteswap(idata8)
                            if(ios.ne.0)then
                               gadget_binary_type2_read%string= &
                                    "Unable to read from file: "//trim(fname)
                               call particle_store_empty(pdata)
                               call close_binary()
                               return
                            endif
                            res = particle_store_add_data(pdata, &
                                 species_name(i), &
                                 prop_name=block(iblock)%name, &
                                 idata=int(idata8,i_prop_kind))
                            deallocate(idata8)
                            if(.not.res%success)then
                               gadget_binary_type2_read%string= &
                                    "Unable to allocate memory"
                               call particle_store_empty(pdata)
                               call close_binary()
                            endif
                         end select
                      case default
                         ! Data precision is not 4 or 8 bytes (or corrupt file)
                         gadget_binary_type2_read%string= &
                              "Length of "//tag//" record doesn't make sense"
                         call particle_store_empty(pdata)
                         call close_binary()
                         return
                      end select
                   endif
                   ! Next particle type
                end do
             else
                ! Block is not recognised, so skip it
                allocate(tmp(nbytes))
                call read_binary(tmp, ios)
                deallocate(tmp)
             end if
          end select

          ! Skip end of record marker
          call read_binary(irec, ios)
          if(ios.ne.0)exit

          ! Next block
       end do

       ! Set any masses that are specified in the header
       do i = 1, 6, 1
          if(npfile(i).gt.0.and.massarr(i).gt.0.0)then
             allocate(mass4(npfile(i)))
             mass4 = massarr(i)
             res = particle_store_add_data(pdata, species_name(i), &
                  prop_name="Mass", rdata=real(mass4,r_prop_kind))
             deallocate(mass4)
             if(.not.res%success)then
                gadget_binary_type2_read%string= &
                     "Unable to allocate memory"
                call particle_store_empty(pdata)
                call close_binary()
             endif
          endif
       end do

       ! Close this file
       call close_binary()

       ! Next file
    end do

    ! Record the redshift etc
    call particle_store_set_time(pdata, real(time), real(redshift), real(time))
    call particle_store_set_boxsize(pdata, real(boxsize))

    gadget_binary_type2_read%success = .true.

    if(need_byteswap)then
       call summary_add_line(s,"Byteswapping required")
    else
       call summary_add_line(s,"Byteswapping not required")
    endif
    call summary_add_line(s,&
         "Number of files = "//trim(adjustl(string(nfiles))))
    
    return
  end function gadget_binary_type2_read


  subroutine flags_to_list(flags, list, n)

    implicit none
    logical, dimension(:), intent(in)  :: flags
    integer, dimension(:), intent(out) :: list
    integer,               intent(out) :: n
    integer :: i
    
    n = 0
    do i = 1, size(flags), 1
       if(flags(i))then
          n = n + 1
          list(n) = i - 1
       endif
    end do

    return
  end subroutine flags_to_list


  subroutine list_to_flags(list, flags)

    implicit none
    logical, dimension(:), intent(out) :: flags
    integer, dimension(:), intent(in)  :: list
    integer :: i, n

    flags = .false.
    do i = 1, size(list), 1
       if(list(i).ge.0.and.list(i).lt.size(flags))then
          flags(list(i)+1) = .true.
       endif
    end do

    return
  end subroutine list_to_flags


end module gadget_binary_type2_reader
