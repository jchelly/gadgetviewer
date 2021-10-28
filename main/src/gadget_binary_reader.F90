module gadget_binary_reader
!
! Module to read binary Gadget files
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
  use summary
  use string_module
  use partial_read_info
  use array_shrinker

  implicit none
  private
  save

  public :: gadget_binary_open
  public :: gadget_binary_read

  ! Simulation details
  integer                     :: nfiles
  logical                     :: need_byteswap

  ! Path information extracted from file name
  type (path_data_type) :: path_data

contains

!
! Scan a simulation directory and store information
! required for reading snapshots
!
  type(result_type) function gadget_binary_open(fname, isnap, rinfo)

    implicit none
    ! Parameters
    character(len=*)     :: fname
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

    gadget_binary_open%success = .false.

    ! Check if the file exists
    inquire(file=fname,exist=fexist,iostat=ios)
    if(ios.eq.0)then
       if(.not.fexist)then
          gadget_binary_open%success = .false.
          gadget_binary_open%string  = "File does not exist"
          return
       endif
    else
       gadget_binary_open%success = .false.
       gadget_binary_open%string  = "Unable to access file" 
       return
    endif

    ! Open the file
    call open_binary(fname,ios)
    if(ios.ne.0)then
       gadget_binary_open%string="Unable to open file: "//trim(fname)
       return
    endif

    ! Detect endian-ness using the first record marker
    call read_binary(irec,ios)
    if(ios.ne.0)then
       gadget_binary_open%string="Unable to read from file: "//trim(fname)
       call close_binary()
       return
    endif

    ! Record marker should be 256 or 65536
    if(irec.eq.256)then
       swap_endian = .false.
    else if(irec.eq.65536)then
       ! Need byte swapping
       swap_endian = .true.
    else
       ! Not a gadget file?
       gadget_binary_open%string="Header record length is incorrect: "// &
            trim(fname)
       call close_binary()
       return
    endif

    ! Read the number of files
    call read_binary(dummy,ios)
    if(ios.ne.0)then
       gadget_binary_open%string="Unable to read from file: "//trim(fname)
       call close_binary()
       return
    endif
    ! This is the number of files
    call read_binary(irec,ios)
    if(ios.ne.0)then
       gadget_binary_open%string="Unable to read from file: "//trim(fname)
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
       gadget_binary_open = res
       isnap = -1
       return
    endif

    ! Looks ok - store details and return snapshot number.
    ! Initial conditions files sometimes have nfiles=0, in which case
    ! we have to assume one file.
    nfiles        = max(1,irec)
    isnap         = jsnap
    need_byteswap = swap_endian

    gadget_binary_open%success = .true.
    return

  end function gadget_binary_open

!
! Read the specified snapshot, if possible
!
  type (result_type) function gadget_binary_read(isnap, rinfo)

    implicit none
    ! Parameters
    integer :: isnap
    type (read_info) :: rinfo
    ! Internal
    integer :: ifile
    integer :: nmass
    integer(kind=int4byte), dimension(6) :: nptot4, npfile4, nptot_hw4
    integer(kind=int8byte), dimension(6) :: nptot, npfile, nptot_hw
    real(kind=real8byte), dimension(6) :: massarr
    real(kind=real8byte) :: time, redshift, boxsize
    integer(kind=int4byte) :: flag_sfr, flag_feedback, flag_cooling, numfiles
    character(len=fname_maxlen) :: fname
    integer(kind=int4byte), dimension(8) :: dummy1
    integer(kind=int4byte), dimension(16) :: dummy2
    integer(kind=int4byte) :: irec, jrec
    integer :: ios, i, j, istat
    integer(kind=index_kind) :: ipart
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
    ! SPH properties
    real(kind=real4byte),    dimension(:),   allocatable :: rho4, u4
    real(kind=real8byte),    dimension(:),   allocatable :: rho8, u8
    ! Particle type names
    character(len=50), dimension(6) :: species_name
    ! Result from function calls
    type (result_type) :: res
    ! Progress bar
    integer(kind=int8byte) :: prog_tot, prog_so_far
    logical :: read_marker
    ! Number of bytes used to store one value (position,velocity,id etc)
    integer :: byte_size
    ! Whether there's a density block
    logical :: have_density
    ! Range of files to read
    integer :: firstfile, lastfile
    ! Which particles to read
    logical, dimension(:), allocatable :: mask
    ! Random float
    real    :: rnd
    ! Number of particles to keep after sampling
    integer(kind=index_kind) :: nkeep, offset

    gadget_binary_read%success = .false.
    have_density = .true.

    call progress_bar_update(0.0)

    ! Read one file to get the total particle number
    call gadget_path_generate(isnap, 0, fname, path_data)

    call open_binary(fname,ios)
    if(ios.ne.0)then
       gadget_binary_read%string="Unable to open file: "//trim(fname)
       return
    endif

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
    call read_binary(nptot_hw4,     ios)
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
       gadget_binary_read%string="Unable to read from file: "//trim(fname)
       call close_binary()
       return
    endif

    ! At this point we know the file is readable, so deallocate the old
    ! snapshot
    call particle_store_empty(pdata)
    call particle_store_empty(psample)

    if(need_byteswap)then
       call byteswap(massarr)
       call byteswap(time)
       call byteswap(redshift)
       call byteswap(flag_sfr)
       call byteswap(flag_feedback)
       call byteswap(numfiles)
    endif
    call close_binary()

    ! This snapshot may have a different number of files
    nfiles = max(1,numfiles)

    ! In single file snapshots nptot may not be set
    if(nfiles.eq.1)nptot=npfile

    ! Now we know how many particles, allocate storage
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
       ! May not know in advance how many particles we'll have if sampling
       if(rinfo%do_sampling.or.rinfo%do_sphere.or.rinfo%just_this_file)then
          res = particle_store_new_species(pdata,species_name(i))
       else
          res = particle_store_new_species(pdata,species_name(i),int(nptot(i),index_kind))
       endif
       ! Abort if memory allocation fails
       if(.not.res%success)then
          gadget_binary_read = res ! Pass back return status
          call particle_store_empty(pdata) ! Clean up any memory allocated
          return
       endif
    end do

    ! If there are SPH particles, allocate storage for density and internal
    ! energy
    if(nptot(1).gt.0)then
       res = particle_store_new_property(pdata,species_name(1), &
            "InternalEnergy","REAL")
       if(.not.res%success)then
          gadget_binary_read = res
          call particle_store_empty(pdata)
          return
       endif
       res = particle_store_new_property(pdata,species_name(1),"Density", &
            "REAL")
       if(.not.res%success)then
          gadget_binary_read = res
          call particle_store_empty(pdata)
          return
       endif
    endif

    ! Allocate storage for masses and IDs
    do i = 1, 6, 1

       res = particle_store_new_property(pdata,species_name(i),"Mass", &
            "REAL", is_mass=.true.)
       if(.not.res%success)then
          gadget_binary_read = res
          call particle_store_empty(pdata)
          return
       endif
       res = particle_store_new_property(pdata,species_name(i),"ID", &
            "INTEGER", is_id=.true.)
       if(.not.res%success)then
          gadget_binary_read = res
          call particle_store_empty(pdata)
          return
       endif
    end do

    ! Work out values for progress bar
    prog_tot    = sum(nptot(1:6))*4
    prog_tot = prog_tot + 2*nptot(1)
    prog_so_far = 0

    ! Loop over files and read them
    if(rinfo%just_this_file)then
       firstfile = path_data%fileno
       lastfile  = path_data%fileno
    else
       firstfile = 0
       lastfile = nfiles-1
    endif

    do ifile = firstfile, lastfile, 1

       ! Generate filename
       call gadget_path_generate(isnap, ifile, fname, &
            path_data)

       ! Open file
       call open_binary(fname, ios)
       if(ios.ne.0)then
          gadget_binary_read%string="Unable to open file: "//trim(fname)
          call particle_store_empty(pdata)
          return
       endif
       
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
          gadget_binary_read%string="Unable to read from file: "//trim(fname)
          call particle_store_empty(pdata)
          call close_binary()
          return
       endif

       if(need_byteswap)then
          call byteswap(massarr)
          call byteswap(time)
          call byteswap(redshift)
          call byteswap(flag_sfr)
          call byteswap(flag_feedback)
       endif

       ! In single file snapshots nptot may not be set
       if(nfiles.eq.1)nptot=npfile

       ! If no particles, go to next file
       if(sum(npfile).eq.0)then
          call close_binary()
          cycle
       endif

       ! Set up mask array for sampling
       if(rinfo%do_sampling.or.rinfo%do_sphere)then
          allocate(mask(sum(npfile)))
          mask = .true.
          if(rinfo%do_sampling)then
             do ipart = 1, sum(npfile), 1
                call random_number(rnd)
                if(rnd.gt.rinfo%sample_rate)mask(ipart)=.false.
             end do
          endif
       endif

       ! Read record marker
       call read_binary(irec, ios)
       if(need_byteswap)call byteswap(irec)
       if(ios.ne.0)then
          call particle_store_empty(pdata)
          gadget_binary_read%string = "Unable to read from file "//trim(fname)
          call close_binary()
          return
       endif

       ! Use record marker to determine byte size for positions
       byte_size = irec / (3*sum(npfile))
       if(byte_size.ne.4.and.byte_size.ne.8)then
          call particle_store_empty(pdata)
          gadget_binary_read%string = &
               "Length of particle coordinate record doesn't make sense"
          return
       endif

       ! Read positions
       do i = 1, 6, 1
          if(npfile(i).gt.0)then
             
             ! Allocate temporary buffer
             select case(byte_size)
             case(4)
                allocate(pos4(3,npfile(i)),stat=istat)
             case(8)
                allocate(pos8(3,npfile(i)),stat=istat)
             end select
             if(istat.gt.0)then
                call particle_store_empty(pdata)
                gadget_binary_read%string = "Unable to allocate pos buffer"
                call close_binary()
                return
             endif
             ! Read the data
             select case(byte_size)
             case(4)
                call read_binary(pos4, ios)
             case(8)
                call read_binary(pos8, ios)
             end select
             if(ios.gt.0)then
                call particle_store_empty(pdata)
                select case(byte_size)
                case(4)
                   deallocate(pos4)
                case(8)
                   deallocate(pos8)
                end select
                gadget_binary_read%string = &
                     "Unable to read from file: "//trim(fname)
                call close_binary()
                return
             endif
             ! Make sure the data has the right endian-ness
             select case(byte_size)
             case(4)
                if(need_byteswap)call byteswap(pos4)
             case(8)
                if(need_byteswap)call byteswap(pos8)
             end select
             ! Set mask=false for particles outside the selected region
             if(rinfo%do_sphere)then
                offset = sum(npfile(1:i-1))
                select case(byte_size)
                case(4)
                   do j = 1, npfile(i), 1
                      if(any(abs(pos4(:,j)-rinfo%pos).gt.rinfo%radius)) &
                           mask(offset+j) = .false.
                   end do
                case(8)
                   do j = 1, npfile(i), 1
                      if(any(abs(pos8(:,j)-rinfo%pos).gt.rinfo%radius)) &
                           mask(offset+j) = .false.
                   end do
                end select
             endif
             ! Store the data we want to keep
             offset = sum(npfile(1:i-1))
             select case(byte_size)
             case(4)
                if(allocated(mask))then
                   nkeep = shrink_array(pos4(1,:), mask(offset+1:))
                   nkeep = shrink_array(pos4(2,:), mask(offset+1:))
                   nkeep = shrink_array(pos4(3,:), mask(offset+1:))
                   res = particle_store_add_data(pdata, species_name(i), &
                        pos=real(pos4(:,1:nkeep),pos_kind))
                   if(.not.res%success)then
                      gadget_binary_read = res
                      call close_binary()
                      return
                   endif
                else
                   res = particle_store_add_data(pdata, species_name(i), &
                        pos=real(pos4,pos_kind))
                   if(.not.res%success)then
                      gadget_binary_read = res
                      call close_binary()
                      return
                   endif
                endif
                deallocate(pos4)
             case(8)
                if(allocated(mask))then
                   nkeep = shrink_array(pos8(1,:), mask(offset+1:))
                   nkeep = shrink_array(pos8(2,:), mask(offset+1:))
                   nkeep = shrink_array(pos8(3,:), mask(offset+1:))
                   res = particle_store_add_data(pdata, species_name(i), &
                        pos=real(pos8(:,1:nkeep),pos_kind))
                   if(.not.res%success)then
                      gadget_binary_read = res
                      call close_binary()
                      return
                   endif
                else
                   res = particle_store_add_data(pdata, species_name(i), &
                        pos=real(pos8,pos_kind))
                   if(.not.res%success)then
                      gadget_binary_read = res
                      call close_binary()
                      return
                   endif
                endif
                deallocate(pos8)
             end select
          endif
       end do

       prog_so_far = prog_so_far + sum(npfile)
       call progress_bar_update(real(prog_so_far)/real(prog_tot))

       ! Skip record markers
       call read_binary(jrec, ios)
       call read_binary(irec, ios)
       if(need_byteswap)call byteswap(irec)
       if(ios.ne.0)then
          call particle_store_empty(pdata)
          gadget_binary_read%string = "Unable to read from file "//trim(fname)
          call close_binary()
          return
       endif

       ! Use record marker to determine byte size for velocities
       byte_size = irec / (3*sum(npfile))
       if(byte_size.ne.4.and.byte_size.ne.8)then
          call particle_store_empty(pdata)
          gadget_binary_read%string = &
               "Length of particle velocity record doesn't make sense"
          call close_binary()
          return
       endif

       ! Read velocities
       do i = 1, 6, 1
          if(npfile(i).gt.0)then
#ifdef READ_VEL
             ! Allocate temporary buffer
             select case(byte_size)
             case(4)
                allocate(vel4(3,npfile(i)),stat=istat)
             case(8)
                allocate(vel8(3,npfile(i)),stat=istat)
             end select
             if(istat.gt.0)then
                call particle_store_empty(pdata)
                gadget_binary_read%string = "Unable to allocate vel buffer"
                call close_binary()
                return
             endif
             ! Read the data
             select case(byte_size)
             case(4)
                call read_binary(vel4, ios)
                if(need_byteswap)call byteswap(vel4)
             case(8)
                call read_binary(vel8, ios)
                if(need_byteswap)call byteswap(vel8)
             end select
#else
             call skip_bytes(int(npfile(i)*3*byte_size), ios)
#endif
             if(ios.gt.0)then
                call particle_store_empty(pdata)
#ifdef READ_VEL
                select case(byte_size)
                case(4)
                   deallocate(vel4)
                case(8)
                   deallocate(vel8)
                end select
#endif
                gadget_binary_read%string = &
                     "Unable to read from file [vel]: "//trim(fname)
                call close_binary()
                return
             endif
#ifdef READ_VEL
             offset = sum(npfile(1:i-1))
             select case(byte_size)
             case(4)
                if(allocated(mask))then
                   nkeep = shrink_array(vel4(1,:), mask(offset+1:))
                   nkeep = shrink_array(vel4(2,:), mask(offset+1:))
                   nkeep = shrink_array(vel4(3,:), mask(offset+1:))
                   res = particle_store_add_data(pdata, species_name(i), &
                        vel=real(vel4(:,1:nkeep),vel_kind))
                else
                   res = particle_store_add_data(pdata, species_name(i), &
                        vel=real(vel4,vel_kind))
                endif
                deallocate(vel4)
             case(8)
                if(allocated(mask))then
                   nkeep = shrink_array(vel8(1,:), mask(offset+1:))
                   nkeep = shrink_array(vel8(2,:), mask(offset+1:))
                   nkeep = shrink_array(vel8(3,:), mask(offset+1:))
                   res = call particle_store_add_data(pdata, species_name(i), &
                        vel=real(vel8(:,1:nkeep),vel_kind))
                else
                   res =  particle_store_add_data(pdata, species_name(i), &
                        vel=real(vel8,vel_kind))
                endif
                deallocate(vel8)
             end select
             if(.not.res%success)then
                gadget_binary_read = res
                call close_binary()
                return
             endif
#endif
          endif
       end do

       prog_so_far = prog_so_far + sum(npfile)
       call progress_bar_update(real(prog_so_far)/real(prog_tot))

       ! Skip record markers
       call read_binary(jrec, ios)
       call read_binary(irec, ios)
       if(need_byteswap)call byteswap(irec)
       if(ios.ne.0)then
          call particle_store_empty(pdata)
          gadget_binary_read%string = "Unable to read from file [rec1]: "&
               //trim(fname)
          call close_binary()
          return
       endif

       ! Use record marker to determine byte size for IDs
       byte_size = irec / (1*sum(npfile))
       if(byte_size.ne.4.and.byte_size.ne.8)then
          call particle_store_empty(pdata)
          gadget_binary_read%string = &
               "Length of particle ID record doesn't make sense"
          call close_binary()
          return
       endif

       ! Read IDs
       do i = 1, 6, 1
          if(npfile(i).gt.0)then
             ! Allocate temporary buffer
             select case(byte_size)
             case(4)
                allocate(id4(npfile(i)),stat=istat)
                call particle_store_set_idsize(pdata, 4)
             case(8)
                allocate(id8(npfile(i)),stat=istat)
                call particle_store_set_idsize(pdata, 8)
             end select
             if(istat.gt.0)then
                call particle_store_empty(pdata)
                gadget_binary_read%string = "Unable to allocate id buffer"
                call close_binary()
                return
             endif
             ! Read the data
             select case(byte_size)
             case(4)
                call read_binary(id4, ios)
                if(need_byteswap)call byteswap(id4)
             case(8)
                call read_binary(id8, ios)
                if(need_byteswap)call byteswap(id8)
             end select
             if(ios.gt.0)then
                call particle_store_empty(pdata)
                select case(byte_size)
                case(4)
                   deallocate(id4)
                case(8)
                   deallocate(id8)
                end select
                gadget_binary_read%string = &
                     "Unable to read from file [ids]: "//trim(fname)
                call close_binary()
                return
             endif
             offset = sum(npfile(1:i-1))
             select case(byte_size)
             case(4)
                if(allocated(mask))then
                   nkeep = shrink_array(id4, mask(offset+1:))
                   res = particle_store_add_data(pdata, species_name(i), &
                        prop_name="ID", idata=int(id4(1:nkeep),i_prop_kind))
                else
                   res = particle_store_add_data(pdata, species_name(i), &
                        prop_name="ID", idata=int(id4,i_prop_kind))
                endif
                deallocate(id4)
             case(8)
                if(allocated(mask))then
                   nkeep = shrink_array(id8, mask(offset+1:))
                   res = particle_store_add_data(pdata, species_name(i), &
                        prop_name="ID", idata=int(id8(1:nkeep),i_prop_kind))
                else
                   res = particle_store_add_data(pdata, species_name(i), &
                        prop_name="ID", idata=int(id8,i_prop_kind))
                endif
                deallocate(id8)
             end select
             if(.not.res%success)then
                gadget_binary_read = res
                call close_binary()
                return
             endif
          endif
       end do

       prog_so_far = prog_so_far + sum(npfile)
       call progress_bar_update(real(prog_so_far)/real(prog_tot))

       ! Determine how many masses there should be in the file
       nmass = 0
       do i = 1, 6, 1
          if(massarr(i).eq.0.0)nmass=nmass+npfile(i)
       end do

       ! Read masses
       read_marker = .false.
       do i = 1, 6, 1
          if(npfile(i).gt.0)then
             if(massarr(i).eq.0.0)then
                if(.not.read_marker)then
                   ! Skip record markers
                   call read_binary(jrec, ios)
                   call read_binary(irec, ios)
                   if(need_byteswap)call byteswap(irec)
                   if(ios.ne.0)then
                      call particle_store_empty(pdata)
                      gadget_binary_read%string = &
                           "Unable to read from file [rec3]"// &
                        trim(fname)
                      call close_binary()
                      return
                   endif
                   read_marker = .true.
                   ! Determine number of bytes used to store a mass
                   byte_size = irec / nmass
                   if(byte_size.ne.4.and.byte_size.ne.8)then
                      call particle_store_empty(pdata)
                      gadget_binary_read%string = &
                           "Length of particle mass record doesn't make sense"
                      call close_binary()
                      return
                   endif
                endif
                ! Allocate temporary buffer
                select case(byte_size)
                case(4)
                   allocate(mass4(npfile(i)),stat=istat)
                case(8)
                   allocate(mass8(npfile(i)),stat=istat)
                end select
                if(istat.gt.0)then
                   call particle_store_empty(pdata)
                   gadget_binary_read%string = &
                        "Unable to allocate mass buffer"
                   call close_binary()
                   return
                endif
                select case(byte_size)
                case(4)
                   call read_binary(mass4, ios)
                   if(need_byteswap)call byteswap(mass4)
                case(8)
                   call read_binary(mass8, ios)
                   if(need_byteswap)call byteswap(mass8)
                end select
                if(ios.gt.0)then
                   call particle_store_empty(pdata)
                   select case(byte_size)
                   case(4)
                      deallocate(mass4)
                   case(8)
                      deallocate(mass8)
                   end select
                   gadget_binary_read%string = &
                        "Unable to read from file [mass]: "//trim(fname)
                   call close_binary()
                   return
                endif
                offset = sum(npfile(1:i-1))
                select case(byte_size)
                case(4)
                   if(allocated(mask))then
                      nkeep = shrink_array(mass4, mask(offset+1:))
                      res = particle_store_add_data(pdata, species_name(i), &
                           prop_name="Mass", rdata=real(mass4(1:nkeep),r_prop_kind))
                   else
                      res = particle_store_add_data(pdata, species_name(i), &
                           prop_name="Mass", rdata=real(mass4,r_prop_kind))
                   endif
                   deallocate(mass4)
                case(8)
                   if(allocated(mask))then
                      nkeep = shrink_array(mass8, mask(offset+1:))
                      res = particle_store_add_data(pdata, species_name(i), &
                           prop_name="Mass", rdata=real(mass8(1:nkeep),r_prop_kind))
                   else
                      res = particle_store_add_data(pdata, species_name(i), &
                           prop_name="Mass", rdata=real(mass8,r_prop_kind))
                   endif
                   deallocate(mass8)
                end select
                if(.not.res%success)then
                   gadget_binary_read = res
                   call close_binary()
                   return
                endif
             else
                allocate(mass4(1:npfile(i)))
                mass4(1:npfile(i)) = massarr(i)
                offset = sum(npfile(1:i-1))
                if(allocated(mask))then
                   nkeep = shrink_array(mass4, mask(offset+1:))
                   res = particle_store_add_data(pdata, species_name(i), &
                        prop_name="Mass", rdata=real(mass4(1:nkeep),r_prop_kind))
                else
                   res = particle_store_add_data(pdata, species_name(i), &
                        prop_name="Mass", rdata=real(mass4,r_prop_kind))
                endif
                deallocate(mass4)
                if(.not.res%success)then
                   gadget_binary_read = res
                   call close_binary()
                   return
                endif
             endif
          endif
       end do

       prog_so_far = prog_so_far + sum(npfile)
       call progress_bar_update(real(prog_so_far)/real(prog_tot))

       ! Read internal energies and densities if we have SPH particles
       if(npfile(1).gt.0)then
          
          ! First read the internal energy
          ! Skip record markers
          call read_binary(jrec, ios)
          call read_binary(irec, ios)
          if(need_byteswap)call byteswap(irec)
          if(ios.ne.0)then
             call particle_store_empty(pdata)
             gadget_binary_read%string = "Unable to read from file "// &
                  trim(fname)
             call close_binary()
             return
          endif

          ! Determine number of bytes used to store an internal energy
          byte_size = irec / npfile(1)
          if(byte_size.ne.4.and.byte_size.ne.8)then
             call particle_store_empty(pdata)
             gadget_binary_read%string = &
                  "Length of internal energy record doesn't make sense"
             call close_binary()
             return
          endif

          ! Allocate read buffer for internal energy
          select case(byte_size)
          case(4)
             allocate(u4(npfile(1)), stat=istat)
          case(8)
             allocate(u8(npfile(1)), stat=istat)
          end select
          if(istat.ne.0)then
             call particle_store_empty(pdata)
             gadget_binary_read%string = "Unable to allocate memory for gas"
             call close_binary()
             return
          endif
          
          ! Read the data
          select case(byte_size)
          case(4)
             call read_binary(u4, ios)
             if(need_byteswap)call byteswap(u4)
          case(8)
             call read_binary(u8, ios)
             if(need_byteswap)call byteswap(u8)
          end select

          prog_so_far = prog_so_far + npfile(1)
          call progress_bar_update(real(prog_so_far)/real(prog_tot))
          if(ios.ne.0)then
             select case(byte_size)
             case(4)
                deallocate(u4)
             case(8)
                deallocate(u8)
             end select
             call particle_store_empty(pdata)
             gadget_binary_read%string = "Unable to read from file:"// &
                  trim(fname)
             call close_binary()
             return
          endif
          offset = 0
          select case(byte_size)
          case(4)
             if(allocated(mask))then
                nkeep = shrink_array(u4, mask(offset+1:))
                res = particle_store_add_data(pdata, species_name(1), &
                     prop_name="InternalEnergy", rdata=real(u4(1:nkeep),r_prop_kind))
             else
                res = particle_store_add_data(pdata, species_name(1), &
                     prop_name="InternalEnergy", rdata=real(u4,r_prop_kind))
             endif
             deallocate(u4)
          case(8)
             if(allocated(mask))then
                nkeep = shrink_array(u8, mask(offset+1:))
                res = particle_store_add_data(pdata, species_name(1), &
                     prop_name="InternalEnergy", rdata=real(u8(1:nkeep),r_prop_kind))
             else
                res = particle_store_add_data(pdata, species_name(1), &
                     prop_name="InternalEnergy", rdata=real(u8,r_prop_kind))
             endif
             deallocate(u8)
          end select
          if(.not.res%success)then
             gadget_binary_read = res
             call close_binary()
             return
          endif

          ! Then do the densities
          ! Skip record markers
          call read_binary(jrec, ios)
          call read_binary(irec, ios)
          if(need_byteswap)call byteswap(irec)

          ! If the read failed this is probably an initial conditions file,
          ! which doesn't have a density block. In this case we'll just
          ! set the densities to zero.
          if(ios.ne.0.and.ifile.eq.firstfile)then
             have_density = .false.
             ios = 0
             call summary_add_line(s,"Initial conditions file")
          endif

          ! If the first file had a density block they should all have one
          if(ios.ne.0.and.have_density)then
             call particle_store_empty(pdata)
             gadget_binary_read%string = "Unable to read from file "// &
                  trim(fname)
             call close_binary()
             return
          endif

          ! Determine number of bytes used to store a density
          if(have_density)then
             byte_size = irec / npfile(1)
             if(byte_size.ne.4.and.byte_size.ne.8)then
                call particle_store_empty(pdata)
                gadget_binary_read%string = &
                     "Length of density record doesn't make sense"
                call close_binary()
                return
             endif
          else
             byte_size = 4
          endif

          ! Allocate read buffer for density
          select case(byte_size)
          case(4)
             allocate(rho4(npfile(1)), stat=istat)
          case(8)
             allocate(rho8(npfile(1)), stat=istat)
          end select
          if(istat.ne.0)then
             call particle_store_empty(pdata)
             gadget_binary_read%string = "Unable to allocate memory for gas"
             call close_binary()
             return
          endif
          
          ! Read the data
          if(have_density)then
             select case(byte_size)
             case(4)
                call read_binary(rho4, ios)
                if(need_byteswap)call byteswap(rho4)
             case(8)
                call read_binary(rho8, ios)
                if(need_byteswap)call byteswap(rho8)
             end select
          else
             rho4 = 0.0
          endif

          prog_so_far = prog_so_far + npfile(1)
          call progress_bar_update(real(prog_so_far)/real(prog_tot))
          if(ios.ne.0)then
             select case(byte_size)
             case(4)
                deallocate(rho4)
             case(8)
                deallocate(rho8)
             end select
             call particle_store_empty(pdata)
             gadget_binary_read%string = "Unable to read from file:"// &
                  trim(fname)
             call close_binary()
             return
          endif
          offset = 0
          select case(byte_size)
          case(4)
             if(allocated(mask))then
                nkeep = shrink_array(rho4, mask(offset+1:))
                res = particle_store_add_data(pdata, species_name(1), &
                     prop_name="Density", rdata=real(rho4(1:nkeep),r_prop_kind))
             else
                res = particle_store_add_data(pdata, species_name(1), &
                     prop_name="Density", rdata=real(rho4,r_prop_kind))
             endif
             deallocate(rho4)
          case(8)
             if(allocated(mask))then
                nkeep = shrink_array(rho8, mask(offset+1:))
                res = particle_store_add_data(pdata, species_name(1), &
                     prop_name="Density", rdata=real(rho8(1:nkeep),r_prop_kind))
             else
                res = particle_store_add_data(pdata, species_name(1), &
                     prop_name="Density", rdata=real(rho8,r_prop_kind))
             endif
             deallocate(rho8)
          end select
          if(.not.res%success)then
             gadget_binary_read = res
             call close_binary()
             return
          endif
       endif

       ! Close this file
       call close_binary()

       if(rinfo%do_sampling.or.rinfo%do_sphere)then
          deallocate(mask)
       endif

       ! Next file
    end do

    ! Record the redshift etc
    call particle_store_set_time(pdata, real(time), real(redshift), real(time))
    call particle_store_set_boxsize(pdata, real(boxsize))

    gadget_binary_read%success = .true.

    if(need_byteswap)then
       call summary_add_line(s,"Byteswapping required")
    else
       call summary_add_line(s,"Byteswapping not required")
    endif
    call summary_add_line(s,&
         "Number of files = "//trim(adjustl(string(nfiles))))

    return
  end function gadget_binary_read


end module gadget_binary_reader
