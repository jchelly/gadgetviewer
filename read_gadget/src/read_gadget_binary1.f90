module read_gadget_binary1

  use f90_util
  use read_gadget_definitions
  use byteswapper
  
  implicit none
  private
  save

  ! Routines in this module
  public :: binary1_snapshot_file_open
  public :: binary1_snapshot_file_close
  public :: binary1_snapshot_file_get_datasets
  public :: binary1_snapshot_file_read_header
  public :: binary1_snapshot_file_read_dataset

  ! Number of particles in the current file
  integer(kind=int4byte), dimension(0:5) :: npfile

  ! Whether we need to byteswap this file
  logical :: need_byteswap

  ! Mass of each particle type
  real(kind=real8byte), dimension(0:5) :: massarr

  ! Precision of datasets
  integer :: pos_size
  integer :: vel_size
  integer :: ids_size
  integer :: mass_size
  integer :: u_size
  integer :: rho_size
  integer :: hsml_size

  ! Whether extra gas properties are present
  logical :: have_u
  logical :: have_rho
  logical :: have_hsml

  ! Locations of datasets in the file
  integer :: pos_offset
  integer :: vel_offset
  integer :: ids_offset
  integer :: mass_offset
  integer :: u_offset
  integer :: rho_offset
  integer :: hsml_offset

contains

  subroutine binary1_snapshot_file_open(fname, iostat)
!
! Open a new snapshot file
!
    implicit none
    character(len=*), intent(in)  :: fname
    integer,          intent(out) :: iostat
    integer(kind=int4byte)        :: dummy1, dummy2
    integer(kind=int4byte)        :: nskip
    integer :: nmass, i

    ! Attempt to open the file
    call try_open()

    ! If something went wrong, clean up
    if(iostat.ne.0)then
       call close_binary()
    endif

    return

  contains
    
    subroutine try_open()

      implicit none

      ! Open the file and check that it looks like a Gadget snapshot
      call open_binary(fname, iostat)
      if(iostat.ne.0)return
      call read_binary(dummy1, iostat, pos=0)
      if(iostat.ne.0)return    
      call read_binary(dummy2, iostat, pos=260)
      if(iostat.ne.0)return

      ! Record markers at start and end of header should be the same
      if(dummy1.ne.dummy2)then
         iostat = -1
         return
      endif

      ! Record markers should be 256 or 65536
      if(dummy1.eq.256)then
         ! Native endian Gadget file
         need_byteswap = .false.
      else if(dummy1.eq.65536)then
         ! Wrong endian Gadget file
         need_byteswap = .true.
      else
         ! Its not a Gadget type 1 binary file
         iostat = -1
         return
      endif

      ! Get number of particles of each type
      call read_binary(npfile, iostat, pos=4)
      if(iostat.ne.0)return
      if(need_byteswap)call byteswap(npfile)

      ! Get particle masses
      call read_binary(massarr, iostat, pos=28)
      if(iostat.ne.0)return
      if(need_byteswap)call byteswap(massarr)

      nskip = 264 ! Size of header

      ! Determine precision of positions
      call read_binary(dummy1, iostat, pos=nskip)
      if(iostat.ne.0)return
      pos_offset = nskip + 4
      if(need_byteswap)call byteswap(dummy1)
      pos_size = dummy1 / (3*sum(npfile))
      if(pos_size.ne.4.and.pos_size.ne.8)return
      nskip = nskip + 4 + 3*pos_size*sum(npfile) + 4

      ! Determine precision of velocities
      call read_binary(dummy1, iostat, pos=nskip)
      if(iostat.ne.0)return
      vel_offset = nskip + 4
      if(need_byteswap)call byteswap(dummy1)
      vel_size = dummy1 / (3*sum(npfile))
      if(vel_size.ne.4.and.vel_size.ne.8)return
      nskip = nskip + 4 + 3*vel_size*sum(npfile) + 4

      ! Determine precision of IDs
      call read_binary(dummy1, iostat, pos=nskip)
      if(iostat.ne.0)return
      ids_offset = nskip + 4
      if(need_byteswap)call byteswap(dummy1)
      ids_size = dummy1 / sum(npfile)
      if(ids_size.ne.4.and.ids_size.ne.8)return
      nskip = nskip + 4 + ids_size*sum(npfile) + 4

      ! Determine number of masses in the file
      nmass = 0
      do i = 0, 5, 1
         if(massarr(i).eq.0)nmass = nmass + npfile(i)
      end do

      ! Determine precision of masses
      call read_binary(dummy1, iostat, pos=nskip)
      if(iostat.ne.0)return
      mass_offset = nskip + 4
      if(need_byteswap)call byteswap(dummy1)
      mass_size = dummy1 / nmass
      if(mass_size.ne.4.and.mass_size.ne.8)return
      nskip = nskip + 4 + mass_size*nmass + 4

      ! The following datasets may not be present
      have_u    = .false.
      have_rho  = .false.
      have_hsml = .false.

      ! Check for internal energy
      call read_binary(dummy1, iostat, pos=nskip)
      if(iostat.ne.0)then
         iostat = 0
         return
      endif
      u_offset = nskip + 4
      if(need_byteswap)call byteswap(dummy1)
      u_size = dummy1 / npfile(0)
      if(u_size.ne.4.and.u_size.ne.8)then
         iostat = 0
         return
      endif
      nskip = nskip + 4 + u_size*npfile(0) + 4
      have_u = .true.

      ! Check for density
      call read_binary(dummy1, iostat, pos=nskip)
      if(iostat.ne.0)then
         iostat = 0
         return
      endif
      rho_offset = nskip + 4
      if(need_byteswap)call byteswap(dummy1)
      rho_size = dummy1 / npfile(0)
      if(rho_size.ne.4.and.rho_size.ne.8)then
         iostat = 0
         return
      endif
      nskip = nskip + 4 + rho_size*npfile(0) + 4
      have_rho = .true.

      ! Check for hsml
      call read_binary(dummy1, iostat, pos=nskip)
      if(iostat.ne.0)then
         iostat = 0
         return
      endif
      hsml_offset = nskip + 4
      if(need_byteswap)call byteswap(dummy1)
      hsml_size = dummy1 / npfile(0)
      if(hsml_size.ne.4.and.hsml_size.ne.8)then
         iostat = 0
         return
      endif
      nskip = nskip + 4 + hsml_size*npfile(0) + 4
      have_hsml = .true.

      iostat = 0

      return
    end subroutine try_open

  end subroutine binary1_snapshot_file_open


  subroutine binary1_snapshot_file_close(iostat)
!
! Close a snapshot file
!
    implicit none
    integer,          intent(out) :: iostat

    call close_binary()
    iostat = 0

    return
  end subroutine binary1_snapshot_file_close


  subroutine binary1_snapshot_file_get_datasets(itype, ndataset, dataset_name,&
       iostat)
!
! Return the number of readable datasets for a particle type and their names.
! Allocates dataset_name to the appropriate size.
!
    implicit none
    ! Parameters
    integer, intent(in)  :: itype
    integer, intent(out) :: ndataset, iostat
    character(len=*), dimension(:), pointer :: dataset_name
    character(len=dset_name_length) :: group_name
    
    select case(itype)
    case(0)
       ! Gas particles
       ndataset = 3
       if(massarr(itype).eq.0.0)ndataset = ndataset + 1
       if(have_u)ndataset    = ndataset + 1
       if(have_rho)ndataset  = ndataset + 1
       if(have_hsml)ndataset = ndataset + 1
       allocate(dataset_name(ndataset))
       dataset_name(1) = "Coordinates"
       dataset_name(2) = "Velocities"
       dataset_name(3) = "ParticleIDs"
       ndataset = 3
       if(massarr(itype).eq.0.0)then
          ndataset = ndataset + 1
          dataset_name(ndataset) = "Masses"
       endif
       if(have_u)then
          ndataset = ndataset + 1
          dataset_name(ndataset) = "InternalEnergy"
       endif
       if(have_rho)then
          ndataset = ndataset + 1
          dataset_name(ndataset) = "Density"
       endif
       if(have_hsml)then
          ndataset = ndataset + 1
          dataset_name(ndataset) = "SmoothingLength"
       endif
    case(1:5)
       ! Collisionless particles:
       ! These have pos, vel, ID and maybe mass
       if(massarr(itype).eq.0.0)then
          ndataset = 4
          allocate(dataset_name(ndataset))
          dataset_name(1) = "Coordinates"
          dataset_name(2) = "Velocities"
          dataset_name(3) = "ParticleIDs"
          dataset_name(4) = "Masses"
       else
          ndataset = 3
          allocate(dataset_name(ndataset))
          dataset_name(1) = "Coordinates"
          dataset_name(2) = "Velocities"
          dataset_name(3) = "ParticleIDs"
       endif
    end select

    iostat = 0
    return
  end subroutine binary1_snapshot_file_get_datasets


  subroutine binary1_snapshot_file_read_header(header, iostat)
!
! Read the header from a snapshot file
!
    implicit none
    type (snapshot_header_type) :: header
    integer,        intent(out) :: iostat
    integer, dimension(10) :: ijunk

    call read_binary(header%NumPart_ThisFile, iostat, pos=4)
    if(iostat.ne.0)return
    call read_binary(header%MassTable, iostat)
    if(iostat.ne.0)return
    call read_binary(header%ExpansionFactor, iostat)
    if(iostat.ne.0)return
    call read_binary(header%Redshift, iostat)
    if(iostat.ne.0)return
    call read_binary(ijunk(1:2), iostat)
    if(iostat.ne.0)return
    call read_binary(header%NumPart_Total, iostat)
    if(iostat.ne.0)return
    call read_binary(ijunk(1:1), iostat)
    if(iostat.ne.0)return
    call read_binary(header%NumFilesPerSnapshot, iostat)
    if(iostat.ne.0)return
    call read_binary(header%BoxSize, iostat)
    if(iostat.ne.0)return

    ! TODO: decide what to do about this !
    header%NumPart_Total_HighWord = 0

    if(need_byteswap)call byteswap_header(header)
    
    return
  end subroutine binary1_snapshot_file_read_header


  subroutine binary1_snapshot_file_read_dataset(itype, dataset_name, data, &
       iostat)
!
! Read a dataset
!
    implicit none
    integer,             intent(in)  :: itype
    character(len=*),    intent(in)  :: dataset_name
    type (dataset_type), intent(out) :: data
    integer,             intent(out) :: iostat
    integer :: nprev, nmass_prev, i

    call dataset_init(data)
    nprev = sum(npfile(0:itype-1))
    nmass_prev = 0
    do i = 0, itype-1, 1
       if(massarr(i).eq.0)nmass_prev = nmass_prev + npfile(i)
    end do

    select case(dataset_name)
    case("Coordinates")
       if(pos_size.eq.4)then
          allocate(data%r4_2d(3, npfile(itype)))
          call read_binary(data%r4_2d, iostat, &
               pos=pos_offset+nprev*pos_size*3)
       else
          allocate(data%r8_2d(3, npfile(itype)))
          call read_binary(data%r8_2d, iostat, &
               pos=pos_offset+nprev*pos_size*3)
       endif
    case("Velocities")
       if(vel_size.eq.4)then
          allocate(data%r4_2d(3, npfile(itype)))
          call read_binary(data%r4_2d, iostat, &
               pos=vel_offset+nprev*vel_size*3)
       else
          allocate(data%r8_2d(3, npfile(itype)))
          call read_binary(data%r8_2d, iostat, &
               pos=vel_offset+nprev*vel_size*3)
       endif
    case("ParticleIDs")
       if(ids_size.eq.4)then
          allocate(data%i4_1d(npfile(itype)))
          call read_binary(data%i4_1d, iostat, &
               pos=ids_offset+nprev*ids_size)
       else
          allocate(data%i8_1d(npfile(itype)))
          call read_binary(data%i8_1d, iostat, &
               pos=ids_offset+nprev*ids_size)
       endif
    case("Masses")
       ! Check that we have masses
       if(massarr(itype).ne.0.0)then
          iostat = -1
          return
       endif
       if(mass_size.eq.4)then
          allocate(data%r4_1d(npfile(itype)))
          call read_binary(data%r4_1d, iostat, &
               pos=mass_offset+mass_size*nmass_prev)
       else
          allocate(data%r8_1d(npfile(itype)))
          call read_binary(data%r8_1d, iostat, &
               pos=mass_offset+mass_size*nmass_prev)
       endif
    case("InternalEnergy")
       if(.not.have_u.or.itype.ne.0)then
          iostat = -1
          return
       end if
       if(u_size.eq.4)then
          allocate(data%r4_1d(npfile(itype)))
          call read_binary(data%r4_1d, iostat, &
               pos=u_offset+nprev*u_size)
       else
          allocate(data%r8_1d(npfile(itype)))
          call read_binary(data%r8_1d, iostat, &
               pos=u_offset+nprev*u_size)
       endif
    case("Density")
       if(.not.have_rho.or.itype.ne.0)then
          iostat = -1
          return
       end if
       if(rho_size.eq.4)then
          allocate(data%r4_1d(npfile(itype)))
          call read_binary(data%r4_1d, iostat, &
               pos=rho_offset+nprev*rho_size)
       else
          allocate(data%r8_1d(npfile(itype)))
          call read_binary(data%r8_1d, iostat, &
               pos=rho_offset+nprev*rho_size)
       endif
    case("SmoothingLength")
       if(.not.have_hsml.or.itype.ne.0)then
          iostat = -1
          return
       end if
       if(hsml_size.eq.4)then
          allocate(data%r4_1d(npfile(itype)))
          call read_binary(data%r4_1d, iostat, &
               pos=hsml_offset+nprev*hsml_size)
       else
          allocate(data%r8_1d(npfile(itype)))
          call read_binary(data%r8_1d, iostat, &
               pos=hsml_offset+nprev*hsml_size)
       endif
    case default
       ! Unrecognised name
       iostat = -1
    end select
    
    if(iostat.ne.0)then
       ! Read error
       call dataset_dealloc(data)
    else
       if(need_byteswap)call byteswap_dataset(data)
    endif

    return
  end subroutine binary1_snapshot_file_read_dataset

end module read_gadget_binary1
