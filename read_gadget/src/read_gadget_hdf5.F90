module read_gadget_hdf5

#include "../../config.h"

  use f90_util
#ifdef HAVE_HDF5
  use read_hdf5
#endif
  use read_gadget_definitions

  implicit none
  private
  save

  ! Routines in this module
  public :: hdf5_snapshot_file_open
  public :: hdf5_snapshot_file_close
  public :: hdf5_snapshot_file_get_datasets
  public :: hdf5_snapshot_file_read_header
  public :: hdf5_snapshot_file_read_dataset

  ! Number of particles in the current file
  integer(kind=int4byte), dimension(0:5) :: npfile

contains


  subroutine hdf5_snapshot_file_open(fname, iostat)
!
! Open a new snapshot file
!
    implicit none
    character(len=*), intent(in)  :: fname
    integer,          intent(out) :: iostat

#ifdef HAVE_HDF5
    iostat = hdf5_open_file(fname)
    if(iostat.ne.0)return

    ! Get number of particles in the file
    iostat = hdf5_read_attribute("Header/NumPart_ThisFile", npfile)
    if(iostat.ne.0)then
       iostat = hdf5_close_file()
       iostat = -1
       return
    endif
#else
    iostat = -10
#endif

    return
  end subroutine hdf5_snapshot_file_open


  subroutine hdf5_snapshot_file_close(iostat)
!
! Close a snapshot file
!
    implicit none
    integer,          intent(out) :: iostat

#ifdef HAVE_HDF5
    iostat = hdf5_close_file()
#else
    iostat = -10
#endif

    return
  end subroutine hdf5_snapshot_file_close


  subroutine hdf5_snapshot_file_get_datasets(itype, ndataset, dataset_name, iostat)
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
    ! Internal
    integer :: n, i, j
    character(len=dset_name_length), dimension(:), allocatable :: tmp
    integer :: type
    character(len=dset_name_length) :: fullname
    integer :: rank
    integer, dimension(7) :: dims

#ifdef HAVE_HDF5
    iostat = -1

    ! Construct group name
    write(group_name,"('/PartType',i1.1)")itype

    ! Count datasets
    if(hdf5_find_datasets(group_name, n).ne.0)return

    ! Get dataset names
    allocate(tmp(n))
    if(hdf5_find_datasets(group_name, n, tmp).ne.0)then
       deallocate(tmp)
       return
    endif

    ! Check sizes and types of datasets
    j = 0
    do i = 1, n, 1

       ! Generate full name of the dataset
       write(fullname, "('/PartType',i1.1,'/')")itype
       fullname = trim(fullname)//trim(tmp(i))

       ! Get type
       if(hdf5_dataset_type(fullname, type).ne.0)then
          deallocate(tmp)
          return
       endif
       
       ! Get dimensions
       if(hdf5_dataset_size(fullname, rank, dims).ne.0)then
          deallocate(tmp)
          return
       endif

       ! Check if we can read this dataset - needs to be 1D N elements
       ! or 2D 3xN elements of 4 or 8 byte reals or ints. 
       if(type.ne.HDF5_UNKNOWN.and.rank.le.2.and.rank.ge.1)then
          ! Need one scalar or vector per particle 
          if(dims(rank).eq.npfile(itype))then
             if(rank.eq.1.or.(rank.eq.2.and.dims(1).eq.3))then
                ! Can read this one
                j = j + 1
                tmp(j) = tmp(i)
             endif
          endif
       endif
    end do

    ! Store the results
    ndataset = j
    allocate(dataset_name(ndataset))
    dataset_name(1:ndataset) = tmp(1:ndataset)
    deallocate(tmp)

    iostat = 0
#else
    iostat = -10
#endif

    return
  end subroutine hdf5_snapshot_file_get_datasets


  subroutine hdf5_snapshot_file_read_header(header, iostat)
!
! Read the header from a snapshot file
!
    implicit none
    type (snapshot_header_type) :: header
    integer,          intent(out) :: iostat

#ifdef HAVE_HDF5
    ! Read header elements we need
    iostat = -1
    ! Particle numbers
    if(hdf5_read_attribute("Header/NumPart_Total",    header%NumPart_Total).ne.0)return
    if(hdf5_read_attribute("Header/NumPart_ThisFile", header%NumPart_ThisFile).ne.0)return
    ! This field may not be present, in which case we assume its zero
    if(hdf5_read_attribute("Header/NumPart_Total_HighWord", header%NumPart_Total_HighWord).ne.0) &
         header%NumPart_Total_HighWord = 0
    ! Box size
    if(hdf5_read_attribute("Header/BoxSize",   header%BoxSize).ne.0)return
    ! Fixed particle masses
    if(hdf5_read_attribute("Header/MassTable", header%MassTable).ne.0)return
    ! Number of files in this snapshot
    if(hdf5_read_attribute("Header/NumFilesPerSnapshot", header%NumFilesPerSnapshot).ne.0)return
    ! Expansion factor - may be labelled as "Time"
    if(hdf5_read_attribute("Header/ExpansionFactor", header%ExpansionFactor).ne.0)then
       ! No ExpansionFactor so try Time
       if(hdf5_read_attribute("Header/Time", header%ExpansionFactor).ne.0)return
    endif
    ! Redshift
    if(hdf5_read_attribute("Header/Redshift", header%Redshift).ne.0)return

    ! Success
    iostat = 0
#else
    iostat = -10
#endif

    return
  end subroutine hdf5_snapshot_file_read_header


  subroutine hdf5_snapshot_file_read_dataset(itype, dataset_name, data, &
       iostat)
!
! Read a dataset
!
    implicit none
    integer,             intent(in)  :: itype
    character(len=*),    intent(in)  :: dataset_name
    type (dataset_type), intent(out) :: data
    integer,             intent(out) :: iostat
    character(len=dset_name_length)  :: fullname
    integer                          :: dtype
    integer, dimension(7)            :: dims
    integer                          :: rank

#ifdef HAVE_HDF5
    ! Generate full name of the dataset
    write(fullname, "('/PartType',i1.1,'/')")itype
    fullname = trim(fullname)//trim(dataset_name)

    ! Get type of the dataset
    iostat = hdf5_dataset_type(fullname, dtype)
    if(iostat.ne.0)return

    ! Get dimensions of the dataset
    iostat = hdf5_dataset_size(fullname, rank, dims)
    if(iostat.ne.0)return

    ! Check dimensions and type are ok
    if(dtype.eq.HDF5_UNKNOWN.or.(rank.lt.1.or.rank.gt.2))then
       iostat = -1
       return
    endif
    if(rank.eq.2.and.dims(1).ne.3)then
       iostat = -1
       return
    end if

    ! Read the dataset
    call dataset_init(data)
    select case(dtype)
    case(HDF5_INTEGER4)
       if(rank.eq.1)then
          allocate(data%i4_1d(dims(1)))
          iostat = hdf5_read_dataset(fullname, data%i4_1d)
       else
          allocate(data%i4_2d(dims(1), dims(2)))
          iostat = hdf5_read_dataset(fullname, data%i4_2d)
       endif
    case(HDF5_INTEGER8)
       if(rank.eq.1)then
          allocate(data%i8_1d(dims(1)))
          iostat = hdf5_read_dataset(fullname, data%i8_1d)
       else
          allocate(data%i8_2d(dims(1), dims(2)))
          iostat = hdf5_read_dataset(fullname, data%i8_2d)
       endif
    case(HDF5_REAL4)
       if(rank.eq.1)then
          allocate(data%r4_1d(dims(1)))
          iostat = hdf5_read_dataset(fullname, data%r4_1d)
       else
          allocate(data%r4_2d(dims(1), dims(2)))
          iostat = hdf5_read_dataset(fullname, data%r4_2d)
       endif
    case(HDF5_REAL8)
       if(rank.eq.1)then
          allocate(data%r8_1d(dims(1)))
          iostat = hdf5_read_dataset(fullname, data%r8_1d)
       else
          allocate(data%r8_2d(dims(1), dims(2)))
          iostat = hdf5_read_dataset(fullname, data%r8_2d)
       endif
    end select
    if(iostat.ne.0)call dataset_dealloc(data)
#else
    iostat = -10
#endif

    return
  end subroutine hdf5_snapshot_file_read_dataset

end module read_gadget_hdf5
