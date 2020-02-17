module read_hdf5

#include "../../config.h" 

  use f90_util
  implicit none
  private

  integer, parameter, public :: HDF5_UNKNOWN  = -1
  integer, parameter, public :: HDF5_INTEGER4 = 0
  integer, parameter, public :: HDF5_INTEGER8 = 1
  integer, parameter, public :: HDF5_REAL4    = 2
  integer, parameter, public :: HDF5_REAL8    = 3

  interface hdf5_read_dataset
     module procedure hdf5_read_dataset_r4_1d
     module procedure hdf5_read_dataset_r8_1d
     module procedure hdf5_read_dataset_i4_1d
     module procedure hdf5_read_dataset_i8_1d
     module procedure hdf5_read_dataset_r4_2d
     module procedure hdf5_read_dataset_r8_2d
     module procedure hdf5_read_dataset_i4_2d
     module procedure hdf5_read_dataset_i8_2d
  end interface

  interface hdf5_read_attribute
     module procedure hdf5_read_attribute_r4
     module procedure hdf5_read_attribute_r8
     module procedure hdf5_read_attribute_i4
     module procedure hdf5_read_attribute_i8
     module procedure hdf5_read_attribute_r4_1d
     module procedure hdf5_read_attribute_r8_1d
     module procedure hdf5_read_attribute_i4_1d
     module procedure hdf5_read_attribute_i8_1d
  end interface

  public :: hdf5_open_file
  public :: hdf5_close_file
  public :: hdf5_read_dataset
  public :: hdf5_read_attribute
  public :: hdf5_dataset_size
  public :: hdf5_dataset_type
  public :: hdf5_attribute_size
  public :: hdf5_version
  public :: hdf5_find_datasets

contains

  subroutine hdf5_version(str)
!
! Return HDF5 version
!
    implicit none
    character(len=*) :: str

    call hdf5version(str, len(str))

    return
  end subroutine hdf5_version

  integer function hdf5_open_file(fname)
!
! Open an existing hdf5 file
!
    implicit none
    character(len=*)    :: fname
    integer(kind=C_INT) :: ret

    call openhdf5(trim(fname)//achar(0), ret)
    hdf5_open_file = int(ret)

    return
  end function hdf5_open_file


  integer function hdf5_close_file()
!
! Close the file
!
    implicit none
    integer(kind=C_INT) :: ret

    call closehdf5(ret)
    hdf5_close_file = int(ret)

    return
  end function hdf5_close_file


  integer function hdf5_dataset_size(name, rank, dims)
!
! Return the size of a dataset
!
    implicit none
    character(len=*)      :: name
    integer               :: rank
    integer(kind=int8byte), dimension(:) :: dims
    integer(kind=C_INT) :: c_rank
    integer(kind=C_LONG_LONG), dimension(size(dims)) :: c_dims
    integer(kind=C_INT) :: ios
    integer :: i

    call datasetsize(trim(name)//achar(0), c_rank, c_dims, &
         int(size(dims), kind=C_INT), ios)

    if(ios.ne.0)then
       hdf5_dataset_size = ios
       return
    endif

    rank = c_rank
    do i = 1, rank, 1
       dims(i) = c_dims(rank-i+1)
    end do
    hdf5_dataset_size = ios

    return
  end function hdf5_dataset_size


  integer function hdf5_attribute_size(name, rank, dims)
!
! Return the size of a attribute
!
    implicit none
    character(len=*)      :: name
    integer               :: rank
    integer(kind=int8byte), dimension(:) :: dims
    integer(kind=C_INT) :: c_rank
    integer(kind=C_LONG_LONG), dimension(size(dims)) :: c_dims
    integer(kind=C_INT) :: ios
    integer :: i

    call attribsize(trim(name)//achar(0), c_rank, c_dims, &
         int(size(dims), kind=C_INT), ios)

    if(ios.ne.0)then
       hdf5_attribute_size = ios
       return
    endif

    rank = c_rank
    do i = 1, rank, 1
       dims(i) = c_dims(rank-i+1)
    end do
    hdf5_attribute_size = ios

    return
  end function hdf5_attribute_size


  integer function hdf5_dataset_type(name, type)
!
! Return the type of a dataset
!
    implicit none
    character(len=*)      :: name
    integer               :: type
    integer(kind=C_INT)   :: c_type, ios

    call datasettype(trim(name)//achar(0), c_type, ios)
    select case(c_type)
    case(-1)
       type = HDF5_UNKNOWN
    case(0)
       type = HDF5_INTEGER4
    case(1)
       type = HDF5_INTEGER8
    case(2)
       type = HDF5_REAL4
    case(3)
       type = HDF5_REAL8
    end select
    hdf5_dataset_type = ios

    return
  end function hdf5_dataset_type


  integer function hdf5_read_attribute_r4(name, data) result(res)
!
! Read a HDF5 attribute
!
    implicit none
    character(len=*)                   :: name
    real(kind=real4byte)               :: data
    integer(kind=C_INT)                :: ios

    ! Read the data
    call readattrib(trim(name)//achar(0), int(HDF5_REAL4, kind=C_INT), &
         data, int(0, kind=C_INT), int(0, kind=C_LONG_LONG), ios)
    res = ios

    return
  end function hdf5_read_attribute_r4


  integer function hdf5_read_attribute_r8(name, data) result(res)
!
! Read a HDF5 attribute
!
    character(len=*)                   :: name
    real(kind=real8byte)               :: data
    integer(kind=C_INT)                :: ios

    ! Read the data
    call readattrib(trim(name)//achar(0), int(HDF5_REAL8, kind=C_INT), &
         data, int(0, kind=C_INT), int(0, kind=C_LONG_LONG), ios)
    res = ios

    return
  end function hdf5_read_attribute_r8


  integer function hdf5_read_attribute_i4(name, data) result(res)
!
! Read a HDF5 attribute
!
    implicit none
    character(len=*)                   :: name
    integer(kind=int4byte)             :: data
    integer(kind=C_INT)                :: ios

    ! Read the data
    call readattrib(trim(name)//achar(0), int(HDF5_INTEGER4, kind=C_INT), &
         data, int(0, kind=C_INT), int(0, kind=C_LONG_LONG), ios)
    res = ios

    return
  end function hdf5_read_attribute_i4


  integer function hdf5_read_attribute_i8(name, data) result(res)
!
! Read a HDF5 attribute
!
    implicit none
    character(len=*)                   :: name
    integer(kind=int8byte)             :: data
    integer(kind=C_INT)                :: ios

    ! Read the data
    call readattrib(trim(name)//achar(0), int(HDF5_INTEGER8, kind=C_INT), &
         data, int(0, kind=C_INT), int(0, kind=C_LONG_LONG), ios)
    res = ios

    return
  end function hdf5_read_attribute_i8


  integer function hdf5_read_attribute_r4_1d(name, data) result(res)
!
! Read a HDF5 attribute
!
    implicit none
    character(len=*)                   :: name
    real(kind=real4byte), dimension(:) :: data
    integer(kind=C_INT)                :: ios

    ! Read the data
    call readattrib(trim(name)//achar(0), int(HDF5_REAL4, kind=C_INT), &
         data, int(1, kind=C_INT), int(size(data), kind=C_LONG_LONG), ios)
    res = ios

    return
  end function hdf5_read_attribute_r4_1d


  integer function hdf5_read_attribute_r8_1d(name, data) result(res)
!
! Read a HDF5 attribute
!
    implicit none
    character(len=*)                   :: name
    real(kind=real8byte), dimension(:) :: data
    integer(kind=C_INT)                :: ios

    ! Read the data
    call readattrib(trim(name)//achar(0), int(HDF5_REAL8, kind=C_INT), &
         data, int(1, kind=C_INT), int(size(data), kind=C_LONG_LONG), ios)
    res = ios

    return
  end function hdf5_read_attribute_r8_1d


  integer function hdf5_read_attribute_i4_1d(name, data) result(res)
!
! Read a HDF5 attribute
!
    implicit none
    character(len=*)                   :: name
    integer(kind=int4byte), dimension(:) :: data
    integer(kind=C_INT)                :: ios

    ! Read the data
    call readattrib(trim(name)//achar(0), int(HDF5_INTEGER4, kind=C_INT), &
         data, int(1, kind=C_INT), int(size(data), kind=C_LONG_LONG), ios)
    res = ios

    return
  end function hdf5_read_attribute_i4_1d


  integer function hdf5_read_attribute_i8_1d(name, data) result(res)
!
! Read a HDF5 attribute
!
    implicit none
    character(len=*)                   :: name
    integer(kind=int8byte), dimension(:) :: data
    integer(kind=C_INT)                :: ios

    ! Read the data
    call readattrib(trim(name)//achar(0), int(HDF5_INTEGER8, kind=C_INT), &
         data, int(1, kind=C_INT), int(size(data), kind=C_LONG_LONG), ios)
    res = ios

    return
  end function hdf5_read_attribute_i8_1d


  integer function hdf5_read_dataset_r4_1d(name, data, start, count) &
       result(res)
!
! Read a HDF5 dataset
!
    implicit none
    ! Things that vary betweem data types
    real(kind=real4byte), dimension(:) :: data
    integer, parameter                 :: mem_rank = 1
    integer, parameter                 :: dtype = HDF5_REAL4
    ! Everything else...
    character(len=*)                   :: name
    integer(kind=int8byte), dimension(:), optional :: start,   count
    integer(kind=C_LONG_LONG), dimension(7) :: c_start
    integer(kind=C_LONG_LONG), dimension(7) :: c_count
    integer(kind=C_INT)                :: ios, c_rank
    integer(kind=int8byte), dimension(7) :: mem_dims, file_dims
    integer                            :: file_rank
    integer                            :: i

    ! Rank and size of input array
#ifdef SHAPE_HAS_KIND_PARAM
    mem_dims(1:mem_rank) = shape(data, kind=int8byte)
#else
    mem_dims(1:mem_rank) = shape(data)
#endif
    c_rank = mem_rank

    ! Get size of dataset in file
    i = hdf5_dataset_size(name, file_rank, file_dims)
    if(i.ne.0)then
       res = -1
       return
    endif

    ! Check rank matches
    if(file_rank.ne.mem_rank)then
       res = -1
       return
    end if

    ! Set start/count parameters
    if(present(start))then
       do i = 1, size(start), 1
          c_start(i) = start(i)
       end do
    else
       c_start = 0
    endif
    if(present(count))then
       do i = 1, size(count), 1
          c_count(i) = count(i)
       end do
    else
       do i = 1, mem_rank, 1
          c_count(i) = file_dims(i)
       end do
    endif

    ! Check requested data fits in memory
    do i = 1, mem_rank, 1
       if(c_count(i).gt.mem_dims(i))then
          res = -2
          return
       endif
    end do
    
    ! Check selection is in range in file
    do i = 1, mem_rank, 1
       if(c_start(i).lt.0.or.c_start(i)+c_count(i).gt.file_dims(i))then
          res = -3
          return
       endif
    end do
    
    ! Read the data
    call readdataset(trim(name)//achar(0), int(dtype, kind=C_INT), &
         data, c_rank, c_start, c_count, ios)
    res = ios

    return
  end function hdf5_read_dataset_r4_1d


  integer function hdf5_read_dataset_r8_1d(name, data, start, count) &
       result(res)
!
! Read a HDF5 dataset
!
    implicit none
    ! Things that vary betweem data types
    real(kind=real8byte), dimension(:) :: data
    integer, parameter                 :: mem_rank = 1
    integer, parameter                 :: dtype = HDF5_REAL8
    ! Everything else...
    character(len=*)                   :: name
    integer(kind=int8byte), dimension(:), optional :: start,   count
    integer(kind=C_LONG_LONG), dimension(7) :: c_start, c_count
    integer(kind=C_INT)                :: ios, c_rank
    integer(kind=int8byte), dimension(7) :: mem_dims, file_dims
    integer :: file_rank
    integer :: i

    ! Rank and size of input array
#ifdef SHAPE_HAS_KIND_PARAM
    mem_dims(1:mem_rank) = shape(data, kind=int8byte)
#else
    mem_dims(1:mem_rank) = shape(data)
#endif
    c_rank = mem_rank

    ! Get size of dataset in file
    i = hdf5_dataset_size(name, file_rank, file_dims)
    if(i.ne.0)then
       res = -1
       return
    endif

    ! Check rank matches
    if(file_rank.ne.mem_rank)then
       res = -1
       return
    end if

    ! Set start/count parameters
    if(present(start))then
       do i = 1, size(start), 1
          c_start(i) = start(i)
       end do
    else
       c_start = 0
    endif
    if(present(count))then
       do i = 1, size(count), 1
          c_count(i) = count(i)
       end do
    else
       do i = 1, mem_rank, 1
          c_count(i) = file_dims(i)
       end do
    endif

    ! Check requested data fits in memory
    do i = 1, mem_rank, 1
       if(c_count(i).gt.mem_dims(i))then
          res = -2
          return
       endif
    end do
    
    ! Check selection is in range in file
    do i = 1, mem_rank, 1
       if(c_start(i).lt.0.or.c_start(i)+c_count(i).gt.file_dims(i))then
          res = -3
          return
       endif
    end do
    
    ! Read the data
    call readdataset(trim(name)//achar(0), int(dtype, kind=C_INT), &
         data, c_rank, c_start, c_count, ios)
    res = ios

    return
  end function hdf5_read_dataset_r8_1d


  integer function hdf5_read_dataset_i4_1d(name, data, start, count) &
       result(res)
!
! Read a HDF5 dataset
!
    implicit none
    ! Things that vary betweem data types
    integer(kind=int4byte), dimension(:) :: data
    integer, parameter                 :: mem_rank = 1
    integer, parameter                 :: dtype = HDF5_INTEGER4
    ! Everything else...
    character(len=*)                   :: name
    integer(kind=int8byte), dimension(:), optional :: start,   count
    integer(kind=C_LONG_LONG), dimension(7)  :: c_start, c_count
    integer(kind=C_INT)                :: ios, c_rank
    integer(kind=int8byte), dimension(7) :: mem_dims, file_dims
    integer                            :: file_rank
    integer                            :: i

    ! Rank and size of input array
#ifdef SHAPE_HAS_KIND_PARAM
    mem_dims(1:mem_rank) = shape(data, kind=int8byte)
#else
    mem_dims(1:mem_rank) = shape(data)
#endif
    c_rank = mem_rank

    ! Get size of dataset in file
    i = hdf5_dataset_size(name, file_rank, file_dims)
    if(i.ne.0)then
       res = -1
       return
    endif

    ! Check rank matches
    if(file_rank.ne.mem_rank)then
       res = -1
       return
    end if

    ! Set start/count parameters
    if(present(start))then
       do i = 1, size(start), 1
          c_start(i) = start(i)
       end do
    else
       c_start = 0
    endif
    if(present(count))then
       do i = 1, size(count), 1
          c_count(i) = count(i)
       end do
    else
       do i = 1, mem_rank, 1
          c_count(i) = file_dims(i)
       end do
    endif

    ! Check requested data fits in memory
    do i = 1, mem_rank, 1
       if(c_count(i).gt.mem_dims(i))then
          res = -2
          return
       endif
    end do
    
    ! Check selection is in range in file
    do i = 1, mem_rank, 1
       if(c_start(i).lt.0.or.c_start(i)+c_count(i).gt.file_dims(i))then
          res = -3
          return
       endif
    end do
    
    ! Read the data
    call readdataset(trim(name)//achar(0), int(dtype, kind=C_INT), &
         data, c_rank, c_start, c_count, ios)
    res = ios

    return
  end function hdf5_read_dataset_i4_1d


  integer function hdf5_read_dataset_i8_1d(name, data, start, count) &
       result(res)
!
! Read a HDF5 dataset
!
    implicit none
    ! Things that vary betweem data types
    integer(kind=int8byte), dimension(:) :: data
    integer, parameter                 :: mem_rank = 1
    integer, parameter                 :: dtype = HDF5_INTEGER8
    ! Everything else...
    character(len=*)                   :: name
    integer(kind=int8byte), dimension(:), optional :: start, count
    integer(kind=C_LONG_LONG), dimension(7) :: c_start
    integer(kind=C_LONG_LONG), dimension(7) :: c_count
    integer(kind=C_INT)                :: ios, c_rank
    integer(kind=int8byte), dimension(7) :: mem_dims, file_dims
    integer                            :: file_rank
    integer                            :: i

    ! Rank and size of input array
#ifdef SHAPE_HAS_KIND_PARAM
    mem_dims(1:mem_rank) = shape(data, kind=int8byte)
#else
    mem_dims(1:mem_rank) = shape(data)
#endif
    c_rank = mem_rank

    ! Get size of dataset in file
    i = hdf5_dataset_size(name, file_rank, file_dims)
    if(i.ne.0)then
       res = -1
       return
    endif

    ! Check rank matches
    if(file_rank.ne.mem_rank)then
       res = -1
       return
    end if

    ! Set start/count parameters
    if(present(start))then
       do i = 1, size(start), 1
          c_start(i) = start(i)
       end do
    else
       c_start = 0
    endif
    if(present(count))then
       do i = 1, size(count), 1
          c_count(i) = count(i)
       end do
    else
       do i = 1, mem_rank, 1
          c_count(i) = file_dims(i)
       end do
    endif

    ! Check requested data fits in memory
    do i = 1, mem_rank, 1
       if(c_count(i).gt.mem_dims(i))then
          res = -2
          return
       endif
    end do
    
    ! Check selection is in range in file
    do i = 1, mem_rank, 1
       if(c_start(i).lt.0.or.c_start(i)+c_count(i).gt.file_dims(i))then
          res = -3
          return
       endif
    end do
    
    ! Read the data
    call readdataset(trim(name)//achar(0), int(dtype, kind=C_INT), &
         data, c_rank, c_start, c_count, ios)
    res = ios

    return
  end function hdf5_read_dataset_i8_1d


  integer function hdf5_read_dataset_r4_2d(name, data, start, count) &
       result(res)
!
! Read a HDF5 dataset
!
    implicit none
    ! Things that vary betweem data types
    real(kind=real4byte), dimension(:,:) :: data
    integer, parameter                 :: mem_rank = 2
    integer, parameter                 :: dtype = HDF5_REAL4
    ! Everything else...
    character(len=*)                   :: name
    integer(kind=int8byte), dimension(:), optional :: start,   count
    integer(kind=C_LONG_LONG), dimension(7) :: c_start
    integer(kind=C_LONG_LONG), dimension(7) :: c_count
    integer(kind=C_INT)                :: ios, c_rank
    integer(kind=int8byte), dimension(7) :: mem_dims, file_dims
    integer                            :: file_rank
    integer                            :: i

    ! Rank and size of input array
#ifdef SHAPE_HAS_KIND_PARAM
    mem_dims(1:mem_rank) = shape(data, kind=int8byte)
#else
    mem_dims(1:mem_rank) = shape(data)
#endif
    c_rank = mem_rank

    ! Get size of dataset in file
    i = hdf5_dataset_size(name, file_rank, file_dims)
    if(i.ne.0)then
       res = -1
       return
    endif

    ! Check rank matches
    if(file_rank.ne.mem_rank)then
       res = -1
       return
    end if

    ! Set start/count parameters
    if(present(start))then
       do i = 1, size(start), 1
          c_start(i) = start(i)
       end do
    else
       c_start = 0
    endif
    if(present(count))then
       do i = 1, size(count), 1
          c_count(i) = count(i)
       end do
    else
       do i = 1, mem_rank, 1
          c_count(i) = file_dims(i)
       end do
    endif

    ! Check requested data fits in memory
    do i = 1, mem_rank, 1
       if(c_count(i).gt.mem_dims(i))then
          res = -2
          return
       endif
    end do
    
    ! Check selection is in range in file
    do i = 1, mem_rank, 1
       if(c_start(i).lt.0.or.c_start(i)+c_count(i).gt.file_dims(i))then
          res = -3
          return
       endif
    end do
    
    ! Read the data
    call readdataset(trim(name)//achar(0), int(dtype, kind=C_INT), &
         data, c_rank, c_start, c_count, ios)
    res = ios

    return
  end function hdf5_read_dataset_r4_2d


  integer function hdf5_read_dataset_r8_2d(name, data, start, count) &
       result(res)
!
! Read a HDF5 dataset
!
    implicit none
    ! Things that vary betweem data types
    real(kind=real8byte), dimension(:,:) :: data
    integer, parameter                 :: mem_rank = 2
    integer, parameter                 :: dtype = HDF5_REAL8
    ! Everything else...
    character(len=*)                   :: name
    integer(kind=int8byte), dimension(:), optional :: start,   count
    integer(kind=C_LONG_LONG), dimension(7) :: c_start, c_count
    integer(kind=C_INT)                :: ios, c_rank
    integer(kind=int8byte), dimension(7) :: mem_dims, file_dims
    integer                            :: file_rank
    integer                            :: i

    ! Rank and size of input array
#ifdef SHAPE_HAS_KIND_PARAM
    mem_dims(1:mem_rank) = shape(data, kind=int8byte)
#else
    mem_dims(1:mem_rank) = shape(data)
#endif
    c_rank = mem_rank

    ! Get size of dataset in file
    i = hdf5_dataset_size(name, file_rank, file_dims)
    if(i.ne.0)then
       res = -1
       return
    endif

    ! Check rank matches
    if(file_rank.ne.mem_rank)then
       res = -1
       return
    end if

    ! Set start/count parameters
    if(present(start))then
       do i = 1, size(start), 1
          c_start(i) = start(i)
       end do
    else
       c_start = 0
    endif
    if(present(count))then
       do i = 1, size(count), 1
          c_count(i) = count(i)
       end do
    else
       do i = 1, mem_rank, 1
          c_count(i) = file_dims(i)
       end do
    endif

    ! Check requested data fits in memory
    do i = 1, mem_rank, 1
       if(c_count(i).gt.mem_dims(i))then
          res = -2
          return
       endif
    end do
    
    ! Check selection is in range in file
    do i = 1, mem_rank, 1
       if(c_start(i).lt.0.or.c_start(i)+c_count(i).gt.file_dims(i))then
          res = -3
          return
       endif
    end do
    
    ! Read the data
    call readdataset(trim(name)//achar(0), int(dtype, kind=C_INT), &
         data, c_rank, c_start, c_count, ios)
    res = ios

    return
  end function hdf5_read_dataset_r8_2d


  integer function hdf5_read_dataset_i4_2d(name, data, start, count) &
       result(res)
!
! Read a HDF5 dataset
!
    implicit none
    ! Things that vary betweem data types
    integer(kind=int4byte), dimension(:,:) :: data
    integer, parameter                 :: mem_rank = 2
    integer, parameter                 :: dtype = HDF5_INTEGER4
    ! Everything else...
    character(len=*)                   :: name
    integer(kind=int8byte), dimension(:), optional    :: start,   count
    integer(kind=C_LONG_LONG), dimension(7)  :: c_start, c_count
    integer(kind=C_INT)                :: ios, c_rank
    integer(kind=int8byte), dimension(7) :: mem_dims, file_dims
    integer                            :: file_rank
    integer                            :: i

    ! Rank and size of input array
#ifdef SHAPE_HAS_KIND_PARAM
    mem_dims(1:mem_rank) = shape(data, kind=int8byte)
#else
    mem_dims(1:mem_rank) = shape(data)
#endif
    c_rank = mem_rank

    ! Get size of dataset in file
    i = hdf5_dataset_size(name, file_rank, file_dims)
    if(i.ne.0)then
       res = -1
       return
    endif

    ! Check rank matches
    if(file_rank.ne.mem_rank)then
       res = -1
       return
    end if

    ! Set start/count parameters
    if(present(start))then
       do i = 1, size(start), 1
          c_start(i) = start(i)
       end do
    else
       c_start = 0
    endif
    if(present(count))then
       do i = 1, size(count), 1
          c_count(i) = count(i)
       end do
    else
       do i = 1, mem_rank, 1
          c_count(i) = file_dims(i)
       end do
    endif

    ! Check requested data fits in memory
    do i = 1, mem_rank, 1
       if(c_count(i).gt.mem_dims(i))then
          res = -2
          return
       endif
    end do
    
    ! Check selection is in range in file
    do i = 1, mem_rank, 1
       if(c_start(i).lt.0.or.c_start(i)+c_count(i).gt.file_dims(i))then
          res = -3
          return
       endif
    end do
    
    ! Read the data
    call readdataset(trim(name)//achar(0), int(dtype, kind=C_INT), &
         data, c_rank, c_start, c_count, ios)
    res = ios

    return
  end function hdf5_read_dataset_i4_2d


  integer function hdf5_read_dataset_i8_2d(name, data, start, count) &
       result(res)
!
! Read a HDF5 dataset
!
    implicit none
    ! Things that vary betweem data types
    integer(kind=int8byte), dimension(:,:) :: data
    integer, parameter                 :: mem_rank = 2
    integer, parameter                 :: dtype = HDF5_INTEGER8
    ! Everything else...
    character(len=*)                   :: name
    integer(kind=int8byte), dimension(:), optional :: start,   count
    integer(kind=C_LONG_LONG), dimension(7) :: c_start, c_count
    integer(kind=C_INT)                :: ios, c_rank
    integer(kind=int8byte), dimension(7) :: mem_dims, file_dims
    integer                            :: file_rank
    integer                            :: i

    ! Rank and size of input array
#ifdef SHAPE_HAS_KIND_PARAM
    mem_dims(1:mem_rank) = shape(data, kind=int8byte)
#else
    mem_dims(1:mem_rank) = shape(data)
#endif
    c_rank = mem_rank

    ! Get size of dataset in file
    i = hdf5_dataset_size(name, file_rank, file_dims)
    if(i.ne.0)then
       res = -1
       return
    endif

    ! Check rank matches
    if(file_rank.ne.mem_rank)then
       res = -1
       return
    end if

    ! Set start/count parameters
    if(present(start))then
       do i = 1, size(start), 1
          c_start(i) = start(i)
       end do
    else
       c_start = 0
    endif
    if(present(count))then
       do i = 1, size(count), 1
          c_count(i) = count(i)
       end do
    else
       do i = 1, mem_rank, 1
          c_count(i) = file_dims(i)
       end do
    endif

    ! Check requested data fits in memory
    do i = 1, mem_rank, 1
       if(c_count(i).gt.mem_dims(i))then
          res = -2
          return
       endif
    end do
    
    ! Check selection is in range in file
    do i = 1, mem_rank, 1
       if(c_start(i).lt.0.or.c_start(i)+c_count(i).gt.file_dims(i))then
          res = -3
          return
       endif
    end do
    
    ! Read the data
    call readdataset(trim(name)//achar(0), int(dtype, kind=C_INT), &
         data, c_rank, c_start, c_count, ios)
    res = ios

    return
  end function hdf5_read_dataset_i8_2d


  integer function hdf5_find_datasets(group_name, nfound, dataset_names) result(res)
!
! Find all datasets under the specified group. Returns 0 on success, nonzero otherwise.
! If dataset_names is not provided, just return the number of datasets.
!
    implicit none
    ! Parameters
    character(len=*), intent(in)  :: group_name
    integer,          intent(out) :: nfound
    character(len=*), dimension(:), intent(inout), optional :: dataset_names
    character(len=1), dimension(1) :: dummy
    ! C variables
    integer(kind=c_int) :: c_nmax
    integer(kind=c_int) :: c_maxlen
    integer(kind=c_int) :: c_nfound
    integer(kind=c_int) :: c_iostat

    if(present(dataset_names))then
       ! Count datasets and return names
       c_nmax   = size(dataset_names)
       c_maxlen = len(dataset_names)
       call listdatasets(trim(adjustl(group_name))//achar(0), &
            c_nmax, c_maxlen, c_nfound, dataset_names, c_iostat)
    else
       ! Just count the datasets
       c_nmax   = 0
       c_maxlen = len(dummy)
       call listdatasets(trim(adjustl(group_name))//achar(0), &
            c_nmax, c_maxlen, c_nfound, dummy, c_iostat)
    endif
    if(c_iostat.eq.0)then
       res = 0 ! Success
       nfound = c_nfound
    else
       res = -1 ! Failure
       nfound = 0
    endif
    
    return
  end function hdf5_find_datasets

end module read_hdf5
