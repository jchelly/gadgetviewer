module f90_util
!
! Various utility functions for use with the GUI library
!

#include "../../config.h"

  use c_types
  use get_argumentsmod

  implicit none
  integer, parameter :: fname_maxlen=500

  ! Kind numbers for 4 and 8 byte variables
  integer, parameter :: real4byte = selected_real_kind(6,37)
  integer, parameter :: real8byte = selected_real_kind(15,307)
  integer, parameter :: int4byte  = selected_int_kind(9)
  integer, parameter :: int8byte  = selected_int_kind(18)

  interface get_argument
     module procedure get_integer_argument
     module procedure get_real_argument
     module procedure get_double_argument
     module procedure get_string_argument
     module procedure get_logical_argument
  end interface

  interface read_binary
     module procedure read_binary_string_1d
     module procedure read_binary_string_scalar
     module procedure read_binary_integer4_scalar
     module procedure read_binary_integer8_scalar
     module procedure read_binary_real4_scalar
     module procedure read_binary_real8_scalar
     module procedure read_binary_integer4_1d
     module procedure read_binary_integer8_1d
     module procedure read_binary_real4_1d
     module procedure read_binary_real8_1d
     module procedure read_binary_integer4_2d
     module procedure read_binary_integer8_2d
     module procedure read_binary_real4_2d
     module procedure read_binary_real8_2d
  end interface

contains

  logical function get_full_path(fname, full_path)
!
! Given a filename which may or may not include an absolute or
! relative path, return the complete path. Returns false if the path
! cannot be expanded or the file doesn't exist.
!
    implicit none
    character(len=*)            :: fname, full_path
    character(len=fname_maxlen) :: str1

    ! First check if this is already an absolute path
    if(fname(1:1).eq."/")then
       full_path = trim(fname)
       get_full_path = .true.
       return
    endif

    ! Otherwise, get the current working directory
    call getcurrentdir(str1, int(len(str1),kind=C_INT))

    ! And prepend it to the path
    full_path = trim(str1)//"/"//trim(fname)

    get_full_path = .true.

    return
  end function get_full_path


  subroutine get_home_directory(path)
!
! Return users home directory
!
    implicit none
    character(len=*) :: path

    call gethomedir(path,int(len(path),kind=C_INT))

    return
  end subroutine get_home_directory


  logical function make_directory(dir)
!
! Create a directory. Return false on failure.
!
    implicit none
    character(len=*)    :: dir
    integer(kind=C_INT) :: status
    
    call makedir(trim(dir)//achar(0),status)

    ! Return status
    make_directory = (status.eq.0)

    return
  end function make_directory


  logical function write_png(fname,width,height,image)
!
! Write an image as a PNG file. Returns false on failure.
!
    implicit none
    ! Parameters
    character(len=*)               :: fname
    character(len=1), dimension(:) :: image
    integer, intent(in)            :: width, height
    ! Internal
    integer(kind=C_INT)            :: c_width, c_height, c_res

    c_width  = width
    c_height = height

    call writepng(trim(fname)//achar(0),c_width,c_height,image,c_res)
    
    ! Set return status
    if(c_res.eq.0)then
       write_png = .true.
    else
       write_png = .false.
    endif

    return
  end function write_png


  subroutine open_binary(fname, iostat)
!
! Open a binary file
!
    implicit none
    character(len=*)    :: fname
    integer             :: iostat
    integer(kind=C_INT) :: c_return

    call openbinaryfile(trim(fname)//achar(0), c_return)
    iostat = c_return

    return
  end subroutine open_binary


  subroutine close_binary()
!
! Close a binary file
!
    implicit none

    call closebinaryfile()

    return
  end subroutine close_binary

!
! Routines for reading binary data
!
!
! Scalars
!
  subroutine read_binary_string_scalar(data, iostat, pos)

    implicit none
    character(len=*)        :: data
    integer                 :: iostat
    integer, optional       :: pos
    integer                 :: nbytes
    integer(kind=C_INT)     :: c_return

    nbytes = len(data)
    if(present(pos))then
       call readbinarypos(pos, int(nbytes,kind=C_INT), data, c_return)
    else
       call readbinaryfile(nbytes, data, c_return)
    endif

    iostat = c_return

    return
  end subroutine read_binary_string_scalar


  subroutine read_binary_integer4_scalar(data, iostat, pos)

    implicit none
    integer(kind=int4byte)  :: data
    integer                 :: iostat
    integer, optional       :: pos
    integer                 :: nbytes
    integer(kind=C_INT)     :: c_return

    nbytes = 4
    if(present(pos))then
       call readbinarypos(pos, int(nbytes,kind=C_INT), data, c_return)
    else
       call readbinaryfile(int(nbytes,kind=C_INT), data, c_return)
    endif
    iostat = c_return

    return
  end subroutine read_binary_integer4_scalar

  subroutine read_binary_integer8_scalar(data, iostat, pos)

    implicit none
    integer(kind=int8byte)  :: data
    integer                 :: iostat
    integer, optional       :: pos
    integer                 :: nbytes
    integer(kind=C_INT)     :: c_return

    nbytes = 8
    if(present(pos))then
       call readbinarypos(pos, int(nbytes,kind=C_INT), data, c_return)
    else
       call readbinaryfile(int(nbytes,kind=C_INT), data, c_return)
    endif
    iostat = c_return

    return
  end subroutine read_binary_integer8_scalar

  subroutine read_binary_real4_scalar(data, iostat, pos)

    implicit none
    real(kind=real4byte)    :: data
    integer                 :: iostat
    integer, optional       :: pos
    integer                 :: nbytes
    integer(kind=C_INT)     :: c_return

    nbytes = 4
    if(present(pos))then
       call readbinarypos(pos, int(nbytes,kind=C_INT), data, c_return)
    else
       call readbinaryfile(int(nbytes,kind=C_INT), data, c_return)
    endif
    iostat = c_return

    return
  end subroutine read_binary_real4_scalar

  subroutine read_binary_real8_scalar(data, iostat, pos)

    implicit none
    real(kind=real8byte)    :: data
    integer                 :: iostat
    integer, optional       :: pos
    integer                 :: nbytes
    integer(kind=C_INT)     :: c_return

    nbytes = 8
    if(present(pos))then
       call readbinarypos(pos, int(nbytes,kind=C_INT), data, c_return)
    else
       call readbinaryfile(int(nbytes,kind=C_INT), data, c_return)
    endif
    iostat = c_return

    return
  end subroutine read_binary_real8_scalar
!
! 1D arrays
!
  subroutine read_binary_string_1d(data, iostat, pos)

    implicit none
    character(len=*), dimension(:) :: data
    integer                 :: iostat
    integer, optional       :: pos
    integer                 :: nbytes
    integer(kind=C_INT)     :: c_return

    nbytes = len(data) * size(data)
    if(present(pos))then
       call readbinarypos(pos, int(nbytes,kind=C_INT), data, c_return)
    else
       call readbinaryfile(int(nbytes,kind=C_INT), data, c_return)
    endif
    iostat = c_return

    return
  end subroutine read_binary_string_1d

  subroutine read_binary_integer4_1d(data, iostat, pos)

    implicit none
    integer(kind=int4byte), dimension(:) :: data
    integer                 :: iostat
    integer, optional       :: pos
    integer                 :: nbytes
    integer(kind=C_INT)     :: c_return

    nbytes = 4 * size(data)
    if(present(pos))then
       call readbinarypos(pos, int(nbytes,kind=C_INT), data, c_return)
    else
       call readbinaryfile(int(nbytes,kind=C_INT), data, c_return)
    endif
    iostat = c_return

    return
  end subroutine read_binary_integer4_1d

  subroutine read_binary_integer8_1d(data, iostat, pos)

    implicit none
    integer(kind=int8byte), dimension(:) :: data
    integer                 :: iostat
    integer, optional       :: pos
    integer                 :: nbytes
    integer(kind=C_INT)     :: c_return

    nbytes = 8 * size(data)
    if(present(pos))then
       call readbinarypos(pos, int(nbytes,kind=C_INT), data, c_return)
    else
       call readbinaryfile(int(nbytes,kind=C_INT), data, c_return)
    endif
    iostat = c_return

    return
  end subroutine read_binary_integer8_1d

  subroutine read_binary_real4_1d(data, iostat, pos)

    implicit none
    real(kind=real4byte), dimension(:) :: data
    integer                 :: iostat
    integer, optional       :: pos
    integer                 :: nbytes
    integer(kind=C_INT)     :: c_return

    nbytes = 4 * size(data)
    if(present(pos))then
       call readbinarypos(pos, int(nbytes,kind=C_INT), data, c_return)
    else
       call readbinaryfile(int(nbytes,kind=C_INT), data, c_return)
    endif
    iostat = c_return

    return
  end subroutine read_binary_real4_1d

  subroutine read_binary_real8_1d(data, iostat, pos)

    implicit none
    real(kind=real8byte), dimension(:) :: data
    integer                 :: iostat
    integer, optional       :: pos
    integer                 :: nbytes
    integer(kind=C_INT)     :: c_return

    nbytes = 8 * size(data)
    if(present(pos))then
       call readbinarypos(pos, int(nbytes,kind=C_INT), data, c_return)
    else
       call readbinaryfile(int(nbytes,kind=C_INT), data, c_return)
    endif
    iostat = c_return

    return
  end subroutine read_binary_real8_1d
!
! 2D arrays
!
  subroutine read_binary_integer4_2d(data, iostat, pos)

    implicit none
    integer(kind=int4byte), dimension(:,:) :: data
    integer                 :: iostat
    integer, optional       :: pos
    integer                 :: nbytes
    integer(kind=C_INT)     :: c_return

    nbytes = 4 * size(data)
    if(present(pos))then
       call readbinarypos(pos, int(nbytes,kind=C_INT), data, c_return)
    else
       call readbinaryfile(int(nbytes,kind=C_INT), data, c_return)
    endif
    iostat = c_return

    return
  end subroutine read_binary_integer4_2d

  subroutine read_binary_integer8_2d(data, iostat, pos)

    implicit none
    integer(kind=int8byte), dimension(:,:) :: data
    integer                 :: iostat
    integer, optional       :: pos
    integer                 :: nbytes
    integer(kind=C_INT)     :: c_return

    nbytes = 8 * size(data)
    if(present(pos))then
       call readbinarypos(pos, int(nbytes,kind=C_INT), data, c_return)
    else
       call readbinaryfile(int(nbytes,kind=C_INT), data, c_return)
    endif
    iostat = c_return

    return
  end subroutine read_binary_integer8_2d

  subroutine read_binary_real4_2d(data, iostat, pos)

    implicit none
    real(kind=real4byte), dimension(:,:) :: data
    integer                 :: iostat
    integer, optional       :: pos
    integer                 :: nbytes
    integer(kind=C_INT)     :: c_return

    nbytes = 4 * size(data)
    if(present(pos))then
       call readbinarypos(pos, int(nbytes,kind=C_INT), data, c_return)
    else
       call readbinaryfile(int(nbytes,kind=C_INT), data, c_return)
    endif
    iostat = c_return

    return
  end subroutine read_binary_real4_2d

  subroutine read_binary_real8_2d(data, iostat, pos)

    implicit none
    real(kind=real8byte), dimension(:,:) :: data
    integer                 :: iostat
    integer, optional       :: pos
    integer                 :: nbytes
    integer(kind=C_INT)     :: c_return

    nbytes = 8 * size(data)
    if(present(pos))then
       call readbinarypos(pos, int(nbytes,kind=C_INT), data, c_return)
    else
       call readbinaryfile(int(nbytes,kind=C_INT), data, c_return)
    endif
    iostat = c_return

    return
  end subroutine read_binary_real8_2d

  subroutine skip_bytes(nbytes, iostat)

    implicit none
    integer :: nbytes
    integer :: iostat
    integer(kind=C_INT)     :: c_return

    call skipbytes(nbytes, c_return)
    iostat = c_return

    return
  end subroutine skip_bytes

  subroutine glob(pattern)

    implicit none
    character(len=*) :: pattern
    integer          :: i

    ! Add null terminator
    i = len_trim(pattern) + 1
    if(i.gt.0.and.i.le.len(pattern))pattern(i:i) = achar(0)

    ! Try to match to a file name
    call globfname(pattern, len(pattern))
    
    return
  end subroutine glob

end module f90_util


