module key_file

  use c_types

  implicit none
  private
  save

  ! Opening/closing key files
  public :: new_key_file
  public :: read_key_file
  public :: write_key_file
  public :: close_key_file
  public :: get_group_names
  public :: file_has_group

  ! Interface for setting keys
  interface set_key
     module procedure set_integer_key_single
     module procedure set_integer_key_list
     module procedure set_real_key_single
     module procedure set_real_key_list
     module procedure set_string_key_single
     module procedure set_string_key_list
     module procedure set_logical_key_single
     module procedure set_logical_key_list
  end interface
  public :: set_key

  ! Interface for getting keys
  interface get_key
     module procedure get_integer_key_single
     module procedure get_integer_key_list
     module procedure get_real_key_single
     module procedure get_real_key_list
     module procedure get_string_key_single
     module procedure get_string_key_list
     module procedure get_logical_key_single
     module procedure get_logical_key_list
  end interface
  public :: get_key

contains

  subroutine new_key_file()
!
! Make a new key file in memory
!
    implicit none

    call newkeyfile()

    return
  end subroutine new_key_file

  subroutine read_key_file(fname, iostat)
!
! Read in a key file
!
    implicit none
    character(len=*), intent(in) :: fname
    integer, optional            :: iostat
    integer(kind=C_INT)          :: c_iostat

    call readkeyfile(trim(fname)//char(0), c_iostat)
    if(present(iostat))iostat=c_iostat

    return
  end subroutine read_key_file
  

  subroutine write_key_file(fname, iostat)
!
! Write out a key file
!
    implicit none
    character(len=*), intent(in) :: fname
    integer, optional            :: iostat
    integer(kind=C_INT)          :: c_iostat

    call writekeyfile(trim(fname)//char(0), c_iostat)
    if(present(iostat))iostat=c_iostat

    return
  end subroutine write_key_file


  subroutine close_key_file()
!
! Close the key file
!
    implicit none
    
    call closekeyfile()

    return
  end subroutine close_key_file


  subroutine set_integer_key_single(group, name, value)
!
! Set a key to a single integer
!
    character(len=*), intent(in) :: group, name
    integer,          intent(in) :: value
    integer(kind=C_INT)          :: c_value(1)
    
    c_value(1) = value

    call addintkey( &
         trim(group)//char(0), &
         trim(name)//char(0), &
         c_value, int(1, kind=C_INT))

    return
  end subroutine set_integer_key_single


  subroutine set_integer_key_list(group, name, value)
!
! Set a key to a single integer
!
    character(len=*), intent(in) :: group, name
    integer, dimension(:), intent(in) :: value
    integer(kind=C_INT), dimension(size(value)) :: c_value
    
    c_value = value

    call addintkey( &
         trim(group)//char(0), &
         trim(name)//char(0), &
         c_value, int(size(c_value), kind=C_INT))

    return
  end subroutine set_integer_key_list


  subroutine set_real_key_single(group, name, value)
!
! Set a key to a single real
!
    character(len=*), intent(in) :: group, name
    real,             intent(in) :: value
    real(kind=C_DOUBLE)          :: c_value(1)
    
    c_value(1) = value

    call adddoublekey( &
         trim(group)//char(0), &
         trim(name)//char(0), &
         c_value, int(1, kind=C_INT))

    return
  end subroutine set_real_key_single


  subroutine set_real_key_list(group, name, value)
!
! Set a key to a single real
!
    character(len=*), intent(in) :: group, name
    real, dimension(:), intent(in) :: value
    real(kind=C_DOUBLE), dimension(size(value)) :: c_value
    
    c_value = value

    call adddoublekey( &
         trim(group)//char(0), &
         trim(name)//char(0), &
         c_value, int(size(c_value), kind=C_INT))

    return
  end subroutine set_real_key_list


  subroutine set_string_key_single(group, name, value)
!
! Set a key to a single string
!
    character(len=*), intent(in) :: group, name
    character(len=*), intent(in) :: value

    call addstringkey( &
         trim(group)//char(0), &
         trim(name)//char(0), &
         trim(value)//char(0), &
         int(len(value), kind=C_INT), &
         int(1, kind=C_INT))

    return
  end subroutine set_string_key_single


  subroutine set_string_key_list(group, name, value)
!
! Set a key to an array of strings
!
    character(len=*), intent(in) :: group, name
    character(len=*),          dimension(:), intent(in) :: value
    character(len=len(value)), dimension(size(value))   :: c_value
    integer :: i

    do i = 1, size(value), 1
       c_value(i) = trim(value(i))//char(0)
    end do

    call addstringkey( &
         trim(group)//char(0), &
         trim(name)//char(0), &
         c_value, &
         int(len(c_value),  kind=C_INT), &
         int(size(c_value), kind=C_INT))

    return
  end subroutine set_string_key_list


  subroutine set_logical_key_single(group, name, value)
!
! Set a logical key
!
    implicit none
    character(len=*), intent(in)    :: group, name
    logical, intent(in)             :: value
    integer(kind=C_INT)             :: c_value(1)

    if(value)then
       c_value(1) = 1
    else
       c_value(1) = 0
    endif
    call addintkey( &
         trim(group)//char(0), &
         trim(name)//char(0), &
         c_value, int(1, kind=C_INT))
    
    return
  end subroutine set_logical_key_single


  subroutine set_logical_key_list(group, name, value)
!
! Set a logical key
!
    implicit none
    character(len=*), intent(in)      :: group, name
    logical, intent(in), dimension(:) :: value
    integer(kind=C_INT), dimension(size(value)) :: c_value
    integer :: i

    do i = 1, size(value), 1
       if(value(i))then
          c_value(i) = 1
       else
          c_value(i) = 0
       endif
    end do
    call addintkey( &
         trim(group)//char(0), &
         trim(name)//char(0), &
         c_value, int(size(c_value), kind=C_INT))
    
    return
  end subroutine set_logical_key_list


  subroutine get_integer_key_single(group, name, value)
!
! Retrieve a single integer key
!
    character(len=*), intent(in)    :: group, name
    integer,          intent(inout) :: value
    integer(kind=C_INT)             :: c_value(1)
    integer(kind=C_INT)             :: n

    call getintkey( &
         trim(group)//char(0), &
         trim(name)//char(0), &
         c_value, n, &
         int(1, kind=C_INT))
 
    if(n.gt.0)value = c_value(1)

    return
  end subroutine get_integer_key_single


  subroutine get_integer_key_list(group, name, value)
!
! Retrieve a list of integer keys
!
    character(len=*), intent(in) :: group, name
    integer, dimension(:), intent(inout) :: value
    integer(kind=C_INT), dimension(size(value)) :: c_value
    integer(kind=C_INT) :: n
    integer :: i
    
    call getintkey( &
         trim(group)//char(0), &
         trim(name)//char(0), &
         c_value, n, int(size(c_value), kind=C_INT))
    do i = 1, n, 1
       value(i) = c_value(i)
    end do

    return
  end subroutine get_integer_key_list


  subroutine get_real_key_single(group, name, value)
!
! Retrieve a single real key
!
    character(len=*), intent(in)    :: group, name
    real,             intent(inout) :: value
    real(kind=C_DOUBLE)             :: c_value(1)
    integer(kind=C_INT)             :: n

    call getdoublekey( &
         trim(group)//char(0), &
         trim(name)//char(0), &
         c_value, n, &
         int(1, kind=C_INT))
    if(n.gt.0)value = c_value(1)

    return
  end subroutine get_real_key_single


  subroutine get_real_key_list(group, name, value)
!
! Retrieve a list of real keys
!
    character(len=*), intent(in) :: group, name
    real, dimension(:), intent(inout) :: value
    real(kind=C_DOUBLE), dimension(size(value)) :: c_value
    integer(kind=C_INT) :: n
    integer :: i
    
    call getdoublekey( &
         trim(group)//char(0), &
         trim(name)//char(0), &
         c_value, n, int(size(c_value), kind=C_INT))
    do i = 1, n, 1
       value(i) = c_value(i)
    end do

    return
  end subroutine get_real_key_list


  subroutine get_string_key_single(group, name, value)
!
! Retrieve a single string key
!
    character(len=*), intent(in)    :: group, name
    character(len=*), intent(inout) :: value
    integer(kind=C_INT)             :: n

    call getstringkey( &
         trim(group)//char(0), &
         trim(name)//char(0), &
         value, int(len(value), kind=C_INT), n, &
         int(1, kind=C_INT))

    return
  end subroutine get_string_key_single


  subroutine get_string_key_list(group, name, value)
!
! Retrieve a list of strings
!
    character(len=*), intent(in)    :: group, name
    character(len=*), dimension(:), intent(inout) :: value
    integer(kind=C_INT)             :: n

    call getstringkey( &
         trim(group)//char(0), &
         trim(name)//char(0), &
         value, int(len(value), kind=C_INT), n, &
         int(size(value), kind=C_INT))

    return
  end subroutine get_string_key_list


  subroutine get_logical_key_single(group, name, value)
!
! Get a logical key
!
    implicit none
    character(len=*), intent(in)    :: group, name
    logical, intent(inout)             :: value
    integer(kind=C_INT)             :: c_value(1)
    integer(kind=C_INT)             :: n

    call getintkey( &
         trim(group)//char(0), &
         trim(name)//char(0), &
         c_value, n, int(1, kind=C_INT))
    
    if(n.eq.1)value = c_value(1).ne.0

    return
  end subroutine get_logical_key_single


  subroutine get_logical_key_list(group, name, value)
!
! Get a logical key
!
    implicit none
    character(len=*), intent(in)      :: group, name
    logical, intent(inout), dimension(:) :: value
    integer(kind=C_INT), dimension(size(value)) :: c_value
    integer(kind=C_INT) :: n
    integer :: i

    call getintkey( &
         trim(group)//char(0), &
         trim(name)//char(0), &
         c_value, n, int(size(c_value), kind=C_INT))
    do i = 1, n, 1
       value(i) = c_value(i).ne.0
    end do
    
    return
  end subroutine get_logical_key_list


  subroutine get_group_names(n, names)
!
! Return a list of the names of groups in this file.
! Just get the number of groups if names is not supplied.
!
    implicit none
    character(len=*), dimension(:), optional, intent(inout) :: names
    integer, intent(out) :: n
    integer(kind=C_INT)  :: n_c

    if(present(names))then
       call getgroupnames(names, int(len(names), C_INT), &
            n_c, int(size(names), C_INT))
    else
       call getngroups(n_c)
    endif
    n = n_c
    
    return
  end subroutine get_group_names


  logical function file_has_group(name)
!
! Return true if the specified group exists
!
    implicit none
    character(len=*) :: name
    integer(kind=C_INT) :: flag
   
    call hasgroup(trim(name)//char(0), flag)
    
    file_has_group = flag.ne.0

    return
  end function file_has_group

end module key_file
