module String_Module
!
! Function to convert real/integer/double precision/logical to a string.
! Optional parameter 'fmt' is format statement to use.
!
! This is similar to the IDL 'string' function.
!
  use data_types

  interface string
     module procedure int4_to_string
     module procedure int8_to_string
     module procedure real4_to_string
     module procedure real8_to_string
     module procedure logical_to_string
  end interface

contains

  function int4_to_string(i,fmt)

    implicit none
    integer(kind=int4byte), intent(in) :: i
    character(LEN=20) :: int4_to_string, tmp
    character(LEN=*), optional :: fmt

    if(present(fmt))then
       write(tmp,fmt)i
    else
       write(tmp,'(i20)')i
    end if

    int4_to_string = adjustl(tmp)

    return
  end function int4_to_string

  function int8_to_string(i,fmt)

    implicit none
    integer(kind=int8byte), intent(in) :: i
    character(LEN=20) :: int8_to_string, tmp
    character(LEN=*), optional :: fmt

    if(present(fmt))then
       write(tmp,fmt)i
    else
       write(tmp,'(i20)')i
    end if

    int8_to_string = adjustl(tmp)

    return
  end function int8_to_string

  function real4_to_string(r,fmt)

    implicit none
    real(kind=real4byte), intent(in) :: r
    character(LEN=20) :: real4_to_string, tmp
    character(LEN=*), optional :: fmt

    if(present(fmt))then
       write(tmp,fmt)r
    else
       write(tmp,'(e14.6)')r
    end if

    real4_to_string = adjustl(tmp)

    return
  end function real4_to_string

  function real8_to_string(r,fmt)

    implicit none
    real(kind=int8byte), intent(in) :: r
    character(LEN=20) :: real8_to_string, tmp
    character(LEN=*), optional :: fmt

    if(present(fmt))then
       write(tmp,fmt)r
    else
       write(tmp,'(e14.6)')r
    end if

    real8_to_string = adjustl(tmp)

    return
  end function real8_to_string

  function logical_to_string(l)

    implicit none
    logical, intent(in) :: l
    character(LEN=20) :: logical_to_string
  
    if(l)then
       logical_to_string = "true"
    else
       logical_to_string = "false"
    end if
       
    return
  end function logical_to_string



  subroutine split_string(str, delim, n, arr)
!
! Split a string into substrings delimited by 'delim'
!
    implicit none
    integer                        :: n
    character(len=*)               :: str, delim
    character(len=*), dimension(:) :: arr
    integer :: i
    character(len=len(str))        :: s
    
    s = trim(adjustl(str))
    n = 0
    do while(len_trim(s).gt.0.and.n.lt.size(arr))
       i = index(s,delim)
       if(i.lt.1)then
          ! No delimiter so take whole remaining string
          n = n + 1
          arr(n) = trim(adjustl(s))
          s = ""
       else
          ! Found delimiter
          n = n + 1
          arr(n) = trim(adjustl(s(1:i-1)))
          s = s(i+len(delim):)
       endif
    end do

    return
  end subroutine split_string


  integer function replace_string(str, find, replace, result, back)
!
! Replace any instances of find with replace in str.
! Returns non zero on failure.
!
! Replaces last instance if back=.true., first instance
! otherwise. 
!
    implicit none
    character(len=*), intent(in)    :: str, find
    character(len=*), intent(in)    :: replace
    character(len=*), intent(inout) :: result
    logical, intent(in), optional :: back
    logical :: from_back
    integer :: i

    from_back = .false.
    if(present(back))from_back = back

    ! Check for string to replace
    i = index(str, find, back=back)
    if(i.le.0)then
       ! Not found
       replace_string = -1
       return
    endif
    
    result = str(1:i-1)//trim(replace)//str(i+len_trim(find):)
    replace_string = 0

    return
  end function replace_string

end module String_Module
