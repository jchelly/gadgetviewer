module get_argumentsmod
  !
  ! This module provides the subroutine get_argument(iarg,x), which
  ! returns the i'th command line argument n variable x. x can be
  ! real, integer, double precision, string or logical.
  !
  use terminate_module

  implicit none
#include "../../config.h" 
  character(LEN=500) :: usage
  logical :: usage_set = .false.

contains

  integer function get_nargs()
!
! Return number of command line arguments
!
    implicit none

#ifdef F2003_CMDLINE
    get_nargs = command_argument_count()
#else
#ifdef IARGC_DECLARE
      integer, external :: iargc
      get_nargs = iargc()
#else
#ifdef IARGC_CMDLINE
      get_nargs = iargc()
#else
      get_nargs = 0
#endif
#endif
#endif

    return
  end function get_nargs

  
  subroutine get_arg(i,str)
!
! Return the i'th argument as a string
!
    implicit none
    integer :: i
    character(len=*) :: str

#ifdef F2003_CMDLINE
    call get_command_argument(i,str)
#else
#ifdef IARGC_CMDLINE
    call getarg(i,str)
#else
#ifdef IARGC_DECLARE
    call getarg(i,str)
#else
      ! In this case we have no way to get command line arguments
      str = "" 
#endif
#endif
#endif

    return
  end subroutine get_arg


  subroutine set_usage(message)
    
    implicit none
    character(LEN=*) :: message

    usage=trim(message)
    usage_set = .true.

    return
  end subroutine set_usage


  subroutine get_integer_argument(iarg, i)

    implicit none

    integer, intent(IN)  :: iarg
    integer, intent(OUT) :: i
    integer :: ios
    character(LEN=256)   :: str

    if(get_nargs().lt.iarg)then
       write(*,*)'Error in get_argument(): Too few arguments'
       write(*,*)'Number of command line arguments is ',get_nargs()
       write(*,*)'Attempted to read argument ',iarg
       if(usage_set)then
          write(*,*)''
          write(*,*)'Usage: ',trim(usage)
          write(*,*)''
       end if
       call terminate()
    end if

    call get_arg(iarg,str)
    read(str,*,iostat=ios)i

    if(ios.ne.0)then
       write(*,*)'Error in get_argument():'
       write(*,*)'Unable to interpret argument ',trim(str),' as INTEGER'
       if(usage_set)then
          write(*,*)''
          write(*,*)'Usage: ',trim(usage)
          write(*,*)''
       end if
       call terminate()
    end if

    return
  end subroutine get_integer_argument


  subroutine get_real_argument(iarg, r)

    implicit none

    integer, intent(IN)  :: iarg
    real, intent(OUT) :: r
    integer :: ios
    character(LEN=256)   :: str

    if(get_nargs().lt.iarg)then
       write(*,*)'Error in get_argument(): Too few arguments'
       write(*,*)'Number of command line arguments is ',get_nargs()
       write(*,*)'Attempted to read argument ',iarg
       if(usage_set)then
          write(*,*)''
          write(*,*)'Usage: ',trim(usage)
          write(*,*)''
       end if
       call terminate()
    end if

    call get_arg(iarg,str)
    read(str,*,iostat=ios)r

    if(ios.ne.0)then
       write(*,*)'Error in get_argument():'
       write(*,*)'Unable to interpret argument ',trim(str),' as REAL'
       if(usage_set)then
          write(*,*)''
          write(*,*)'Usage: ',trim(usage)
          write(*,*)''
       end if
       call terminate()
    end if

    return
  end subroutine get_real_argument


  subroutine get_double_argument(iarg, d)

    implicit none

    integer, intent(IN)  :: iarg
    double precision, intent(OUT) :: d
    integer :: ios
    character(LEN=256)   :: str

    if(get_nargs().lt.iarg)then
       write(*,*)'Error in get_argument(): Too few arguments'
       write(*,*)'Number of command line arguments is ',get_nargs()
       write(*,*)'Attempted to read argument ',iarg
       if(usage_set)then
          write(*,*)''
          write(*,*)'Usage: ',trim(usage)
          write(*,*)''
       end if
       call terminate()
    end if

    call get_arg(iarg,str)
    read(str,*,iostat=ios)d

    if(ios.ne.0)then
       write(*,*)'Error in get_argument():'
       write(*,*)'Unable to interpret argument ',trim(str), &
            ' as DOUBLE PRECISION'
       if(usage_set)then
          write(*,*)''
          write(*,*)'Usage: ',trim(usage)
          write(*,*)''
       end if
       call terminate()
    end if

    return
  end subroutine get_double_argument


  subroutine get_logical_argument(iarg, l)

    implicit none

    integer, intent(IN)  :: iarg
    logical, intent(OUT) :: l
    integer :: ios
    character(LEN=256)   :: str

    if(get_nargs().lt.iarg)then
       write(*,*)'Error in get_argument(): Too few arguments'
       write(*,*)'Number of command line arguments is ',get_nargs()
       write(*,*)'Attempted to read argument ',iarg
       if(usage_set)then
          write(*,*)''
          write(*,*)'Usage: ',trim(usage)
          write(*,*)''
       end if
       call terminate()
    end if

    call get_arg(iarg,str)
    read(str,*,iostat=ios)l

    if(ios.ne.0)then
       write(*,*)'Error in get_argument():'
       write(*,*)'Unable to interpret argument ',trim(str), &
            ' as LOGICAL'
       if(usage_set)then
          write(*,*)''
          write(*,*)'Usage: ',trim(usage)
          write(*,*)''
       end if
       call terminate()
    end if

    return
  end subroutine get_logical_argument


  subroutine get_string_argument(iarg, str)

    implicit none

    integer, intent(IN)  :: iarg
    character(LEN=*), intent(OUT) :: str

    if(get_nargs().lt.iarg)then
       write(*,*)'Error in get_argument(): Too few arguments'
       write(*,*)'Number of command line arguments is ',get_nargs()
       write(*,*)'Attempted to read argument ',iarg
       if(usage_set)then
          write(*,*)''
          write(*,*)'Usage: ',trim(usage)
          write(*,*)''
       end if
       call terminate()
    end if

    call get_arg(iarg,str)

    return
  end subroutine get_string_argument

end module get_argumentsmod




