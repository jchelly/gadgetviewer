module file_units
!
! This returns the number of an unused file unit
!
! It tries unit numbers from 100 upwards and returns the first free unit.
! It aborts if it runs out of valid unit numbers.
!
  use f90_util

contains

  integer function get_unit()

    implicit none
    integer :: i
    logical :: open, exists
    
    i = 100
    open   = .true.
    exists = .true.
    do while(open.and.exists)
       i=i+1
       inquire(UNIT=i,OPENED=open,EXIST=exists)
    end do
    
    if(.not.exists)then
       call terminate('Error in get_unit(): ran out of valid unit numbers!')
    end if
    
    get_unit = i
    
    return
  end function get_unit
  
end module file_units
