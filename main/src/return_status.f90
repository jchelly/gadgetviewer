module return_status

  implicit none
  integer, parameter :: error_maxlen = 200
  
!
! This defines a type that allows an error code and a message
! to be returned by a function call
!
  type result_type
     ! Whether the call was successful
     logical :: success
     ! An error code
     integer :: errno
     ! A string describing the error
     character(len=error_maxlen) :: string
  end type result_type

end module return_status
