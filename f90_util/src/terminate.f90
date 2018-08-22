module terminate_module

  use c_types
  implicit none

contains
  
  subroutine terminate(message)
    
    implicit none
    character(len=*), intent(in), optional :: message
    
    if(present(message))then
       write(0,*)trim(message)
    endif
    call cterminate(int(0, kind=C_INT))

    return
  end subroutine terminate

end module terminate_module
