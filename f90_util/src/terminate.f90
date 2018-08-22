module terminate_module

  implicit none

contains
  
  subroutine terminate(message)
    
    implicit none
    character(len=*), intent(in), optional :: message
    
    if(present(message))then
       write(0,*)trim(message)
    endif
    call cterminate(0)

    return
  end subroutine terminate

end module terminate_module
