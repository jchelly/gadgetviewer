module configuration
!
! Module to handle configuration files
!
  use f90_util
  use gadget_hdf5_reader
  use gadget_eagle_reader
  use gadget_binary_type2_reader
  use swift_reader
  use colour_table
  use mouse_handler

  character(len=500) :: config_dir
  character(len=500) :: default_file

contains

  subroutine configuration_read()
!
! Make a .gadgetviewer directory if it doesn't exist
!
    implicit none
    character(len=500) :: dirname
    logical :: res
    integer :: ios
    
    dirname = ""
    call get_home_directory(dirname)
    dirname = trim(dirname)//"/.gadgetviewer_settings"
    config_dir = dirname
    default_file = trim(dirname)//"/default"

    res = make_directory(dirname)
    if(res)then
       write(*,*)'Created configuration directory: ',trim(dirname)
    endif

    ! Check configuration dir is accessible
    open(unit=1,file=trim(dirname)//"/.tmp",status='unknown',&
         form='formatted', iostat=ios)
    if(ios.ne.0)then
       write(0,*)trim(dirname)//" is not writeable"
    else
       write(1,'(".")',iostat=ios)
       if(ios.ne.0)then
          write(0,*)trim(dirname)//" is not writeable"
       endif
       close(1, iostat=ios)
    endif
!
! Read configuration for modules that need it
!    
    call gadget_hdf5_read_conf(dirname)
    call gadget_eagle_read_conf(dirname)
    call gadget_binary_type2_read_conf(dirname)
    call swift_read_conf(dirname)
    call colour_table_read_conf(dirname)
    call mouse_handler_read_conf(dirname)

    return
  end subroutine configuration_read

end module configuration
