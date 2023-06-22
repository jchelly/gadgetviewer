module select_point

  use data_types
  use f90_gui
  use view_parameters
  use particle_store
  use transform
  use stereo

  implicit none
  private
  save

  public :: select_point_open
  public :: select_point_close
  public :: select_point_process_events

  ! Widgets
  type (gui_window)   :: window
  type (gui_entrybox) :: xcoord_box, ycoord_box, zcoord_box
  type (gui_button)   :: go_button
  logical             :: window_open = .false.

contains

  subroutine select_point_open(mainwin)
!
! Open a window that can be used to go to specific coordinates
!    
    implicit none
    type (gui_window) :: mainwin
    type (gui_box)    :: vbox, hbox
    type (gui_label)  :: label

    if(window_open)return

    ! Set packing mode
    call gui_packing_mode(expand=.false., fill=.false., spacing=3, &
         position=gui_start)

    ! Create the window
    call gui_create_window(window,"Jump to position",parent=mainwin,&
         resize=.false.)
    call gui_create_box(vbox, window, gui_vertical)
    call gui_create_label(label, vbox, "Enter coordinates:")
    call gui_create_box(hbox, vbox, gui_horizontal)
    call gui_create_label(label, hbox, "x: ")
    call gui_create_entrybox(xcoord_box, hbox, 12)
    call gui_create_label(label, hbox, "y: ")
    call gui_create_entrybox(ycoord_box, hbox, 12)
    call gui_create_label(label, hbox, "z: ")
    call gui_create_entrybox(zcoord_box, hbox, 12)
    call gui_create_box(hbox, vbox, gui_horizontal)
    call gui_packing_mode(position=gui_end)
    call gui_create_button(go_button, hbox, " Go to position ")
    call gui_packing_mode(position=gui_start)
    call gui_show_window(window)
    window_open = .true.

    if(stereo_enabled.and.is_fullscreen)then
       call gui_window_move(window,0.1,0.1)
    endif

    return
  end subroutine select_point_open


  logical function select_point_process_events()
!
! Open a window that can be used to go to specific coordinates
!    
    implicit none
    real                :: x,y,z
    integer             :: ios1, ios2, ios3
    character(len=100)  :: bt

    select_point_process_events = .false.

    ! Close the window
    if(gui_window_closed(window))then
       call gui_destroy_window(window)
       window_open = .false.
    endif

    ! Go to the specified coordinates
    if(gui_button_clicked(go_button))then
       if(particle_store_loaded(pdata))then
          call gui_entrybox_get_value(xcoord_box, x, ios1)
          call gui_entrybox_get_value(ycoord_box, y, ios2)
          call gui_entrybox_get_value(zcoord_box, z, ios3)
          if(ios1.ne.0.or.ios2.ne.0.or.ios3.ne.0)then
             bt=gui_display_dialog(window,"error",&
                  "Unable to interpret text as a coordinate")
          else
             ! Apply the new coordinates
             call transform_modify(view_transform, centre=real((/x,y,z/),&
             kind=pos_kind))
             select_point_process_events = .true.
          endif
       endif
    endif

    return
  end function select_point_process_events


  subroutine select_point_close()
!
! Close the window
!
    implicit none

    if(window_open)then
       call gui_destroy_window(window)
       window_open=.false.
    endif

    return
  end subroutine select_point_close

end module select_point
