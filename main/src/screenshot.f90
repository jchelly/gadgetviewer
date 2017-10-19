module screenshot
!
! Module to write out screenshots as PNG files
!
  use f90_gui
  use f90_util
  use particle_store
  use view_parameters
  use return_status
  use sampling
  use plotter

  implicit none
  private
  save

  public :: screenshot_open
  public :: screenshot_close
  public :: screenshot_process_events

  ! The window
  logical          :: is_open = .false.
  type(gui_window) :: window

  ! Widgets
  type (gui_entrybox)     :: fname_box
  type (gui_button)       :: browse_button, ok_button
  type (gui_radio_button) :: displayed_button, generate_button
  type (gui_entrybox)     :: width_box, height_box, maxpart_box

  ! Settings
  integer            :: img_width  = 640
  integer            :: img_height = 480
  integer            :: maxpart    = 500000
  character(len=500) :: fname      = "screenshot.png"
  logical            :: generate_new = .false.

contains

  subroutine screenshot_open(mainwin)
!
! Open the screenshot window
!
    implicit none
    type (gui_window) :: mainwin
    type (gui_label)  :: label
    type (gui_box)    :: hbox, vbox
    type (gui_box)    :: image_box
    character(len=20) :: str

    if(is_open)return

    call gui_packing_mode(expand=.true., fill=.true., spacing=3, &
         position=gui_start)

    call gui_create_window(window, title="Save image as PNG file", &
         parent=mainwin)
    call gui_create_box(hbox, window, gui_horizontal)
    call gui_create_box(vbox, hbox,   gui_vertical)

    ! Image options
    call gui_create_box(image_box, vbox, gui_vertical, frame=.true., &
         label="Image to write out")
    call gui_create_radio_button(displayed_button, image_box, &
         "Currently displayed image")
    call gui_create_radio_button(generate_button, image_box,&
         "Generate a new image", previous=displayed_button)
    if(generate_new)then
       call gui_radio_button_set_state(generate_button, .true.)
    else
       call gui_radio_button_set_state(displayed_button, .true.)
    endif

    ! Resolution
    call gui_create_box(hbox, image_box, gui_horizontal)
    call gui_packing_mode(position=gui_end, expand=.false., fill=.false.)
    call gui_create_entrybox(height_box, hbox, 5)
    call gui_create_label(label, hbox, " by ")
    call gui_create_entrybox(width_box, hbox, 5)
    call gui_create_label(label, hbox, "Dimensions:")
    call gui_packing_mode(position=gui_start, expand=.true., fill=.true.)

    ! Maximum number of particles
    call gui_create_box(hbox, image_box, gui_horizontal)
    call gui_packing_mode(position=gui_end, expand=.false., fill=.false.)
    call gui_create_entrybox(maxpart_box, hbox, 10)
    call gui_create_label(label, hbox, "Max. particles to use:")
    call gui_packing_mode(position=gui_start, expand=.true., fill=.true.)

    ! Filename
    call gui_create_box(hbox, vbox, gui_horizontal)
    call gui_create_label(label, hbox, "Filename:")
    call gui_packing_mode(expand=.true.,fill=.true.)
    call gui_create_entrybox(fname_box, hbox)
    call gui_packing_mode(expand=.false.,fill=.false.)
    call gui_create_button(browse_button, hbox, "Browse")

    ! Ok button
    call gui_create_box(hbox, vbox, gui_horizontal)
    call gui_packing_mode(position=gui_end)
    call gui_create_button(ok_button, hbox, "Write PNG file")
    call gui_packing_mode(position=gui_start)

    call gui_show_window(window)

    is_open = .true.

    write(str,'(1i20)')img_width
    call gui_entrybox_set_text(width_box, trim(adjustl(str)))
    write(str,'(1i20)')img_height
    call gui_entrybox_set_text(height_box, trim(adjustl(str)))
    write(str,'(1i20)')maxpart
    call gui_entrybox_set_text(maxpart_box, trim(adjustl(str)))
    call gui_entrybox_set_text(fname_box, fname)

    call screenshot_update_window()

    return
  end subroutine screenshot_open


  subroutine screenshot_close()
!
! Close the screenshot window
!
    implicit none

    if(.not.is_open)return

    call gui_destroy_window(window)

    is_open = .false.

    return
  end subroutine screenshot_close


  subroutine screenshot_process_events(mainwin, width, height, image)
!
! Respond to events in the screenshot window
!
    implicit none
    type (gui_window) :: mainwin
    logical :: ok
    logical :: state
    integer :: width, height
    character, dimension(:) :: image
    integer :: ios
    character(len=10)  :: bt
    integer :: ndisplay
    character, dimension(:), allocatable :: new_image
    type (result_type) :: res

    if(.not.is_open)return

    if(gui_window_closed(window))call screenshot_close()

    ! Pop up file selector if browse button is clicked
    if(gui_button_clicked(browse_button))then
       call gui_select_file(mainwin, &
            "Choose base name for output images", &
            gui_file_save, ok, fname)
       if(ok)then
          call gui_entrybox_set_text(fname_box, trim(fname))
       endif
    endif

    if(gui_radio_button_changed(displayed_button))then
       call screenshot_update_window()
    endif

    ! Make the image if the ok button is pressed
    if(gui_button_clicked(ok_button))then

       if(.not.particle_store_loaded(psample))then
          bt=gui_display_dialog(mainwin,"error", &
               "No particle data loaded")
          return
       endif

       ! Get settings from window
       call gui_entrybox_get_text(fname_box,   fname)
       call gui_radio_button_get_state(displayed_button, state)

       ! Save the image
       generate_new = (.not.state)
       if(state)then
          ! Image should already have been generated
          !if(.not.allocated(image))stop &
          !     'Image not allocated when writing out screenshot'
          if(write_png(fname,width,height,image))then
             ! Success
             bt=gui_display_dialog(mainwin,"info", &
                  "Wrote file "//trim(fname))
             call screenshot_close()
          else
             ! Failure
             bt=gui_display_dialog(mainwin,"error", &
                  "Unable to write file "//trim(fname))
          endif
       else

          ! Make a new image with settings from the window
          call gui_entrybox_get_value(width_box,  img_width, ios)
          if(ios.ne.0)then
             bt=gui_display_dialog(mainwin,"error", &
                  "Unable to interpret width as integer")
             return
          endif
          call gui_entrybox_get_value(height_box, img_height, ios)
          if(ios.ne.0)then
             bt=gui_display_dialog(mainwin,"error", &
                  "Unable to interpret height as integer")
             return
          endif
          call gui_entrybox_get_value(maxpart_box, maxpart, ios)
          if(ios.ne.0)then
             bt=gui_display_dialog(mainwin,"error", &
                  "Unable to interpret height as integer")
             return
          endif

          ! Check values are sane
          if(img_width.lt.100.or.img_width.gt.10000)then
             bt=gui_display_dialog(mainwin,"error", &
                  "Image width must be in range 100-10000")
             return
          endif
          if(img_height.lt.100.or.img_height.gt.10000)then
             bt=gui_display_dialog(mainwin,"error", &
                  "Image height must be in range 100-10000")
             return
          endif
          if(maxpart.lt.1)then
             bt=gui_display_dialog(mainwin,"error", &
                  "Maximum number of particles must be a positive integer")
             return
          endif

          call screenshot_close()

          ! Allocate image
          allocate(new_image(img_width*img_height*3), stat=ios)
          if(ios.ne.0)then
             bt=gui_display_dialog(mainwin,"error", &
                  "Unable to allocate memory for image")
             return
          endif
          
          ! Resample the particles with the requested sampling rate
          ndisplay     = npmax_display
          npmax_display = maxpart
          res = sample_region(keep_coords=.true.)
          if(.not.res%success)then
             bt=gui_display_dialog(mainwin,"error", &
                  res%string)
             call particle_store_empty(pdata)
             call particle_store_empty(psample)
             return
          endif
          
          ! Make the image
          call plotter_make_image(img_width, img_height, view_transform, &
               new_image)

          ! Write out the image
          if(write_png(fname,img_width,img_height,new_image))then
             ! Success
             bt=gui_display_dialog(mainwin,"info", &
                  "Wrote file "//trim(fname))
          else
             ! Failure
             bt=gui_display_dialog(mainwin,"error", &
                  "Unable to write file "//trim(fname))
          endif

          ! Discard the image
          deallocate(new_image)
          
          ! Restore original sampling rate
          npmax_display = ndisplay
          res = sample_region(keep_coords=.true.)
          if(.not.res%success)then
             bt=gui_display_dialog(mainwin,"error", &
                  res%string)
             call particle_store_empty(pdata)
             call particle_store_empty(psample)
             return
          endif 

       endif
    endif

    return
  end subroutine screenshot_process_events


  subroutine screenshot_update_window()
!
! Update the widgets in the screenshot window
!
    implicit none
    logical :: state
    
    call gui_radio_button_get_state(displayed_button, state)       
    call gui_set_sensitive(width_box, .not.state)
    call gui_set_sensitive(height_box, .not.state)
    call gui_set_sensitive(maxpart_box, .not.state)

    return
  end subroutine screenshot_update_window

end module screenshot
