module progress_bar

  use f90_gui
  use stereo

  implicit none
  private
  save

  public :: progress_bar_init
  public :: progress_bar_display
  public :: progress_bar_update
  public :: progress_bar_close
  public :: progress_bar_displayed
  public :: progress_bar_cancelled

  logical :: display_progress_bar = .false.
  type(gui_window), pointer :: main_window

  ! Window to contain the progress bar
  type (gui_window)       :: pbar_window
  type (gui_progress_bar) :: pbar

  ! Whether the window is open
  logical :: window_open = .false.

  ! Keep track of if the progress bar is already displayed. Routines that
  ! display the progress bar may call other routines which would normally
  ! also show a progress bar. Only the outermost routine should display
  ! anything in this case.
  integer :: nesting_level = 0

  ! Button to cancel the operation
  type(gui_button) :: cancel_button

  ! Whether the progress bar has a cancel button
  logical :: have_cancel_button

contains

  subroutine progress_bar_init(window)

    implicit none
    type(gui_window), target :: window
    
    ! Save a pointer to the main window
    main_window => window
    display_progress_bar = .true.
    nesting_level = 0

    return
  end subroutine progress_bar_init



  subroutine progress_bar_display(message, cancel)
!
! Display a window with a progress bar
!
    implicit none
    character(len=*)  :: message
    type(gui_box)     :: vbox, hbox, hbox2
    type(gui_label)   :: label
    logical, optional :: cancel

    if(.not.display_progress_bar)return

    nesting_level = nesting_level + 1
    if(nesting_level.gt.1)return

    if(present(cancel))then
       have_cancel_button = cancel
    else
       have_cancel_button = .false.
    endif

    call gui_create_window(pbar_window, title="Progress", &
         decorated=.true., parent=main_window)
    call gui_packing_mode(expand=.true.,fill=.true., spacing=10, &
         position=gui_start)
    call gui_create_box(hbox,pbar_window,gui_horizontal)
    call gui_create_box(vbox,hbox,gui_vertical)
    call gui_create_label(label,vbox,message)
    call gui_create_progress_bar(pbar, vbox)
    call gui_progress_bar_set(pbar,0.0)
    if(have_cancel_button)then
       call gui_packing_mode(spacing=3)
       call gui_create_box(hbox2, vbox, gui_horizontal)
       call gui_packing_mode(expand=.false., fill=.false.,position=gui_end)
       call gui_create_button(cancel_button,hbox2,"Cancel")
    endif
    call gui_show_window(pbar_window)
    call gui_set_sensitive(main_window,.FALSE.)
    
    ! Display on LHS in stereo mode
    if(stereo_enabled.and.is_fullscreen) &
         call gui_window_move(pbar_window,0.1,0.1)

    window_open = .true.

    return
  end subroutine progress_bar_display



  subroutine progress_bar_update(f)
!
! Update the progress bar. Also check if the main window
! has been closed and terminate if it has.
!
    implicit none
    real, intent(in) :: f

    if(.not.display_progress_bar)return

    if(.not.window_open)call terminate('Progress bar window not open!')

    if(nesting_level.ne.1)then
       ! Nested calls to the progress bar module will redraw
       ! the window without altering the displayed value
       call gui_redraw_window(pbar_window)
       return
    else
       call gui_progress_bar_set(pbar,f)
       call gui_redraw_window(pbar_window)
    endif

    if(gui_window_closed(main_window))then
       call terminate()
    endif

    return
  end subroutine progress_bar_update



  subroutine progress_bar_close()
!
! Close the progress bar window
!
    implicit none

    if(.not.display_progress_bar)return

    if(.not.window_open)call terminate('Progress bar window not open!')

    nesting_level = nesting_level - 1
    if(nesting_level.gt.0)return

    call gui_destroy_window(pbar_window)
    call gui_set_sensitive(main_window,.TRUE.)

    window_open = .false.

    return
  end subroutine progress_bar_close



  logical function progress_bar_displayed()
!
! Returns true if the progress bar is being displayed
!
    implicit none

    progress_bar_displayed = window_open

    return
  end function progress_bar_displayed



  logical function progress_bar_cancelled()
!
! Return true if the cancel button is pressed
!
    implicit none

    if(have_cancel_button.and.nesting_level.eq.1 &
         .and.display_progress_bar)then
       progress_bar_cancelled = gui_button_clicked(cancel_button)
    else
       progress_bar_cancelled = .false.
    endif

    return
  end function progress_bar_cancelled


end module progress_bar
