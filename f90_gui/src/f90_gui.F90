module f90_gui

#include "../../config.h"

!
! Simplified Fortran 90 interface to parts of the
! GTK 2.0 GUI library
!
! - JCH, 02/01/2007
!
! List of routines implemented so far:
! ------------------------------------
!
! Event loop:
!
!   gui_init
!   gui_process_events
!   gui_clear_events
!   gui_set_event_handler
!   gui_unset_event_handler
!   gui_main_loop
!
! Windows:
!
!   gui_create_window
!   gui_show_window
!   gui_hide_window
!   gui_destroy_window
!   gui_window_closed
!   gui_window_set_statusbar
!
! Packing boxes: 
!
!   gui_create_box
!   gui_packing_mode
!
! Buttons:
!
!   gui_create_button
!   gui_button_clicked
!
! Labels:
!
!   gui_create_label
!
! Text entry boxes:
!
!   gui_create_entrybox
!   gui_entrybox_changed
!   gui_entrybox_get_text
!   gui_entrybox_set_text
!
! Check boxes:
!
!   gui_create_checkbox
!   gui_checkbox_changed
!   gui_checkbox_get_state
!   gui_checkbox_set_state
!
! Horizontal / vertical sliders:
!
!   gui_create_slider
!   gui_slider_changed
!   gui_slider_get_value
!   gui_slider_set_value
!
! Dialog boxes:
!
!   gui_display_dialog
!
! File selectors:
!
!   gui_select_file
!
! Menus:
!
!   gui_create_menu
!   gui_create_menu_item
!   gui_menu_item_clicked
!   gui_menu_item_changed
!   gui_menu_item_get_state
!   gui_menu_item_set_state
!
! Radio buttons:
!
!   gui_create_radio_button
!   gui_radio_button_changed
!   gui_radio_button_get_state
!   gui_radio_button_set_state
!
! Spin buttons:
!
!   gui_create_spin_button
!   gui_spin_button_changed
!   gui_spin_button_get_value
!   gui_spin_button_set_value
!
! Separators:
!
!   gui_create_separator
!
! Static images/icons, read from .png files:
!
!   gui_create_image
!
! Drawing area:
!
!   gui_create_drawing_area
!   gui_drawing_area_clicked
!   gui_drawing_area_mouse_moved
!   gui_drawing_area_mouse_down
!   gui_drawing_area_get_mouse_pos
!   gui_drawing_area_resized
!   gui_drawing_area_get_size
!   gui_drawing_area_redraw
!
! Routines for drawing on the drawing area:
!
!   gui_draw_image
!
! Combo boxes
!
!   gui_create_combo_box
!   gui_combobox_changed
!   gui_combobox_set_index
!   gui_combobox_get_index
!   gui_combobox_set_text
!
! Colour selector button
!
!   gui_create_colour_button
!   gui_colour_button_set_colour
!   gui_colour_button_get_colour
!
! Showing/hiding/graying out widgets
!
!   gui_set_sensitive
!   gui_set_visible
!
! Notebook widgets
!
!   gui_create_notebook
!   gui_create_box_in_notebook
!   gui_notebook_set_page
!
! To Do
! -----
!
! - Ability to destroy widgets (can only do windows at the moment)
! - Ability to change static images?
!
! * Drawing area optimisation?
!

  use c_types
  use f90_util

  implicit none
  public

!
! ---- Types representing various interface elements ----
!
! The calling program should never need to access the components of
! these directly - use the functions provided by this module instead.
!
  ! Type to store information about a window
  type gui_window
     ! Pointer to a GtkWidget object corresponding to this window
     integer(kind=C_PTR) :: ptr
     ! An 'int' that will be set to 1 if the close button is clicked
     integer(kind=C_INT) :: close
     ! Pointer to the box in the window where widgets should be put
     ! (this is inside a vbox which also contains the menubar and status bar)
     integer(kind=C_PTR) :: box
     ! Pointer to the status bar
     logical :: has_statusbar
     integer(kind=C_PTR) :: statusbar
     integer(kind=C_INT) :: contextid
     ! Pointer to the menubar
     logical :: has_menubar
     integer(kind=C_PTR) :: menubar
     ! Pointer to accelerator group
     logical :: has_accel_group
     integer(kind=C_PTR) :: accel_group
  end type gui_window

  ! Type to store information about a 'box' - a container used
  ! to store other interface elements arranged horizontally or
  ! vertically.
  type gui_box
     ! Pointer to the GtkWidget object
     integer(kind=C_PTR) :: ptr
     ! An 'int' that is 0 for a hbox and 1 for a vbox
     integer(kind=C_INT) :: is_vbox
  end type gui_box

  ! Type to store information about a button
  type gui_button
     integer(kind=C_PTR) :: ptr
     integer(kind=C_INT) :: clicked
  end type gui_button

  ! Type to store information about a label
  type gui_label
     integer(kind=C_PTR) :: ptr
  end type gui_label

  ! Type to store information about a text entry box
  type gui_entrybox
     integer(kind=C_PTR) :: ptr
     integer(kind=C_INT) :: changed
  end type gui_entrybox

  ! Type to store information about a checkbox
  type gui_checkbox
     integer(kind=C_PTR) :: ptr
     integer(kind=C_INT) :: changed
  end type gui_checkbox

  ! Type to store information about a slider
  type gui_slider
     integer(kind=C_PTR) :: ptr
     integer(kind=C_INT) :: changed
     integer(kind=C_INT) :: is_vslider
  end type gui_slider

  ! Type to store information about a menu
  type gui_menu
     integer(kind=C_PTR) :: ptr
  end type gui_menu

  ! Type to store information about an item in a menu
  type gui_menu_item
     integer(kind=C_PTR) :: ptr
     integer(kind=C_INT) :: clicked
     integer(kind=C_INT) :: changed
     logical             :: is_checkbox
     logical             :: is_radiobutton
  end type gui_menu_item

  ! Type to store information about a radio button
  type gui_radio_button
     integer(kind=C_PTR) :: ptr
     integer(kind=C_PTR) :: prevbutton
     integer(kind=C_INT) :: changed
     logical :: has_next
  end type gui_radio_button

  ! Type to store information about a spin button
  type gui_spin_button
     integer(kind=C_PTR) :: ptr
     integer(kind=C_INT) :: changed
     real                :: last_value
  end type gui_spin_button

  ! Type to store information about a separator
  ! (these are just decorative)
  type gui_separator
     integer(kind=C_PTR) :: ptr
  end type gui_separator

  ! Type to store information about a drawing area
  type gui_drawing_area
     integer(kind=C_PTR) :: ptr
     ! Pointer to the backing store pixmap
     integer(kind=C_PTR) :: pixmap
     ! This stores the mouse state - coordinates, button clicked
     ! flags, mouse moved flag and button states (up or down)
     ! Mouse_state array elements:
     !
     ! Element    Meaning
     ! ------------------
     ! 1          Moved flag (non-zero = moved)
     ! 2          x coord in drawing area ([0,0] = top left)
     ! 3          y coord in drawing area
     ! 4-8        Button clicked flags
     ! 9-13       Button state flags
     integer(kind=C_INT) :: mouse_state(13)
     integer(kind=C_INT) :: resized
     integer(kind=C_INT) :: width, height
     ! Pointer to the graphics context
     integer(kind=C_PTR) :: gc
     logical             :: gc_created
  end type gui_drawing_area

!
! Type to store information about an image
!
  type gui_image
     integer(kind=C_PTR) :: ptr
  end type gui_image

!
! Type to store information about a combo box
!
  type gui_combo_box
     integer(kind=C_PTR) :: ptr
     integer(kind=C_INT) :: changed
     integer             :: nlines
  end type gui_combo_box

!
! Type to store information about a colour button
!
  type gui_colour_button
     integer(kind=C_PTR) :: ptr
     integer(kind=C_INT) :: changed
  end type gui_colour_button

!
! Type to store information about a progress bar
!
  type gui_progress_bar
     integer(kind=C_PTR) :: ptr
  end type gui_progress_bar
!
! Type to store information about a notebook
!
  type gui_notebook
     integer(kind=C_PTR) :: ptr
  end type gui_notebook

!
! Type to store information about a textview
!
  type gui_textview
     integer(kind=C_PTR) :: ptr
     integer(kind=C_PTR) :: textbuffer
  end type gui_textview

!
! ---- Interfaces for functions that take variable argument types ----
!

  ! Boxes can be created in windows or in other boxes
  private :: create_box_in_window, create_box_in_box
  interface gui_create_box
     module procedure create_box_in_window
     module procedure create_box_in_box
  end interface

  ! Slider ranges can be real or integer
  private :: create_integer_slider, create_real_slider
  interface gui_create_slider
     module procedure create_integer_slider
     module procedure create_real_slider
  end interface

  ! Slider return values can also be real or integer
  private :: slider_get_integer_value, slider_get_real_value
  interface gui_slider_get_value
     module procedure slider_get_real_value
     module procedure slider_get_integer_value
  end interface

  ! Slider set values can also be real or integer
  private :: slider_set_integer_value, slider_set_real_value
  interface gui_slider_set_value
     module procedure slider_set_real_value
     module procedure slider_set_integer_value
  end interface

  ! Spin button ranges can be real or integer
  private :: create_integer_spin_button, create_real_spin_button
  interface gui_create_spin_button
     module procedure create_integer_spin_button
     module procedure create_real_spin_button
  end interface

  ! Spin button return values can also be real or integer
  private :: spin_button_get_integer_value, spin_button_get_real_value
  interface gui_spin_button_get_value
     module procedure spin_button_get_real_value
     module procedure spin_button_get_integer_value
  end interface

  ! Spin button set values can also be real or integer
  private :: spin_button_set_integer_value, spin_button_set_real_value
  interface gui_spin_button_set_value
     module procedure spin_button_set_real_value
     module procedure spin_button_set_integer_value
  end interface

  ! A menu can be created in a window or as part of another menu
  private :: create_menu_in_window, create_menu_in_menu
  interface gui_create_menu
     module procedure create_menu_in_window
     module procedure create_menu_in_menu
  end interface

  ! Various sorts of widgets can be greyed out
  private :: set_sensitive_window, set_sensitive_box, &
             set_sensitive_button, set_sensitive_entrybox, &
             set_sensitive_checkbox, set_sensitive_slider, &
             set_sensitive_menu, set_sensitive_menu_item, &
             set_sensitive_radio_button, set_sensitive_spin_button

  interface gui_set_sensitive
     module procedure set_sensitive_window
     module procedure set_sensitive_box
     module procedure set_sensitive_button
     module procedure set_sensitive_entrybox
     module procedure set_sensitive_checkbox
     module procedure set_sensitive_slider
     module procedure set_sensitive_menu
     module procedure set_sensitive_menu_item
     module procedure set_sensitive_radio_button
     module procedure set_sensitive_spin_button
     module procedure set_sensitive_combo_box
     module procedure set_sensitive_colour_button
  end interface


  interface gui_set_visible
     module procedure set_visible_window
     module procedure set_visible_box
     module procedure set_visible_button
     module procedure set_visible_entrybox
     module procedure set_visible_checkbox
     module procedure set_visible_slider
     module procedure set_visible_menu
     module procedure set_visible_menu_item
     module procedure set_visible_radio_button
     module procedure set_visible_spin_button
     module procedure set_visible_combo_box
     module procedure set_visible_colour_button
  end interface

  interface gui_entrybox_get_value
     module procedure gui_entrybox_get_integer
     module procedure gui_entrybox_get_real
  end interface

!
! ---- Constants used as flags in subroutine calls ----
!

  ! Constants for specifying orientation of a widget
  type gui_orientation
     integer :: id
  end type gui_orientation
  type(gui_orientation), parameter :: gui_horizontal = gui_orientation(1)
  type(gui_orientation), parameter :: gui_vertical   = gui_orientation(2)

  ! Constants for specifying packing positions for widgets
  type gui_position
     integer :: id
  end type gui_position
  type(gui_position), parameter    :: gui_start = gui_position(1)
  type(gui_position), parameter    :: gui_end   = gui_position(2)

  ! Constants for specifying mode for file selector
  type gui_file_mode
     integer :: id
  end type gui_file_mode
  type (gui_file_mode), parameter  :: gui_file_open  = gui_file_mode(1)
  type (gui_file_mode), parameter  :: gui_file_save  = gui_file_mode(2)

contains

!
! Initialise GTK
!
  subroutine gui_init

    implicit none

    call noeventhandler()
    call startgtk()

    return
  end subroutine gui_init

!
! Wait for (then process) events - needs to be called continuously for the
! application to remain responsive.
!
  subroutine gui_process_events()

    implicit none

    call iterategtk()

    return
  end subroutine gui_process_events

!
! Process all pending events
!
  subroutine gui_clear_events()
    
    implicit none

    call clearallevents()

    return
  end subroutine gui_clear_events

!
! Create a new window
!
! Registers a GTK callback function that sets window%close=1 if
! the close button has been clicked. Check for this with gui_window_closed(),
! which returns true if the close button has been clicked. It also clears the
! close flag ready for the next iteration.
!
! If menubar = .true., menus can be added to the window.
! If statusbar = .true. the window will have a status bar at the bottom.
! Put text in the statusbar with gui_window_set_statusbar().
!
  subroutine gui_create_window(window, title, dimensions, min_dimensions, &
       statusbar, menubar, decorated, parent, resize)

    implicit none
    type (gui_window) :: window
    integer, dimension(2), optional :: dimensions, min_dimensions
    integer(kind=C_INT) :: xsize, ysize, min_xsize, min_ysize
    character(len=*), optional :: title
    logical, optional :: statusbar, menubar, decorated, resize
    integer(kind=C_INT) :: c_statusbar, c_menubar, c_decorated, c_resize
    type(gui_window), optional :: parent

    c_statusbar = 0
    window%has_statusbar = .false.
    if(present(statusbar))then
       if(statusbar)c_statusbar = 1
       window%has_statusbar = .true.
    endif

    c_menubar = 0
    window%has_menubar = .false.
    if(present(menubar))then
       if(menubar)c_menubar = 1
       window%has_menubar = .true.
    endif

    c_decorated = 1
    if(present(decorated))then
       if(.not.decorated)c_decorated = 0
    endif

    c_resize = 1
    if(present(resize))then
       if(.not.resize)c_resize = 0
    endif

    if(present(dimensions))then
       xsize = dimensions(1)
       ysize = dimensions(2)
    else
       xsize = -1
       ysize = -1
    endif

    if(present(min_dimensions))then
       min_xsize = min_dimensions(1)
       min_ysize = min_dimensions(2)
    else
       min_xsize = -1
       min_ysize = -1
    endif

    if(present(title))then
       call createwindow(window%ptr,window%close,window%box, &
            xsize,ysize,min_xsize,min_ysize,trim(title)//char(0), &
            c_statusbar,window%statusbar,window%contextid, &
            c_menubar, window%menubar, c_decorated, c_resize)
    else
       call createwindow(window%ptr,window%close,window%box, &
            xsize,ysize,min_xsize,min_ysize," "//char(0), &
            c_statusbar,window%statusbar,window%contextid, &
            c_menubar, window%menubar, c_decorated, c_resize)
    endif

    if(present(parent))then
       call setparent(window%ptr,parent%ptr)
    endif

    window%close = 0
    window%has_accel_group = .false.

    return
  end subroutine gui_create_window


  subroutine gui_window_move(window,x,y)
!
! Move a window to the specified coordinates
!
    implicit none
    type(gui_window) :: window
    real             :: x, y
    real(kind=C_DOUBLE) :: x_c, y_c
    
    x_c = x
    y_c = y
    call movewindow(window%ptr,x_c,y_c)

    return
  end subroutine gui_window_move

!
! Show a GTK window
!
  subroutine gui_show_window(window)

    implicit none
    type(gui_window) :: window

    call showwindow(window%ptr)

    return
  end subroutine gui_show_window

!
! Hide a GTK window
!
  subroutine gui_hide_window(window)

    implicit none
    type(gui_window) :: window

    call hidewindow(window%ptr)

    return
  end subroutine gui_hide_window

!
! Redraw a GTK window
!
  subroutine gui_redraw_window(window)

    implicit none
    type(gui_window) :: window

    call redrawwindow(window%ptr)
    call gui_clear_events()

    return
  end subroutine gui_redraw_window

!
! Destroy a GTK window and any widgets it contains
!
  subroutine gui_destroy_window(window)

    implicit none
    type(gui_window) :: window

    call destroywindow(window%ptr)

    return
  end subroutine gui_destroy_window

!
! Check if the close button on a window has been
! clicked
!
  logical function gui_window_closed(window)
    
    implicit none
    type (gui_window) :: window

    if(window%close.ne.0)then
       gui_window_closed = .true.
    else
       gui_window_closed = .false.
    endif
    window%close = 0

    return
  end function gui_window_closed


!
! Routines to create boxes to contain other widgets
! Should only be called via gui_create_box().
!
  subroutine create_box_in_window(box,window,orientation,frame,label, &
       scrollable, expander)

    implicit none
    type (gui_window) :: window
    type (gui_box)    :: box
    type (gui_orientation) :: orientation
    logical, optional      :: frame
    integer(kind=C_INT)    :: c_frame, c_has_label
    character(len=*), optional :: label
    logical, optional :: scrollable
    integer(kind=C_INT) :: c_scroll
    logical, optional :: expander
    integer(kind=C_INT) :: c_expand

    c_expand = 0
    if(present(expander))then
       if(expander)c_expand=1
       if(expander.and.(.not.present(label)))&
            call terminate('create_box(): Label must be specified if exapnder=.true.')
    endif

    c_scroll = 0
    if(present(scrollable))then
       if(scrollable)c_scroll = 1
    endif

    c_has_label = 0
    if(present(label))then
       c_has_label = 1
    endif

    c_frame = 0
    if(present(frame))then
       if(frame)c_frame = 1
    endif

    if(orientation%id.eq.gui_vertical%id)then
       box%is_vbox = 1
    else if(orientation%id.eq.gui_horizontal%id)then
       box%is_vbox = 0
    else
       write(0,*)'gui_create_box(): Unrecognised orientation!'
    endif

    if(present(label))then
       if(box%is_vbox.eq.0)then
          call createhbox(box%ptr,window%box,c_frame,c_has_label, &
               trim(label)//char(0),c_scroll, c_expand)
       else
          call createvbox(box%ptr,window%box,c_frame,c_has_label, &
               trim(label)//char(0),c_scroll, c_expand)
       endif
    else
       if(box%is_vbox.eq.0)then
          call createhbox(box%ptr,window%box,c_frame,c_has_label, &
               "null",c_scroll, c_expand)
       else
          call createvbox(box%ptr,window%box,c_frame,c_has_label, &
               "null",c_scroll, c_expand)
       endif
    endif

  end subroutine create_box_in_window

  ! This allows nesting of boxes
  subroutine create_box_in_box(box,inbox,orientation,frame,label,scrollable,&
       expander)

    implicit none
    type (gui_box)         :: box, inbox
    type (gui_orientation) :: orientation
    logical, optional      :: frame
    integer(kind=C_INT)    :: c_frame, c_has_label
    character(len=*), optional :: label
    logical, optional :: scrollable
    integer(kind=C_INT) :: c_scroll
    logical, optional :: expander
    integer(kind=C_INT) :: c_expand

    c_expand = 0
    if(present(expander))then
       if(expander)c_expand=1
       if(expander.and.(.not.present(label)))&
            call terminate('create_box(): Label must be specified if exapnder=.true.')
    endif

    c_scroll = 0
    if(present(scrollable))then
       if(scrollable)c_scroll = 1
    endif

    c_frame = 0
    if(present(frame))then
       if(frame)c_frame = 1
    endif

    c_has_label = 0
    if(present(label))then
       c_has_label = 1
    endif

    if(orientation%id.eq.gui_vertical%id)then
       box%is_vbox = 1
    else if(orientation%id.eq.gui_horizontal%id)then
       box%is_vbox = 0
    else
       write(0,*)'gui_create_box(): Unrecognised orientation!'
    endif

    if(present(label))then
       if(box%is_vbox.eq.0)then
          call createhbox(box%ptr,inbox%ptr, c_frame, c_has_label, &
               trim(label)//char(0), c_scroll, c_expand)
       else
          call createvbox(box%ptr,inbox%ptr, c_frame, c_has_label, &
               trim(label)//char(0), c_scroll, c_expand)
       endif
    else
       if(box%is_vbox.eq.0)then
          call createhbox(box%ptr,inbox%ptr, c_frame, c_has_label, &
               "null", c_scroll, c_expand)
       else
          call createvbox(box%ptr,inbox%ptr, c_frame, c_has_label, &
               "null", c_scroll, c_expand)
       endif
    endif
       

  end subroutine create_box_in_box


!
! Subroutine to create a button in a box
!
  subroutine gui_create_button(button, box, label, image)

    implicit none
    type (gui_button)  :: button
    type (gui_box)     :: box
    character(len=*), optional :: label, image
    
    if((.not.present(image)).and.(.not.present(label)))then
       call terminate("gui_create_button(): button must have a label or an image!")
    endif

    if(present(image).and.present(label))then
       call terminate("gui_create_button(): button must have a label OR an image!")
    endif
    
    if(present(label))then
       call createbutton(button%ptr, button%clicked, box%ptr, &
            trim(label)//char(0))
    else
       call createimagebutton(button%ptr, button%clicked, box%ptr, &
            trim(image)//char(0))
    endif

    button%clicked = 0

    return
  end subroutine gui_create_button

!
! Check if a button was clicked
!
  logical function gui_button_clicked(button)
    
    implicit none
    type (gui_button) :: button
    
    if(button%clicked.eq.0)then
       gui_button_clicked = .false.
    else
       gui_button_clicked = .true.
    endif

    button%clicked = 0
    
    return
  end function gui_button_clicked

!
! Make a label that displays some static text
!
  subroutine gui_create_label(label, box, text)

    implicit none
    type (gui_label) :: label
    type (gui_box)   :: box
    character(len=*) :: text

    call createlabel(label%ptr,box%ptr,trim(text)//char(0))

    return
  end subroutine gui_create_label

!
! Make an editable text entry box
!
  subroutine gui_create_entrybox(entrybox, box, length, max_chars, editable)
    
    implicit none
    type (gui_entrybox) :: entrybox
    type (gui_box)      :: box
    integer, optional   :: length, max_chars
    integer(kind=C_INT) :: c_length, c_editable, c_max_chars
    logical, optional   :: editable

    if(present(max_chars))then
       c_max_chars = max_chars
    else
       c_max_chars = -1
    endif

    if(present(length))then
       c_length = length
    else
       c_length = -1
    endif

    if(present(editable))then
       if(editable)then
          c_editable = 1
       else
          c_editable = 0
       endif
    else
       c_editable = 1
    endif

    call createentrybox(entrybox%ptr,box%ptr,c_length,entrybox%changed, &
         c_editable, c_max_chars)
    entrybox%changed = 0

    return
  end subroutine gui_create_entrybox

!
! Check if the value in an entry box has been changed
! (this only returns true when the value is confirmed by pressing enter)
!
  logical function gui_entrybox_changed(entrybox)

    implicit none
    type (gui_entrybox) :: entrybox

    if(entrybox%changed.ne.0)then
       gui_entrybox_changed = .true.
    else
       gui_entrybox_changed = .false.
    endif
    entrybox%changed = 0

    return
  end function gui_entrybox_changed

!
! Get the string from a text entry box
!
  subroutine gui_entrybox_get_text(entrybox, text)
    
    implicit none
    character(len=*) :: text
    type(gui_entrybox) :: entrybox
    integer(kind=C_INT) :: string_length
    
    string_length = len(text)
    call getentryboxtext(entrybox%ptr,string_length,text)

    return
  end subroutine gui_entrybox_get_text

!
! Get a number from a text entry box
!
  subroutine gui_entrybox_get_integer(entrybox, value, iostat)
    
    implicit none
    integer             :: value
    type(gui_entrybox)  :: entrybox
    integer             :: iostat
    integer(kind=C_INT) :: string_length
    character(len=500)  :: text
    double precision    :: r

    string_length = len(text)
    call getentryboxtext(entrybox%ptr,string_length,text)
    
    ! First read as double precision to check its in range
    read(text,*,iostat=iostat)r
    if(iostat.ne.0)return
    if(abs(r).gt.0.999*huge(value))then
       iostat = -1
       return
    endif

    ! Then read as an integer
    read(text,*,iostat=iostat)value
    if(iostat.ne.0)return

    iostat = 0

    return
  end subroutine gui_entrybox_get_integer

!
! Get a number from a text entry box
!
  subroutine gui_entrybox_get_real(entrybox, value, iostat)
    
    implicit none
    real                :: value
    type(gui_entrybox)  :: entrybox
    integer             :: iostat
    integer(kind=C_INT) :: string_length
    character(len=500)  :: text
    double precision    :: r

    string_length = len(text)
    call getentryboxtext(entrybox%ptr,string_length,text)
    
    ! First read as double precision to check its in range
    read(text,*,iostat=iostat)r
    if(iostat.ne.0)return
    if(abs(r).gt.0.999*huge(value))then
       iostat = -1
       return
    endif

    ! Then read as a real
    read(text,*,iostat=iostat)value
    if(iostat.ne.0)return

    iostat = 0

    return
  end subroutine gui_entrybox_get_real

!
! Put a string in a text entry box
!
  subroutine gui_entrybox_set_text(entrybox, text)

    implicit none
    character(len=*)   :: text
    type(gui_entrybox) :: entrybox

    call setentryboxtext(entrybox%ptr,trim(text)//char(0))
    entrybox%changed = 0

    return
  end subroutine gui_entrybox_set_text

!
! Set parameters relating to box packing - ie layout of 
! widgets in horizontal and vertical boxes.
! These will persist until this routine is called again.
!
  subroutine gui_packing_mode(expand,fill,spacing,position)

    implicit none
    logical, optional            :: expand, fill
    integer, optional            :: spacing
    type(gui_position), optional :: position
    integer(kind=C_INT)          :: c_expand, c_fill, c_spacing, c_position
    
    ! -1 : keep current value
    ! For logical parameters 0 = false, 1 = true
    ! For position 0 = start, 1 = end
    c_expand   = -1
    c_fill     = -1
    c_spacing  = -1
    c_position = -1

    if(present(expand))then
       if(expand)then
          c_expand=1
       else
          c_expand=0
       endif
    endif

    if(present(fill))then
       if(fill)then
          c_fill=1
       else
          c_fill=0
       endif
    endif

    if(present(spacing))then
       c_spacing = spacing
    endif

    if(present(position))then
       if(position%id.eq.gui_start%id)then
          c_position = 0
       else if(position%id.eq.gui_end%id)then
          c_position = 1
       else
          write(0,*)"gui_packing_mode(): unrecognised position!"
       end if
    endif
    
    call packingmode(c_expand,c_fill,c_spacing,c_position)

    return
  end subroutine gui_packing_mode

!
! Create a checkbox
!
  subroutine gui_create_checkbox(checkbox, box, label)

    implicit none
    type (gui_checkbox) :: checkbox
    type (gui_box)      :: box
    character(len=*)    :: label

    call createcheckbox(checkbox%ptr,box%ptr,checkbox%changed, &
         trim(label)//char(0))

    checkbox%changed = 0

    return
  end subroutine gui_create_checkbox

!
! See if the state of a checkbox has changed
!
  logical function gui_checkbox_changed(checkbox)

    implicit none
    type (gui_checkbox) :: checkbox

    if(checkbox%changed.ne.0)then
       gui_checkbox_changed = .true.
    else
       gui_checkbox_changed = .false.
    endif
    checkbox%changed = 0

    return
  end function gui_checkbox_changed

!
! Find out the state of a checkbox
!
  subroutine gui_checkbox_get_state(checkbox, checked)

    implicit none
    type (gui_checkbox) :: checkbox
    logical :: checked
    integer(kind=C_INT) :: ichecked
    
    call checkboxstate(checkbox%ptr,ichecked)

    if(ichecked.eq.0)then
       checked = .false.
    else
       checked = .true.
    endif

    return
  end subroutine gui_checkbox_get_state

!
! Set the state of a checkbox
!
  subroutine gui_checkbox_set_state(checkbox, checked)

    implicit none
    type (gui_checkbox) :: checkbox
    logical :: checked
    integer(kind=C_INT) :: ichecked
    
    if(checked)then
       ichecked = 1
    else
       ichecked = 0
    endif

    call setcheckboxstate(checkbox%ptr,ichecked)

    checkbox%changed=0

    return
  end subroutine gui_checkbox_set_state


!
! Create a slider widget - either horizontal or vertical
!
  subroutine create_integer_slider(slider, box, range, step, orientation, &
       draw_value, min_size)

    implicit none
    type (gui_slider)      :: slider
    type (gui_box)         :: box
    integer, dimension(2)  :: range
    integer                :: step
    type(gui_orientation)  :: orientation
    real(kind=C_DOUBLE)    :: c_min, c_max, c_step
    logical, optional      :: draw_value
    integer(kind=C_INT)    :: c_draw, c_size
    integer, optional      :: min_size

    c_size = 0
    if(present(min_size))c_size=min_size

    if(orientation%id.eq.gui_horizontal%id)then
       slider%is_vslider = 0
    else if(orientation%id.eq.gui_vertical%id)then
       slider%is_vslider = 1
    else
       call terminate("gui_create_slider(): Unrecognised orientation for slider:")
    end if

    c_draw = 0
    if(present(draw_value))then
       if(draw_value)c_draw = 1
    endif

    c_min  = dble(minval(range))
    c_max  = dble(maxval(range))
    c_step = dble(step)

    if(slider%is_vslider.ne.0)then
       call createvslider(slider%ptr, box%ptr, slider%changed, &
            c_min, c_max, c_step, c_draw, c_size)
    else
       call createhslider(slider%ptr, box%ptr, slider%changed, &
            c_min, c_max, c_step, c_draw, c_size)
    endif

    slider%changed = 0
    
    return
  end subroutine create_integer_slider

  subroutine create_real_slider(slider, box, range, step, orientation, &
       draw_value, min_size)

    implicit none
    type (gui_slider)     :: slider
    type (gui_box)        :: box
    real, dimension(2)    :: range
    real                  :: step
    type(gui_orientation) :: orientation
    real(kind=C_DOUBLE)   :: c_min, c_max, c_step
    logical, optional     :: draw_value
    integer(kind=C_INT)   :: c_draw, c_size
    integer, optional      :: min_size

    c_size = 0
    if(present(min_size))c_size=min_size

    if(orientation%id.eq.gui_horizontal%id)then
       slider%is_vslider = 0
    else if(orientation%id.eq.gui_vertical%id)then
       slider%is_vslider = 1
    else
       call terminate("gui_create_slider(): Unrecognised orientation for slider:")
    end if

    c_draw = 0
    if(present(draw_value))then
       if(draw_value)c_draw = 1
    endif

    c_min  = dble(minval(range))
    c_max  = dble(maxval(range))
    c_step = dble(step)

    if(slider%is_vslider.ne.0)then
       call createvslider(slider%ptr, box%ptr, slider%changed, &
            c_min, c_max, c_step, c_draw, c_size)
    else
       call createhslider(slider%ptr, box%ptr, slider%changed, &
            c_min, c_max, c_step, c_draw, c_size)
    endif

    slider%changed = 0
    
    return
  end subroutine create_real_slider

!
! Get the current value of a slider
!
  subroutine slider_get_real_value(slider, value)

    implicit none
    type (gui_slider)   :: slider
    real                :: value
    real(kind=C_DOUBLE) :: c_value
    
    call getslidervalue(slider%ptr, c_value)
    
    value = real(c_value,kind(value))

    return
  end subroutine slider_get_real_value

  subroutine slider_get_integer_value(slider, value)

    implicit none
    type (gui_slider)   :: slider
    integer             :: value
    real(kind=C_DOUBLE) :: c_value
    
    call getslidervalue(slider%ptr, c_value)
    
    value = int(c_value,kind(value))

    return
  end subroutine slider_get_integer_value

!
! Set the value of a slider
!
  subroutine slider_set_real_value(slider, value)

    implicit none
    type (gui_slider)   :: slider
    real                :: value
    real(kind=C_DOUBLE) :: c_value
    
    c_value = real(value,kind=C_DOUBLE)
    call setslidervalue(slider%ptr, c_value)
    slider%changed = 0

    return
  end subroutine slider_set_real_value

  subroutine slider_set_integer_value(slider, value)

    implicit none
    type (gui_slider)   :: slider
    integer             :: value
    real(kind=C_DOUBLE) :: c_value
    
    c_value = real(value,kind=C_DOUBLE)
    call setslidervalue(slider%ptr, c_value)
    slider%changed = 0

    return
  end subroutine slider_set_integer_value

!
! Check if the value of a slider has changed
!
  logical function gui_slider_changed(slider)

    implicit none
    type (gui_slider) :: slider

    if(slider%changed.ne.0)then
       gui_slider_changed = .true.
    else
       gui_slider_changed = .false.
    endif

    slider%changed = 0

    return
  end function gui_slider_changed

!
! Display a message dialogue box and return 'ok', 'cancel', 'yes' or 'no'
! depending on which button is pressed. Returns 'closed' if the window
! is closed without a button being pressed.
!
! This function doesn't return until the dialog box is closed.
!
  function gui_display_dialog(window,type,text)
    
    implicit none
    type (gui_window)   :: window
    character(len=*)    :: text
    integer(kind=C_INT) :: itype, ires
    character(len=*)    :: type
    character(len=10)   :: gui_display_dialog
    
    select case(type)
    case("info")
       itype = 0
    case("warning")
       itype = 1
    case("question")
       itype = 2
    case("error")
       itype = 3
    case default
       call terminate('gui_display_dialog(): unrecognised type '//trim(type))
    end select

    call createdialog(window%ptr,itype,trim(text)//char(0),ires)

    select case(ires)
    case(0)
       gui_display_dialog = "closed"
    case(1)
       gui_display_dialog = "ok"       
    case(2)
       gui_display_dialog = "cancel"
    case(3)
       gui_display_dialog = "yes"
    case(4)
       gui_display_dialog = "no"
    case default
       write(0,*)'gui_display_dialog(): unrecognised response from dialog_run!'
       gui_display_dialog = "unknown"
    end select
    
    return
  end function gui_display_dialog

!
! Display a file selector dialog
! Doesn't return until the dialog box is closed.
!
! Returns ok_clicked = .true. if the ok button (or enter) was pressed.
! ok_clicked = false means the dialog box was closed or canceled. In this
! case the filename is undefined.
!
  subroutine gui_select_file(window, message, mode, ok_clicked, filename)

    implicit none
    type (gui_window) :: window
    character(len=*) :: message
    type (gui_file_mode) :: mode
    logical :: ok_clicked
    character(len=*) :: filename
    integer(kind=C_INT) :: c_mode, string_length,c_ok

    if(mode%id.eq.gui_file_open%id)then
       c_mode = 0
    else if(mode%id.eq.gui_file_save%id)then
       c_mode = 1
    else
       write(0,*)'gui_select_file(): unrecognised mode!'
    end if

    string_length = len(filename)

    call selectfile(window%ptr, trim(message)//char(0), c_mode, c_ok, &
         filename, string_length)

    ! Filename is undefined if ok_clicked = .false.
    if(c_ok.eq.1)then
       ok_clicked = .true.
    else
       ok_clicked = .false.
    endif

    return
  end subroutine gui_select_file

!
! Set the statusbar text in a window
!
  subroutine gui_window_set_statusbar(window, text)

    implicit none
    type (gui_window) :: window
    character(len=*)  :: text

    if(.not.window%has_statusbar)then
       call terminate('gui_window_set_statusbar(): window has no statusbar!')
    endif

    call setstatusbar(window%statusbar, trim(text)//char(0), window%contextid)

    return
  end subroutine gui_window_set_statusbar

!
! Add a menu to a window
!
  subroutine create_menu_in_window(menu,window,name,right_justify)

    implicit none
    type (gui_window) :: window
    type (gui_menu)   :: menu
    character(len=*)  :: name
    logical, optional :: right_justify
    integer(kind=C_INT) :: c_right

    if(.not.window%has_menubar)then
       call terminate('gui_create_menu: window has no menubar!')
    endif

    c_right = 0
    if(present(right_justify))then
       if(right_justify)c_right = 1
    endif

    call createmenu(menu%ptr,window%menubar,trim(name)//char(0),c_right)

    return
  end subroutine create_menu_in_window

!
! Add a submenu as part of another menu
!
  subroutine create_menu_in_menu(menu,inmenu,name,separator)

    implicit none
    type (gui_menu)   :: inmenu
    type (gui_menu)   :: menu
    character(len=*)  :: name
    logical, optional    :: separator
    integer(kind=C_INT)  :: c_separator

    c_separator = 0
    if(present(separator))then
       if(separator)c_separator = 1
    endif
    call createsubmenu(menu%ptr,inmenu%ptr,trim(name)//char(0), &
         c_separator)

    return
  end subroutine create_menu_in_menu

!
! Add an item to a menu
!
! Put a separator above the item if separator=.true.
! Make it a tickable 'checkbox' menu item if checkbox=.true.
!
  subroutine gui_create_menu_item(menuitem, menu, name, separator, &
       checkbox, radiobutton, previous)

    implicit none
    type (gui_menu)      :: menu
    type (gui_menu_item) :: menuitem
    character(len=*)     :: name
    logical, optional    :: separator
    integer(kind=C_INT)  :: c_separator
    logical, optional    :: checkbox
    integer(kind=C_INT)  :: c_type
    logical, optional    :: radiobutton
    type(gui_menu_item), optional  :: previous
    integer(kind=C_PTR)  :: previtem_ptr

    c_separator = 0
    if(present(separator))then
       if(separator)c_separator = 1
    endif

    ! Type 0 is a normal, clickable menu item
    c_type = 0
    menuitem%is_checkbox = .false.
    if(present(checkbox))then
       if(checkbox)then
          ! Type 1 means its a 'checkbox' menu item
          c_type = 1
          menuitem%is_checkbox = .true.
       endif
    endif

    menuitem%is_radiobutton = .false.
    previtem_ptr = 0
    if(present(radiobutton))then
       if(radiobutton)then
          ! Type=2 means the first item in a new radio button group
          c_type = 2
          ! Type=3 means add the item to the same group as previtem
          if(present(previous))then
             c_type = 3
             previtem_ptr = previous%ptr
          endif
          menuitem%is_radiobutton = .true.
       endif
    endif

    if(menuitem%is_checkbox.and.menuitem%is_radiobutton)then
       call terminate("gui_create_menu_item(): item cannot be checkbox AND radiobutton!")
    endif

    call createmenuitem(menuitem%ptr, menuitem%clicked, &
         menu%ptr, trim(name)//char(0),c_separator,c_type,&
         previtem_ptr, menuitem%changed)

    menuitem%clicked = 0
    menuitem%changed = 0

    return
  end subroutine gui_create_menu_item

!
! Check if a menu item has been clicked
!
  logical function gui_menu_item_clicked(menuitem)
  
    implicit none
    type (gui_menu_item) :: menuitem

    if(menuitem%clicked.ne.0)then
       gui_menu_item_clicked = .true.
    else
       gui_menu_item_clicked = .false.
    endif

    menuitem%clicked = 0

    return
  end function gui_menu_item_clicked

!
! Check if a menu item's state has changed (radio or checkbox items only)
!
  logical function gui_menu_item_changed(menuitem)
  
    implicit none
    type (gui_menu_item) :: menuitem

    if(.not.(menuitem%is_checkbox.or.menuitem%is_radiobutton))then
       call terminate('gui_menu_item_changed():'&
            //' only available for checkbox and radiobutton menu items!')
    endif

    if(menuitem%changed.ne.0)then
       gui_menu_item_changed = .true.
    else
       gui_menu_item_changed = .false.
    endif

    menuitem%changed = 0

    return
  end function gui_menu_item_changed

!
! Find out the state of a checkbox or radiobutton menu item
!
  subroutine gui_menu_item_get_state(menuitem, checked)

    implicit none
    type (gui_menu_item) :: menuitem
    logical :: checked
    integer(kind=C_INT) :: c_checked

    if(.not.(menuitem%is_checkbox.or.menuitem%is_radiobutton))then
       call terminate('gui_menu_item_changed():'&
            //' only available for checkbox and radiobutton menu items!')
    endif

    call menuitemstate(menuitem%ptr, c_checked)

    if(c_checked.ne.0)then
       checked = .true.
    else
       checked = .false.
    endif

    return
  end subroutine gui_menu_item_get_state

!
! Set the state of a checkbox or radiobutton menu item
!
  subroutine gui_menu_item_set_state(menuitem, checked)

    implicit none
    type (gui_menu_item) :: menuitem
    logical :: checked
    integer(kind=C_INT) :: c_checked

    if(.not.(menuitem%is_checkbox.or.menuitem%is_radiobutton))then
       call terminate('gui_menu_item_changed():'&
            //' only available for checkbox and radiobutton menu items!')
    endif

    if(checked)then
       c_checked = 1
    else
       c_checked = 0
    end if

    call setmenuitemstate(menuitem%ptr, c_checked)

    menuitem%changed = 0

    return
  end subroutine gui_menu_item_set_state

!
! Create a radio button. To group these together, pass 
! the previous button in the group as the last argument.
!
! To start a new group of buttons, omit the last argument.
!
  subroutine gui_create_radio_button(radiobutton, box, label, previous)

    implicit none
    type (gui_box)          :: box
    type (gui_radio_button) :: radiobutton
    character(len=*)        :: label
    type (gui_radio_button), optional :: previous

    radiobutton%has_next = .false.
    if(.not.present(previous)) then
       call newradiobuttongroup(radiobutton%ptr,box%ptr,trim(label)//char(0),&
            radiobutton%changed)
    else
       if(previous%has_next)then
          call terminate('gui_create_radio_button(): buttons must be linked in a chain')
       endif
       call addradiobutton(radiobutton%ptr,box%ptr,trim(label)//char(0), &
            previous%ptr,radiobutton%changed)
       previous%has_next = .true.
    endif
    
    radiobutton%changed = 0
    if(present(previous))radiobutton%prevbutton = previous%ptr

    return
  end subroutine gui_create_radio_button

!
! Check if the state of a radiobutton has changed
!
  logical function gui_radio_button_changed(radiobutton)

    implicit none
    type (gui_radio_button) :: radiobutton

    if(radiobutton%changed.ne.0)then
       gui_radio_button_changed = .true.
    else
       gui_radio_button_changed = .false.
    endif
    radiobutton%changed = 0
    
    return
  end function gui_radio_button_changed

!
! Determine the state of a radiobutton 
!
  subroutine gui_radio_button_get_state(radiobutton, checked)

    implicit none
    type (gui_radio_button) :: radiobutton
    logical :: checked
    integer(kind=C_INT) :: c_checked

    call radiobuttonstate(radiobutton%ptr,c_checked)

    if(c_checked.eq.0)then
       checked = .false.
    else
       checked = .true.
    end if

    return
  end subroutine gui_radio_button_get_state
!
! Set the state of a radiobutton 
!
  subroutine gui_radio_button_set_state(radiobutton, checked)

    implicit none
    type (gui_radio_button) :: radiobutton
    logical :: checked
    integer(kind=C_INT) :: c_checked

    if(checked)then
       c_checked = 1
    else
       c_checked = 0
    endif
    
    call setradiobuttonstate(radiobutton%ptr,c_checked)

    radiobutton%changed = 0

    return
  end subroutine gui_radio_button_set_state

!
! Create a spin button
!
  subroutine create_integer_spin_button(spinbutton,box,range,step)

    implicit none
    type (gui_spin_button) :: spinbutton
    type (gui_box)         :: box
    integer, dimension(2)  :: range
    integer                :: step
    real(kind=C_DOUBLE)    :: c_min, c_max, c_step
    
    c_min  = dble(minval(range))
    c_max  = dble(maxval(range))
    c_step = dble(step)

    call createspinbutton(spinbutton%ptr,box%ptr,c_min,c_max,c_step, &
         spinbutton%changed)

    spinbutton%changed = 0
    spinbutton%last_value = -2000000000

    return
  end subroutine create_integer_spin_button

  subroutine create_real_spin_button(spinbutton,box,range,step)

    implicit none
    type (gui_spin_button) :: spinbutton
    type (gui_box)         :: box
    real, dimension(2)     :: range
    real                   :: step
    real(kind=C_DOUBLE)    :: c_min, c_max, c_step
    
    c_min  = dble(minval(range))
    c_max  = dble(maxval(range))
    c_step = dble(step)

    call createspinbutton(spinbutton%ptr,box%ptr,c_min,c_max,c_step, &
         spinbutton%changed)

    spinbutton%changed = 0
    spinbutton%last_value = -2000000000

    return
  end subroutine create_real_spin_button

!
! Check if the state of a spin button has changed
!
  logical function gui_spin_button_changed(spinbutton)

    implicit none
    type (gui_spin_button) :: spinbutton
    real :: new_value

    if(spinbutton%changed.ne.0)then
       ! Check if value has really changed - GTK sometimes lies about this
       call spin_button_get_real_value(spinbutton, new_value)
       if(new_value.ne.spinbutton%last_value)then
          gui_spin_button_changed = .true. 
          spinbutton%last_value = new_value
       else
          gui_spin_button_changed = .false.
       endif
    else
       gui_spin_button_changed = .false.
    endif
    spinbutton%changed = 0
    
    return
  end function gui_spin_button_changed

!
! Get the current value of a spin button
!
  subroutine spin_button_get_real_value(spinbutton, value)

    implicit none
    type (gui_spin_button)   :: spinbutton
    real                     :: value
    real(kind=C_DOUBLE)      :: c_value
    
    call getspinbuttonvalue(spinbutton%ptr, c_value)
    
    value = real(c_value,kind(value))

    return
  end subroutine spin_button_get_real_value

  subroutine spin_button_get_integer_value(spinbutton, value)

    implicit none
    type (gui_spin_button)   :: spinbutton
    integer                  :: value
    real(kind=C_DOUBLE)      :: c_value
    
    call getspinbuttonvalue(spinbutton%ptr, c_value)
    
    value = int(c_value,kind(value))

    return
  end subroutine spin_button_get_integer_value

!
! Set the current value of a spin button
!
  subroutine spin_button_set_real_value(spinbutton, value)

    implicit none
    type (gui_spin_button)   :: spinbutton
    real                     :: value
    real(kind=C_DOUBLE)      :: c_value
    
    c_value = real(value, kind=C_DOUBLE)
    call setspinbuttonvalue(spinbutton%ptr, c_value)
    spinbutton%changed = 0
    spinbutton%last_value = value

    return
  end subroutine spin_button_set_real_value

  subroutine spin_button_set_integer_value(spinbutton, value)

    implicit none
    type (gui_spin_button)   :: spinbutton
    integer                  :: value
    real(kind=C_DOUBLE)      :: c_value
    
    c_value = real(value, kind=C_DOUBLE)
    call setspinbuttonvalue(spinbutton%ptr, c_value)
    spinbutton%changed = 0
    spinbutton%last_value = value

    return
  end subroutine spin_button_set_integer_value

!
! Create a separator
!
  subroutine gui_create_separator(separator, box, orientation)

    implicit none
    type (gui_separator)   :: separator
    type (gui_box)         :: box
    type (gui_orientation) :: orientation
    integer(kind=C_INT)    :: c_vertical
    
    if(orientation%id.eq.gui_horizontal%id)then
       c_vertical = 0
    else if(orientation%id.eq.gui_vertical%id)then
       c_vertical = 1
    else
       write(0,*)'gui_create_separator(): unrecognised orientation!'
    endif
    
    call createseparator(separator%ptr, box%ptr, c_vertical)

    return
  end subroutine gui_create_separator

!
! Create a drawing area
!
  subroutine gui_create_drawing_area(drawingarea,box,double_buffered, &
       width, height)

    implicit none
    type (gui_drawing_area) :: drawingarea
    type (gui_box)          :: box
    logical, optional       :: double_buffered
    integer(kind=C_INT)     :: db
    integer(kind=C_INT)     :: c_width, c_height
    integer, optional       :: width, height

    c_width  = -1
    c_height = -1
    if(present(width))then
       c_width = width
    endif
    if(present(height))then
       c_height = height
    endif
    if(present(width).neqv.present(height)) &
         call terminate('Must specify width AND height in create_drawing_area')

    db = 0
    if(present(double_buffered))then
       if(double_buffered)then
          db = 1
       else
          db = 0
       endif
    endif

    call createdrawingarea(drawingarea%ptr,box%ptr,drawingarea%pixmap, &
         drawingarea%mouse_state,drawingarea%width,drawingarea%height, &
         drawingarea%resized,db,c_width,c_height)

    drawingarea%mouse_state   = 0
    drawingarea%resized       = 0
    drawingarea%width         = -1
    drawingarea%height        = -1
    drawingarea%gc_created    = .false.

    return
  end subroutine gui_create_drawing_area

!
! Check if the mouse has moved in the drawing area
!
  logical function gui_drawing_area_mouse_moved(drawingarea)

    implicit none
    type (gui_drawing_area) :: drawingarea

    if(drawingarea%mouse_state(1).gt.0)then
       gui_drawing_area_mouse_moved = .true.
    else
       gui_drawing_area_mouse_moved = .false.
    endif
    drawingarea%mouse_state(1) = 0

    return
  end function gui_drawing_area_mouse_moved

!
! Get the mouse position in the drawing area
!
  subroutine gui_drawing_area_get_mouse_pos(drawingarea,x,y)

    implicit none
    type (gui_drawing_area) :: drawingarea
    integer :: x, y

    x = drawingarea%mouse_state(2)
    y = drawingarea%mouse_state(3)

    return
  end subroutine gui_drawing_area_get_mouse_pos

!
! Check if a mouse button has been clicked
!
  logical function gui_drawing_area_mouse_clicked(drawingarea,button)

    implicit none
    type (gui_drawing_area) :: drawingarea
    integer :: button

    if(button.lt.1.or.button.gt.5)then
       call terminate("gui_drawing_area_mouse_clicked(): "//&
            "button index must be in range 1-5")
    endif

    if(drawingarea%mouse_state(3+button).gt.0)then
       gui_drawing_area_mouse_clicked=.true.
    else
       gui_drawing_area_mouse_clicked=.false.
    endif

    drawingarea%mouse_state(3+button) = 0

    return
  end function gui_drawing_area_mouse_clicked
!
! Check if a mouse button is currently held down
!
  logical function gui_drawing_area_mouse_down(drawingarea,button)

    implicit none
    type (gui_drawing_area) :: drawingarea
    integer :: button

    if(button.lt.1.or.button.gt.5)then
       call terminate("gui_drawing_area_mouse_down(): "//&
            "button index must be in range 1-5")
    endif

    if(drawingarea%mouse_state(8+button).gt.0)then
       gui_drawing_area_mouse_down=.true.
    else
       gui_drawing_area_mouse_down=.false.
    endif

    return
  end function gui_drawing_area_mouse_down

!
! Return mouse position and button state
!
  subroutine gui_drawing_area_mouse_state(drawingarea, down, x, y)

    implicit none
    type (gui_drawing_area) :: drawingarea
    logical, dimension(5)   :: down
    integer                 :: x, y
    
    ! Get position
    x = drawingarea%mouse_state(2)
    y = drawingarea%mouse_state(3)

    ! Get button states
    down(1:5) = (drawingarea%mouse_state(9:13).gt.0)

    return
  end subroutine gui_drawing_area_mouse_state

!
! Check if the drawing area has been resized
!
  logical function gui_drawing_area_resized(drawingarea)

    implicit none
    type(gui_drawing_area) :: drawingarea

    if(drawingarea%resized.ne.0)then
       gui_drawing_area_resized = .true.
    else
       gui_drawing_area_resized = .false.
    end if

    drawingarea%resized = 0

    return
  end function gui_drawing_area_resized

!
! Get the size of the drawing area
! 
  subroutine gui_drawing_area_get_size(drawingarea,width,height)

    implicit none
    type(gui_drawing_area) :: drawingarea
    integer :: width, height

    width  = drawingarea%width
    height = drawingarea%height

    return
  end subroutine gui_drawing_area_get_size

!
! Redraw the drawing area from its backing pixmap
! 
  subroutine gui_drawing_area_redraw(drawingarea)

    implicit none
    type(gui_drawing_area) :: drawingarea

    call redrawdrawingarea(drawingarea%ptr,drawingarea%width, &
         drawingarea%height)

    return
  end subroutine gui_drawing_area_redraw

!
! Draw an image of size width*height on the drawing area
! at position (x,y). The image is stored as an array of characters
! with 3 characters per pixel representing the red, green and blue
! components - e.g. to set one colour channel for a pixel at (x,y)
! the array index to set would be
!
! array index = colour_channel + 3*x + 3*width*y
!
! where colour_channel, x, and y are indexed from 0 (not 1).
!
  subroutine gui_draw_image(drawingarea,image,width,height,x,y)

    implicit none
    type (gui_drawing_area) :: drawingarea
    integer :: width, height
    integer :: x, y
    character, dimension(:) :: image
    integer(kind=C_INT) :: c_width, c_height, c_x, c_y

    c_width  = width
    c_height = height
    c_x      = x
    c_y      = y

    call drawimage(drawingarea%ptr, drawingarea%pixmap, &
         image,c_width,c_height,c_x,c_y)

    return
  end subroutine gui_draw_image

!
! Create a widget that displays an image from a file
!
  subroutine gui_create_image(image, box, filename)

    implicit none
    type (gui_image) :: image
    type (gui_box)   :: box
    character(len=*) :: filename

    call createimage(image%ptr,box%ptr,trim(filename)//char(0))
    
    return
  end subroutine gui_create_image

!
! Grey or un-grey out a widget
!
  subroutine set_sensitive_window(widget, state)

    implicit none
    type (gui_window) :: widget
    logical :: state
    integer(kind=C_INT) :: flag

    if(state)then
       flag = 1
    else
       flag = 0   
    endif
    call setsensitive(widget%ptr,flag)

    return
  end subroutine set_sensitive_window

  subroutine set_sensitive_box(widget, state)

    implicit none
    type (gui_box) :: widget
    logical :: state
    integer(kind=C_INT) :: flag

    if(state)then
       flag = 1
    else
       flag = 0   
    endif
    call setsensitive(widget%ptr,flag)

    return
  end subroutine set_sensitive_box

  subroutine set_sensitive_button(widget, state)

    implicit none
    type (gui_button) :: widget
    logical :: state
    integer(kind=C_INT) :: flag

    if(state)then
       flag = 1
    else
       flag = 0   
    endif
    call setsensitive(widget%ptr,flag)

    return
  end subroutine set_sensitive_button

  subroutine set_sensitive_entrybox(widget, state)

    implicit none
    type (gui_entrybox) :: widget
    logical :: state
    integer(kind=C_INT) :: flag

    if(state)then
       flag = 1
    else
       flag = 0   
    endif
    call setsensitive(widget%ptr,flag)

    return
  end subroutine set_sensitive_entrybox

  subroutine set_sensitive_checkbox(widget, state)

    implicit none
    type (gui_checkbox) :: widget
    logical :: state
    integer(kind=C_INT) :: flag

    if(state)then
       flag = 1
    else
       flag = 0   
    endif
    call setsensitive(widget%ptr,flag)

    return
  end subroutine set_sensitive_checkbox

  subroutine set_sensitive_slider(widget, state)

    implicit none
    type (gui_slider) :: widget
    logical :: state
    integer(kind=C_INT) :: flag

    if(state)then
       flag = 1
    else
       flag = 0   
    endif
    call setsensitive(widget%ptr,flag)

    return
  end subroutine set_sensitive_slider

  subroutine set_sensitive_menu(widget, state)

    implicit none
    type (gui_menu) :: widget
    logical :: state
    integer(kind=C_INT) :: flag

    if(state)then
       flag = 1
    else
       flag = 0   
    endif
    call setsensitive(widget%ptr,flag)

    return
  end subroutine set_sensitive_menu

  subroutine set_sensitive_menu_item(widget, state)

    implicit none
    type (gui_menu_item) :: widget
    logical :: state
    integer(kind=C_INT) :: flag

    if(state)then
       flag = 1
    else
       flag = 0   
    endif
    call setsensitive(widget%ptr,flag)

    return
  end subroutine set_sensitive_menu_item

  subroutine set_sensitive_radio_button(widget, state)

    implicit none
    type (gui_radio_button) :: widget
    logical :: state
    integer(kind=C_INT) :: flag

    if(state)then
       flag = 1
    else
       flag = 0   
    endif
    call setsensitive(widget%ptr,flag)

    return
  end subroutine set_sensitive_radio_button

  subroutine set_sensitive_spin_button(widget, state)

    implicit none
    type (gui_spin_button) :: widget
    logical :: state
    integer(kind=C_INT) :: flag

    if(state)then
       flag = 1
    else
       flag = 0   
    endif
    call setsensitive(widget%ptr,flag)

    return
  end subroutine set_sensitive_spin_button

  subroutine set_sensitive_combo_box(widget, state)

    implicit none
    type (gui_combo_box) :: widget
    logical :: state
    integer(kind=C_INT) :: flag

    if(state)then
       flag = 1
    else
       flag = 0   
    endif
    call setsensitive(widget%ptr,flag)

    return
  end subroutine set_sensitive_combo_box


  subroutine set_sensitive_colour_button(widget, state)

    implicit none
    type (gui_colour_button) :: widget
    logical :: state
    integer(kind=C_INT) :: flag

    if(state)then
       flag = 1
    else
       flag = 0   
    endif
    call setsensitive(widget%ptr,flag)

    return
  end subroutine set_sensitive_colour_button


  subroutine gui_create_combo_box(combobox, box, text)

    type (gui_combo_box)           :: combobox
    type (gui_box)                 :: box
    character(len=*), dimension(:) :: text
    integer :: i

    call createcombobox(combobox%ptr, box%ptr, combobox%changed)
    do i = 1, size(text), 1
       call comboboxaddtext(combobox%ptr, trim(text(i))//achar(0))
    end do

    combobox%changed = 0
    combobox%nlines  = size(text)

    return
  end subroutine gui_create_combo_box


  logical function gui_combo_box_changed(combobox)

    implicit none
    type (gui_combo_box) :: combobox

    if(combobox%changed.ne.0)then
       gui_combo_box_changed = .true.
    else
       gui_combo_box_changed = .false.
    endif

    combobox%changed = 0

    return
  end function gui_combo_box_changed

  subroutine gui_combo_box_get_index(combobox, i)

    implicit none
    type (gui_combo_box) :: combobox
    integer :: i
    
    call comboboxgetindex(combobox%ptr,i)
    i = i + 1 ! Number from 1 rather than zero

    return
  end subroutine gui_combo_box_get_index

  subroutine gui_combo_box_set_index(combobox, i)

    implicit none
    type (gui_combo_box) :: combobox
    integer :: i
    
    call comboboxsetindex(combobox%ptr,(i-1))

    combobox%changed = 0

    return
  end subroutine gui_combo_box_set_index


  subroutine gui_combo_box_set_text(combobox, text)

    implicit none
    type (gui_combo_box) :: combobox
    character(len=*), dimension(:) :: text
    integer :: i
    integer(kind=C_INT) :: iline

    do i = combobox%nlines-1, 0, -1
       iline = i
       call removeline(combobox%ptr,iline)
    end do

    combobox%nlines = size(text)
    do i = 1, size(text), 1
       call comboboxaddtext(combobox%ptr, trim(text(i))//achar(0))
    end do

    combobox%changed = 0

    return
  end subroutine gui_combo_box_set_text


  subroutine gui_create_colour_button(button, box)

    type (gui_colour_button)       :: button
    type (gui_box)                 :: box

    call createcolourbutton(button%ptr, box%ptr, button%changed)

    button%changed = 0

    return
  end subroutine gui_create_colour_button

  logical function gui_colour_button_changed(colour_button)

    implicit none
    type (gui_colour_button) :: colour_button

    if(colour_button%changed.ne.0)then
       gui_colour_button_changed = .true.
    else
       gui_colour_button_changed = .false.
    endif

    colour_button%changed = 0

    return
  end function gui_colour_button_changed


  subroutine gui_colour_button_get_colour(colour_button, r, g, b)

    implicit none
    type (gui_colour_button) :: colour_button
    integer :: r, g, b
    integer(kind=C_INT) :: r_c, g_c, b_c
    
    call colourbuttonget(colour_button%ptr,r_c,g_c,b_c)
    r=r_c
    g=g_c
    b=b_c
    
    return
  end subroutine gui_colour_button_get_colour


  subroutine gui_colour_button_set_colour(colour_button, r, g, b)

    implicit none
    type (gui_colour_button) :: colour_button
    integer :: r, g, b
    integer(kind=C_INT) :: r_c, g_c, b_c
    
    r_c=r
    g_c=g
    b_c=b
    call colourbuttonset(colour_button%ptr,r_c,g_c,b_c)
    
    colour_button%changed = 0

    return
  end subroutine gui_colour_button_set_colour


  subroutine gui_create_progress_bar(progress_bar, box)

    implicit none

    type (gui_progress_bar) :: progress_bar
    type (gui_box)          :: box

    call createpbar(progress_bar%ptr, box%ptr)

    return
  end subroutine gui_create_progress_bar


  subroutine gui_progress_bar_set(progress_bar, f)

    implicit none
    type (gui_progress_bar) :: progress_bar
    real, intent(in)        :: f
    real(kind=C_DOUBLE)     :: c_f
    
    c_f = max(min(f,1.0),0.0)
    call setprogress(progress_bar%ptr,c_f)

    return
  end subroutine gui_progress_bar_set


  subroutine gui_set_event_handler(event_handler)
!
! Set a function to be called whenever an event occurs
!
    implicit none
    interface
       subroutine event_handler()
       end subroutine event_handler
    end interface

    call seteventhandler(event_handler)

    return
  end subroutine gui_set_event_handler


  subroutine gui_unset_event_handler()
!
! Set null event handler
!
    implicit none

    call noeventhandler()

    return
  end subroutine gui_unset_event_handler


  subroutine gui_main_loop()
!
! Go into the GTK main loop
!
    implicit none

    call gtkmainloop()

    return
  end subroutine gui_main_loop


  subroutine gui_quit()
!
! Exit the GTK main loop
!
    implicit none

    call gtkquit()

    return
  end subroutine gui_quit

!
! Routines to make widgets visible/invisible
!
  subroutine set_visible_window(widget, state)

    implicit none
    type (gui_window) :: widget
    logical :: state
    integer(kind=C_INT) :: flag

    if(state)then
       flag = 1
    else
       flag = 0   
    endif
    call setvisible(widget%ptr,flag)

    return
  end subroutine set_visible_window


  subroutine set_visible_box(widget, state)

    implicit none
    type (gui_box) :: widget
    logical :: state
    integer(kind=C_INT) :: flag

    if(state)then
       flag = 1
    else
       flag = 0   
    endif
    call setvisible(widget%ptr,flag)

    return
  end subroutine set_visible_box



  subroutine set_visible_button(widget, state)

    implicit none
    type (gui_button) :: widget
    logical :: state
    integer(kind=C_INT) :: flag

    if(state)then
       flag = 1
    else
       flag = 0   
    endif
    call setvisible(widget%ptr,flag)

    return
  end subroutine set_visible_button


  subroutine set_visible_entrybox(widget, state)

    implicit none
    type (gui_entrybox) :: widget
    logical :: state
    integer(kind=C_INT) :: flag

    if(state)then
       flag = 1
    else
       flag = 0   
    endif
    call setvisible(widget%ptr,flag)

    return
  end subroutine set_visible_entrybox


  subroutine set_visible_checkbox(widget, state)

    implicit none
    type (gui_checkbox) :: widget
    logical :: state
    integer(kind=C_INT) :: flag

    if(state)then
       flag = 1
    else
       flag = 0   
    endif
    call setvisible(widget%ptr,flag)

    return
  end subroutine set_visible_checkbox


  subroutine set_visible_slider(widget, state)

    implicit none
    type (gui_slider) :: widget
    logical :: state
    integer(kind=C_INT) :: flag

    if(state)then
       flag = 1
    else
       flag = 0   
    endif
    call setvisible(widget%ptr,flag)

    return
  end subroutine set_visible_slider


  subroutine set_visible_radio_button(widget, state)

    implicit none
    type (gui_radio_button) :: widget
    logical :: state
    integer(kind=C_INT) :: flag

    if(state)then
       flag = 1
    else
       flag = 0   
    endif
    call setvisible(widget%ptr,flag)

    return
  end subroutine set_visible_radio_button


  subroutine set_visible_spin_button(widget, state)

    implicit none
    type (gui_spin_button) :: widget
    logical :: state
    integer(kind=C_INT) :: flag

    if(state)then
       flag = 1
    else
       flag = 0   
    endif
    call setvisible(widget%ptr,flag)

    return
  end subroutine set_visible_spin_button


  subroutine set_visible_combo_box(widget, state)

    implicit none
    type (gui_combo_box) :: widget
    logical :: state
    integer(kind=C_INT) :: flag

    if(state)then
       flag = 1
    else
       flag = 0   
    endif
    call setvisible(widget%ptr,flag)

    return
  end subroutine set_visible_combo_box


  subroutine set_visible_colour_button(widget, state)

    implicit none
    type (gui_colour_button) :: widget
    logical :: state
    integer(kind=C_INT) :: flag

    if(state)then
       flag = 1
    else
       flag = 0   
    endif
    call setvisible(widget%ptr,flag)

    return
  end subroutine set_visible_colour_button



  subroutine set_visible_menu(widget, state)

    implicit none
    type (gui_menu) :: widget
    logical :: state
    integer(kind=C_INT) :: flag

    if(state)then
       flag = 1
    else
       flag = 0   
    endif
    call setvisible(widget%ptr,flag)

    return
  end subroutine set_visible_menu

  subroutine set_visible_menu_item(widget, state)

    implicit none
    type (gui_menu_item) :: widget
    logical :: state
    integer(kind=C_INT) :: flag

    if(state)then
       flag = 1
    else
       flag = 0   
    endif
    call setvisible(widget%ptr,flag)

    return
  end subroutine set_visible_menu_item


  subroutine gui_fullscreen(window, full_screen)
!
! Set a window to full screen mode
!
    implicit none
    type (gui_window)   :: window
    logical             :: full_screen
    integer(kind=C_INT) :: flag
    
    flag = 0
    if(full_screen)flag=1

    call fullscreen(window%ptr, flag)

    return
  end subroutine gui_fullscreen


  subroutine gui_menu_item_add_accelerator(menuitem, window, key, modifier)
!
! Add an accelerator key to a menu item
!
    implicit none
    type(gui_menu_item) :: menuitem
    type(gui_window)    :: window
    character(len=*)    :: key
    character(len=*), optional :: modifier
    integer(kind=C_INT)        :: make_group
    character(len=20)          :: c_modifier

    if(window%has_accel_group)then
       make_group = 0
    else
       make_group = 1
    endif

    if(present(modifier))then
       c_modifier = trim(adjustl(modifier))
    else
       c_modifier = ""
    endif

    call addaccel(window%ptr, window%accel_group, menuitem%ptr, &
         trim(adjustl(key))//achar(0), trim(c_modifier)//achar(0), &
         make_group)

    return
  end subroutine gui_menu_item_add_accelerator


  subroutine gui_create_notebook(notebook, box)
!
! Create a notebook widget
!
    implicit none
    type(gui_notebook) :: notebook
    type(gui_box)      :: box

    call createnotebook(notebook%ptr, box%ptr)
    
    return
  end subroutine gui_create_notebook


  subroutine gui_create_box_in_notebook(box, notebook, label)
!
! Make a new vbox and pack it into the notebook as a new page
!
    implicit none
    type(gui_box) :: box
    type(gui_notebook) :: notebook
    character(len=*) :: label

    box%is_vbox = 1
    call createnotebookpage(notebook%ptr, box%ptr, trim(label)//achar(0))

    return
  end subroutine gui_create_box_in_notebook


  subroutine gui_notebook_set_page(notebook, page)
!
! Set the page to be displayed by a notebook
!
    implicit none
    type(gui_notebook)  :: notebook
    integer             :: page
    integer(kind=C_INT) :: c_page

    c_page = page
    call setnotebookpage(notebook%ptr, c_page)

    return
  end subroutine gui_notebook_set_page


  subroutine gui_create_textview(textview, box)
!
! Create a widget to display text
!
    implicit none
    type (gui_textview) :: textview
    type (gui_box)      :: box

    call createtextview(textview%ptr, box%ptr, textview%textbuffer)

    return
  end subroutine gui_create_textview


  subroutine gui_textview_add_line(textview, str)
!
! Add some text to a textview
!
    implicit none
    type (gui_textview) :: textview
    character(len=*) :: str

    call addline(textview%textbuffer,trim(str)//achar(0))

    return
  end subroutine gui_textview_add_line


  subroutine gui_textview_clear(textview)
!
! Delete all text from a textview
!
    implicit none
    type (gui_textview) :: textview

    call cleartext(textview%textbuffer)

    return
  end subroutine gui_textview_clear


end module f90_gui
