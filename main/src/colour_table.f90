module colour_table
!
! Module to generate and store colour tables
!
  use f90_gui
  use stereo

  implicit none
  private
  save

  public :: colour_table_init
  public :: colour_table_list
  public :: colour_table_editor_open
  public :: colour_table_editor_close
  public :: colour_table_process_events
  public :: colour_table_read_conf

  integer :: ncoltab
  integer, parameter, public :: ncoltabmax         = 100
  integer, parameter, public :: coltab_name_length = 40

  ! Colour table editor window
  type (gui_window) :: coltab_window

  ! Widgets for editor window
  type (gui_combo_box)     :: coltab_box
  type (gui_drawing_area)  :: ctab_area
  integer, parameter       :: nbuttonmax = 6
  integer                  :: nbutton    = nbuttonmax
  type (gui_colour_button), dimension(nbuttonmax) :: ctbutton
  type (gui_box)           :: button_box
  type (gui_combo_box)     :: nbutton_box
  type (gui_entrybox)      :: namebox
  type (gui_button)        :: store_button
  type (gui_radio_button)  :: noint_button, linear_button, cubic_button

  logical :: window_open = .false.

  ! Image buffer
  integer :: width, height
  character, dimension(:), allocatable :: image

  ! Type to describe a colour table
  type coltab_type
     character(len=coltab_name_length) :: name
     integer,   dimension(1:3,0:255)   :: data
     character, dimension(1:3,0:255)   :: cdata
     integer :: npoints
     integer, dimension(nbuttonmax) :: idx,r,g,b
     integer :: interpolation
  end type coltab_type
  public :: coltab_type
  type (coltab_type), dimension(ncoltabmax), public :: coltab
  
  ! Filename for saved colour tables
  character(len=500) :: coltab_fname

  ! Constants for interpolation values
  integer, parameter :: INTERP_NONE   = 0
  integer, parameter :: INTERP_LINEAR = 1
  integer, parameter :: INTERP_CUBIC  = 2

contains

  subroutine colour_table_init()
!
! Initialise the module
!
    implicit none

    ncoltab = 0

    return
  end subroutine colour_table_init

  
  subroutine colour_table_defaults()
!
! Create some default colour tables
!
    implicit none

    call colour_table_add( &
         "Constant Grey", & ! Name
         (/ 000 /),       & ! Index
         (/ 128 /),       & ! R
         (/ 128 /),       & ! G
         (/ 128 /),       & ! B
         INTERP_NONE)

    call colour_table_add( &
         "Greyscale",    & ! Name
         (/ 000, 255 /), & ! Index
         (/ 000, 255 /), & ! R
         (/ 000, 255 /), & ! G
         (/ 000, 255 /), & ! B
         INTERP_LINEAR)

    call colour_table_add( &
         "Red to White",   & ! Name
         (/ 000, 080, 160, 255/), & ! Index
         (/ 000, 127, 255, 255/), & ! R
         (/ 000, 000, 127, 255/), & ! G
         (/ 000, 000, 000, 255/), & ! B
         INTERP_CUBIC)

    call colour_table_add( &
         "Green to White", & ! Name
         (/ 000, 080, 160, 255/), & ! Index
         (/ 000, 000, 127, 255/), & ! R
         (/ 000, 127, 255, 255/), & ! G
         (/ 000, 000, 000, 255/), & ! B
         INTERP_CUBIC)

    call colour_table_add( &
         "Blue to White",  & ! Name
         (/ 000, 080, 160, 255/), & ! Index
         (/ 000, 000, 000, 255/), & ! R
         (/ 000, 000, 127, 255/), & ! G
         (/ 000, 127, 255, 255/), & ! B
         INTERP_CUBIC)

    call colour_table_add( &
         "Rainbow",                         & ! Name
         (/ 000, 051, 102, 153, 204, 255/), & ! Index
         (/ 000, 000, 000, 000, 255, 255/), & ! R
         (/ 000, 000, 255, 255, 255, 000/), & ! G
         (/ 000, 255, 255, 000, 000, 000/), & ! B
         INTERP_CUBIC)

   call colour_table_add( &
         "Rainbow (no black)",              & ! Name
         (/ 000, 064, 128, 192, 255/), & ! Index
         (/ 000, 000, 000, 255, 255/), & ! R
         (/ 000, 255, 255, 255, 000/), & ! G
         (/ 255, 255, 000, 000, 000/), & ! B
         INTERP_CUBIC)

    return
  end subroutine colour_table_defaults


  subroutine colour_table_add(name,idx,r,g,b,interp,overwrite)
!
! Add a new colour table by specifying the colour at a
! set of points
!
    implicit none
    character(len=*) :: name
    integer, dimension(:) :: idx, r, g, b
    integer :: i, j, itab
    integer, optional :: overwrite
    integer :: interp

    ! Check if we're overwriting a colour table
    if(present(overwrite))then
       itab = overwrite
    else
       itab = ncoltab + 1
    endif

    ! Make a new colour table using the input data
    if(itab.gt.ncoltabmax)stop'Too many colour tables! Increase ncoltabmax'

    ! Record name of the new colour table
    coltab(itab)%name = trim(name)
    coltab(itab)%interpolation = interp

    ! Calculate r,g,b values
    call colour_table_generate(idx,r,g,b, &
         coltab(itab)%data(1,0:255), &
         coltab(itab)%data(2,0:255), &
         coltab(itab)%data(3,0:255), interp)

    ! Store character version of the data
    do i = 1, 3, 1
       do j = 0, 255, 1
          coltab(itab)%cdata(i,j) = char(coltab(itab)%data(i,j))
       end do
    end do

    ! Store points used to generate the colour table
    coltab(itab)%npoints          = size(idx)
    coltab(itab)%r(1:size(idx))   = r(1:size(idx))
    coltab(itab)%g(1:size(idx))   = g(1:size(idx))
    coltab(itab)%b(1:size(idx))   = b(1:size(idx))
    coltab(itab)%idx(1:size(idx)) = idx(1:size(idx))

    ! Increase ncoltab if necessary
    ncoltab = max(ncoltab,itab)

    return
  end subroutine colour_table_add


  subroutine colour_table_generate(idx,r,g,b,r_out,g_out,b_out,interp)
!
! Generate r,g,b arrays for a colour table
!
    implicit none
    integer, dimension(:)     :: idx, r, g, b
    integer, dimension(:), allocatable :: re, ge, be, ie
    integer, dimension(0:255) :: r_out, g_out, b_out
    integer :: i, j
    real    :: f
    integer :: interp, do_interp
    integer :: i1, i2, i3, i4
    real :: r1, r2, r3, r4
    real :: g1, g2, g3, g4
    real :: b1, b2, b3, b4

    ! If there's only one colour, set values and return
    if(size(idx).eq.1)then
       r_out = r(1)
       g_out = g(1)
       b_out = b(1)
       return
    endif

    select case(interp)
    case(INTERP_NONE)
       ! Just use closest value
       do i = lbound(idx,1), ubound(idx,1), 1
          if(i.eq.lbound(idx,1))then
             i1 = 0
          else
             i1 = 0.5 * (idx(i)+idx(i-1))
          endif
          if(i.eq.ubound(idx,1))then
             i2 = 255
          else
             i2 = 0.5 * (idx(i+1)+idx(i))
          endif
          r_out(i1:i2) = r(i)
          g_out(i1:i2) = g(i)
          b_out(i1:i2) = b(i)
       end do
    case(INTERP_LINEAR)
       ! Linear interpolation
       do i = 1, ubound(idx,1)-lbound(idx,1), 1
          do j = idx(i), idx(i+1), 1
             if(j.lt.0.or.j.gt.255)stop'colour_table_add() - idx out of range'
             f = real(j-idx(i))/(idx(i+1)-idx(i))
             r_out(j) = floor(r(i) + f*(r(i+1)-r(i)))
             g_out(j) = floor(g(i) + f*(g(i+1)-g(i)))
             b_out(j) = floor(b(i) + f*(b(i+1)-b(i)))
          end do
       end do
    case(INTERP_CUBIC)
       if(size(idx).gt.1)then
          ! Cubic interpolation
          ! Here we linearly extrapolate an extra point at each end.
          i1 = lbound(idx, 1)
          i2 = ubound(idx, 1)
          allocate(re(i1-1:i2+1))
          allocate(ge(i1-1:i2+1))
          allocate(be(i1-1:i2+1))
          allocate(ie(i1-1:i2+1))
          re(i1:i2) = r
          ge(i1:i2) = g
          be(i1:i2) = b
          ie(i1:i2) = idx
          ie(i1-1) = idx(i1) - (idx(i1+1)-idx(i1))
          re(i1-1) = r(i1)   - (r(i1+1) - r(i1))
          ge(i1-1) = g(i1)   - (g(i1+1) - g(i1))
          be(i1-1) = b(i1)   - (b(i1+1) - b(i1))
          ie(i2+1) = idx(i2) + (idx(i2)-idx(i2-1))
          re(i2+1) = r(i2) + (r(i2)-r(i2-1))
          ge(i2+1) = g(i2) + (g(i2)-g(i2-1))
          be(i2+1) = b(i2) + (b(i2)-b(i2-1))
          do i = lbound(idx,1), ubound(idx,1)-1, 1
             do j = idx(i), idx(i+1), 1
                f = real(j-idx(i))/real(idx(i+1)-idx(i))             
                r_out(j) = cubic_interpolate(real(re(i-1)), real(re(i)), real(re(i+1)), real(re(i+1)), f)
                g_out(j) = cubic_interpolate(real(ge(i-1)), real(ge(i)), real(ge(i+1)), real(ge(i+1)), f)
                b_out(j) = cubic_interpolate(real(be(i-1)), real(be(i)), real(be(i+1)), real(be(i+1)), f)
             end do
          end do
          deallocate(re,ge,be,ie)
       else
          ! Need at least two points for cubic interpolation 
          r_out = r(1)
          g_out = g(1)
          b_out = b(1)
       endif
    end select

    ! Make sure all values are in range
    do j = 0, 255, 1
       if(r_out(j).lt.0)r_out(j) = 0
       if(g_out(j).lt.0)g_out(j) = 0
       if(b_out(j).lt.0)b_out(j) = 0
       if(r_out(j).gt.255)r_out(j)=255
       if(g_out(j).gt.255)g_out(j)=255
       if(b_out(j).gt.255)b_out(j)=255
    end do

    return

  contains

    real function cubic_interpolate(y0,y1,y2,y3,mu)

      implicit none
      real :: y0,y1,y2,y3,mu
      real :: a0,a1,a2,a3,mu2

      mu2 = mu*mu;
      a0 = y3 - y2 - y0 + y1
      a1 = y0 - y1 - a0
      a2 = y2 - y0
      a3 = y1

      cubic_interpolate = a0*mu*mu2+a1*mu2+a2*mu+a3
  
      return
    end function cubic_interpolate

  end subroutine colour_table_generate


  subroutine colour_table_list(n,name)
!
! Return a list of the colour table names
!
    implicit none
    integer, intent(out) :: n
    character(len=*), dimension(:), intent(out) :: name
    integer :: i
    
    do i = 1, ncoltab, 1
       name(i) = trim(coltab(i)%name)
    end do
    n = ncoltab

    return
  end subroutine colour_table_list


  subroutine colour_table_editor_open(mainwin)
!
! Open a window for creating/editing colour tables
!
    implicit none
    type (gui_window), target :: mainwin
    type (gui_box)    :: hbox, vbox, vbox2
    type (gui_label)  :: label
    integer           :: ncoltab
    character(len=coltab_name_length), dimension(ncoltabmax) :: ctname
    integer           :: ibutton

    if(window_open)return
    window_open = .true.

    ! Create the window
    call gui_create_window(coltab_window, parent=mainwin, &
         title="Colour tables",resize=.false.)

    ! List of current colour tables
    call gui_packing_mode(expand=.true., fill=.true., spacing=3, &
         position=gui_start)
    call gui_create_box(vbox, coltab_window, gui_vertical)
    call gui_packing_mode(expand=.true., fill=.false., spacing=3, &
         position=gui_start)
    call gui_create_box(hbox, vbox, gui_horizontal)
    call gui_create_label(label, hbox, "Existing colour tables ")
    call colour_table_list(ncoltab, ctname)
    call gui_create_combo_box(coltab_box, hbox, ctname(1:ncoltab))

    ! Put a frame around the colour table settings
    call gui_packing_mode(expand=.true., fill=.true., spacing=3, &
         position=gui_start)
    call gui_create_box(vbox2, vbox, gui_vertical, frame=.true.)
        
    ! Area to display/enter name of the colour table
    call gui_create_box(hbox, vbox2, gui_horizontal)
    call gui_create_label(label, hbox, "Name: ")
    call gui_create_entrybox(namebox, hbox, coltab_name_length)

    ! Drawing area to display a colour table
    call gui_create_drawing_area(ctab_area, vbox2, width=200, height=50)

    ! Make a row of colour buttons
    call gui_create_box(button_box, vbox2, gui_horizontal)
    call gui_packing_mode(expand=.true., fill=.false., spacing=3, &
         position=gui_start)
    do ibutton = 1, nbuttonmax, 1
       call gui_create_colour_button(ctbutton(ibutton), button_box)
    end do
    call gui_packing_mode(expand=.false., fill=.false., spacing=3, &
         position=gui_start)

    ! Box with controls to change the number of buttons
    call gui_create_box(hbox, vbox2, gui_horizontal)
    call gui_create_label(label, hbox, "Number of colours to specify ")
    call gui_packing_mode(position=gui_end)
    call gui_create_combo_box(nbutton_box, hbox, &
         (/" 1 "," 2 "," 3 "," 4 "," 5 "," 6 "/))
    call gui_combo_box_set_index(nbutton_box, 5)
    call gui_packing_mode(position=gui_start)

    ! Interpolation options
    call gui_create_box(hbox, vbox2, gui_horizontal)
    call gui_create_label(label, hbox, "Interpolation method: ")
    call gui_packing_mode(position=gui_end)
    call gui_create_radio_button(cubic_button,  hbox, "Cubic")
    call gui_create_radio_button(linear_button, hbox, "Linear", previous=cubic_button)
    call gui_create_radio_button(noint_button,  hbox, "None", previous=linear_button)
    call gui_packing_mode(position=gui_start)

    ! Buttons to save changes / delete a colour table
    call gui_packing_mode(expand=.true., fill=.false., spacing=3, &
         position=gui_start)
    call gui_create_box(hbox, vbox, gui_horizontal)
    call gui_create_button(store_button,   hbox, " Store this colour table")

    ! Display the window
    call gui_show_window(coltab_window)

    if(stereo_enabled.and.is_fullscreen)then
       call gui_window_move(coltab_window,0.1,0.1)
    endif

    call colour_table_display()

    return
  end subroutine colour_table_editor_open


  logical function colour_table_process_events()
!
! Process events from the colour table editor window
!
    implicit none
    integer :: i, j
    character(len=coltab_name_length) :: name
    character(len=20) :: res
    integer, dimension(nbuttonmax) :: r_idx, g_idx, b_idx, idx
    integer :: istat, interp
    logical :: state

    ! Return true if a redraw may be needed (e.g. a colour table is modified)
    colour_table_process_events = .false.

    ! Maintain an image buffer the same size as the drawing area
    if(gui_drawing_area_resized(ctab_area))then
       call gui_drawing_area_get_size(ctab_area,width,height)
       if(allocated(image))deallocate(image)
       allocate(image(0:3*width*height-1), stat=istat)
       image = char(0)
       call colour_table_redraw()
    else
       if(.not.allocated(image))then
          allocate(image(0:3*width*height-1), stat=istat)
       endif
    endif

    ! Close the window if the close button is clicked
    if(gui_window_closed(coltab_window))then
       call gui_destroy_window(coltab_window)
       window_open = .false.
       return
    endif

    ! Change the number of colour buttons if necessary
    if(gui_combo_box_changed(nbutton_box))then
       call gui_combo_box_get_index(nbutton_box, nbutton)
       do i = 1, nbuttonmax, 1
          call gui_set_visible(ctbutton(i), (i.le.nbutton))
       end do
       call colour_table_redraw()
    endif

    ! Check if any of the colour buttons were changed
    do i = 1, nbutton, 1
       if(gui_colour_button_changed(ctbutton(i)))then
          call colour_table_redraw()       
       endif
    end do

    if(gui_radio_button_changed(noint_button).or. &
         gui_radio_button_changed(linear_button))then
       call colour_table_redraw()       
    endif

    ! If a colour table is selected, display it
    if(gui_combo_box_changed(coltab_box))then
       call colour_table_display()
    endif

    ! Save button
    if(gui_button_clicked(store_button))then
       ! Check if a colour table with this name exists
       call gui_entrybox_get_text(namebox, name)
       name = adjustl(name)
       j = ncoltab + 1
       do i = 1, ncoltab, 1
          if(trim(name).eq.trim(coltab(i)%name))j = i
       end do
       ! Warn if we're going to overwrite
       res = "yes"
       if(j.le.ncoltab)then
          res = gui_display_dialog(coltab_window,"question", &
               'Overwrite existing table "'//trim(name)//'"?')
       else
          if(j.gt.ncoltabmax)then
             res = gui_display_dialog(coltab_window,"error",&
                  "Maximum number of colour tables reached")
             return
          endif
       endif
       ! Store the colour table
       if(res.eq."yes")then
          ! Get colours to use
          do i = 1, nbutton, 1
             call gui_colour_button_get_colour(ctbutton(i), &
                  r_idx(i), g_idx(i), b_idx(i))
          end do
          ! Set index values
          idx(1) = 0
          idx(nbutton) = 255
          do i = 2, nbutton-1, 1
             idx(i) = (256/(nbutton-1))*(i-1)
          end do
          ! Get interpolation mode
          interp = INTERP_NONE
          call gui_radio_button_get_state(linear_button, state)
          if(state)interp = INTERP_LINEAR
          call gui_radio_button_get_state(cubic_button, state)
          if(state)interp = INTERP_CUBIC
          ! Add the colour table
          call colour_table_add(name,idx(1:nbutton),&
               r_idx(1:nbutton),g_idx(1:nbutton),b_idx(1:nbutton),interp,overwrite=j)
          ! Update the list
          call gui_combo_box_set_text(coltab_box, coltab(1:ncoltab)%name)
          call gui_combo_box_set_index(coltab_box,j)
          ! Save the colour tables
          call colour_table_write(coltab_fname)
          ! Now we may need a redraw
          colour_table_process_events = .true.

       endif
    endif

    return
  end function colour_table_process_events


  subroutine colour_table_display()
!
! Display the selected colour table
!
    implicit none
    integer :: i,j

    call gui_combo_box_get_index(coltab_box, i)
    nbutton = coltab(i)%npoints
    do j = 1, nbuttonmax, 1
       call gui_set_visible(ctbutton(j), (j.le.nbutton))
    end do
    do j = 1, nbutton, 1
       call gui_colour_button_set_colour(ctbutton(j), &
            coltab(i)%r(j), coltab(i)%g(j), coltab(i)%b(j))
    end do
    call gui_entrybox_set_text(namebox,trim(coltab(i)%name))
    call gui_combo_box_set_index(nbutton_box, nbutton)

    select case(coltab(i)%interpolation)
    case(INTERP_NONE)
       call gui_radio_button_set_state(noint_button, .true.)
    case(INTERP_LINEAR)
       call gui_radio_button_set_state(linear_button, .true.)
    case(INTERP_CUBIC)
       call gui_radio_button_set_state(cubic_button, .true.)
    end select

    call colour_table_redraw()
    
    return
  end subroutine colour_table_display


  subroutine colour_table_redraw()
!
! Draw the current colour table in the editor window
!
    implicit none
    integer, dimension(0:255)      :: r,g,b
    integer, dimension(nbuttonmax) :: r_idx, g_idx, b_idx, idx
    integer :: i, j, icol, interp
    logical :: state

    if(.not.allocated(image))return

    ! Get colours to use
    do i = 1, nbutton, 1
       call gui_colour_button_get_colour(ctbutton(i), &
            r_idx(i), g_idx(i), b_idx(i))
    end do
    
    ! Set index values
    idx(1) = 0
    idx(nbutton) = 255
    do i = 2, nbutton-1, 1
       idx(i) = (256/(nbutton-1))*(i-1)
    end do

    ! Get interpolation mode
    interp = INTERP_NONE
    call gui_radio_button_get_state(linear_button, state)
    if(state)interp = INTERP_LINEAR
    call gui_radio_button_get_state(cubic_button, state)
    if(state)interp = INTERP_CUBIC

    ! Calculate colours
    call colour_table_generate(idx(1:nbutton), &
         r_idx(1:nbutton), g_idx(1:nbutton), b_idx(1:nbutton), &
         r, g, b, interp)

    ! Draw the image in the image buffer
    do i = 0, width-1, 1
       icol = min(floor(real(i)/real(width)*256.0),255)
       do j = 0, height-1, 1
          image(3*i+3*j*width+0) = char(r(icol))
          image(3*i+3*j*width+1) = char(g(icol))
          image(3*i+3*j*width+2) = char(b(icol))
       end do
    end do

    ! Copy the image into the drawing area
    call gui_draw_image(ctab_area,image,width,height,0,0)
    call gui_drawing_area_redraw(ctab_area)

    return
  end subroutine colour_table_redraw


  subroutine colour_table_write(fname)
!
! Write the current set of colour tables to a file
! Just return if the open or writes fail.
!
    use key_file
    implicit none
    character(len=*) :: fname
    integer          :: i, j, ios

    call read_key_file(fname)
    do i = 1, ncoltab, 1
       call set_key(coltab(i)%name, "Colours", coltab(i)%npoints)
       call set_key(coltab(i)%name, "Index", coltab(i)%idx(1:coltab(i)%npoints))
       call set_key(coltab(i)%name, "Red", coltab(i)%r(1:coltab(i)%npoints))
       call set_key(coltab(i)%name, "Green", coltab(i)%g(1:coltab(i)%npoints))
       call set_key(coltab(i)%name, "Blue", coltab(i)%b(1:coltab(i)%npoints))
       call set_key(coltab(i)%name, "Interpolation", coltab(i)%interpolation)
    end do
    call write_key_file(fname)
    call close_key_file()

    return
  end subroutine colour_table_write


  subroutine colour_table_read(fname)
!
! Read colour tables from a file. Abort as soon as a read fails.
!
    use key_file
    implicit none
    character(len=*) :: fname
    integer          :: i, j, n
    character(len=coltab_name_length), dimension(ncoltabmax) :: name
    integer :: npoints
    integer, dimension(nbuttonmax) :: idx,r,g,b
    integer :: interp

    call colour_table_init()
    call read_key_file(fname)
    call get_group_names(n, name)
    do i = 1, n, 1
       r= -1
       g=-1
       b=-1
       idx=-1
       call get_key(name(i), "Colours", npoints)
       npoints = min(max(1, npoints), nbuttonmax)
       call get_key(name(i), "Index", idx(1:npoints))
       call get_key(name(i), "Red",   r(1:npoints))
       call get_key(name(i), "Green", g(1:npoints))
       call get_key(name(i), "Blue",  b(1:npoints))
       interp = 1
       call get_key(name(i), "Interpolation",  interp)
       if(  all(r(1:npoints).ge.0).and.all(r(1:npoints).le.255).and. &
            all(g(1:npoints).ge.0).and.all(g(1:npoints).le.255).and. &
            all(b(1:npoints).ge.0).and.all(b(1:npoints).le.255).and. &
            all(idx(1:npoints).ge.0).and.all(idx(1:npoints).le.255).and. &
            len(name(i)).gt.0)then
          call colour_table_add(trim(adjustl(name(i))),idx(1:npoints), &
               r(1:npoints),g(1:npoints),b(1:npoints), interp)
       endif
    end do
    call close_key_file()

    return
  end subroutine colour_table_read


  subroutine colour_table_read_conf(dirname)
!
! Read in any colour tables
!
    implicit none
    character(len=*)   :: dirname

    coltab_fname = trim(dirname)//"/"//"colour_tables"
    call colour_table_read(coltab_fname)

    ! Set up defaults if no colour tables were read
    if(ncoltab.eq.0)call colour_table_defaults()

    return
  end subroutine colour_table_read_conf


  subroutine colour_table_editor_close()
!
! Close the window
!
    implicit none

    if(window_open)then
       call gui_destroy_window(coltab_window)
       window_open = .false.
    endif

    return
  end subroutine colour_table_editor_close


end module colour_table
