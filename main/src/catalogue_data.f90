module catalogue_data
!
! This module stores a set of point catalogues which can be plotted 
! on the main display
!
  use f90_gui
  use gadget_path
  use return_status
  use data_types
  use particle_store

  implicit none
  private 
  save
  
  public :: catalogue_data_open
  public :: catalogue_data_init
  public :: catalogue_data_new
  public :: catalogue_data_process_events
  public :: catalogue_data_close
  public :: catalogue_data_get_ncat
  public :: catalogue_data_get_points
  public :: catalogue_data_read_all
  public :: catalogue_data_loaded

  ! Maximum length of a text label
  integer, parameter, public :: label_length = 20

  ! Type to store information about one catalogue
  type catalogue_type
     ! Location of the files
     integer              :: nfiles
     type(path_data_type) :: path_data
     ! Catalogue data
     integer                                              :: npoints
     real(kind=pos_kind),         dimension(:,:), pointer :: pos
     character(len=label_length), dimension(:),   pointer :: label
     real(kind=pos_kind),         dimension(:),   pointer :: radius
     logical :: have_label
     logical :: have_radius
     ! Column numbers for various quantities
     integer :: icol_x
     integer :: icol_y
     integer :: icol_z
     integer :: icol_label
     integer :: icol_radius
     ! Number of header lines
     integer :: header_lines
     ! Whether this catalogue has been loaded
     logical :: loaded
  end type catalogue_type

  ! Number of catalogues which are / can be loaded
  integer, parameter :: ncatmax = 20
  integer            :: ncatalogue

  ! Array of catalogues
  type(catalogue_type), dimension(ncatmax), target :: catalogue

  ! Maximum size of a catalogue
  integer, parameter :: max_catsize = 100000

  ! Widgets for the load window
  type (gui_window) :: window
  logical :: is_open = .false.
  type (gui_entrybox)     :: nfiles_entry, fname_entry
  type (gui_entrybox)     :: x_entry, y_entry, z_entry
  type (gui_entrybox)     :: label_entry, radius_entry
  type (gui_checkbox)     :: label_checkbox, radius_checkbox
  type (gui_button)       :: browse_button
  type (gui_button)       :: read_data_button
  type (gui_checkbox)     :: clear_checkbox

contains


  subroutine catalogue_data_init()
!
! Initialise or reset the module
!
    implicit none
    integer :: i
    logical, save :: first_call = .true.

    ncatalogue = 0

    if(first_call)then
       do i = 1, ncatmax, 1
          catalogue(i)%npoints = 0
          nullify(catalogue(i)%pos)
          nullify(catalogue(i)%label)
          nullify(catalogue(i)%radius)
       end do
       first_call = .false.
    endif

    do i = 1, ncatmax, 1
       call deallocate_catalogue(catalogue(i))
       catalogue(i)%loaded = .false.
    end do

    return
  end subroutine catalogue_data_init


  type (result_type) function catalogue_data_new(nfiles, filename, &
       icol_x, icol_y, icol_z, icol_label,     &
       icol_radius, header_lines)
!
! Set up the module to read a new catalogue
!
    implicit none
    ! Parameters
    integer,          intent(in) :: nfiles
    character(len=*), intent(in) :: filename
    integer,          intent(in) :: icol_radius
    integer,          intent(in) :: icol_x, icol_y, icol_z, icol_label
    integer,          intent(in) :: header_lines
    ! Internal
    type (result_type) :: res
    integer            :: isnap
    integer            :: icat
    type(catalogue_type), pointer :: cat

    ! Check we haven't overflowed ncatmax
    icat = ncatalogue + 1
    if(icat.gt.ncatmax)then
       catalogue_data_new%success = .false.
       catalogue_data_new%string  = "Unable to read any more catalogues"
       return
    endif
    
    ! Get a pointer to the new catalogue
    cat => catalogue(icat)

    ! Examine the filename and store the path
    res = gadget_path_extract(filename, isnap, cat%path_data)
    if(.not.res%success)then
       catalogue_data_new = res
       return
    endif
    
    ! Store column numbers
    cat%icol_x       = icol_x
    cat%icol_y       = icol_y
    cat%icol_z       = icol_z
    cat%icol_label   = icol_label
    cat%icol_radius  = icol_radius
    cat%header_lines = header_lines
    cat%nfiles       = nfiles

    ! Flags to indicate if we have IDs etc
    cat%have_label  = (icol_label.gt.0)
    cat%have_radius = (icol_radius.gt.0)
    cat%loaded      = .false.

    ncatalogue = ncatalogue + 1
    catalogue_data_new%success = .true.

    return
  end function catalogue_data_new


  type (result_type) function catalogue_data_read(isnap, cat)

    implicit none
    ! Parameters
    integer, intent(in)             :: isnap
    type (catalogue_type)           :: cat

    catalogue_data_read = read_catalogue(isnap, cat)

    return
  end function catalogue_data_read


  subroutine catalogue_data_read_all(isnap)
!
! Read all point catalogues
!
    implicit none
    integer :: isnap
    integer :: i
    type (result_type) :: res
    
    do i = 1, ncatalogue, 1
       res = catalogue_data_read(isnap, catalogue(i))
    end do

    return
  end subroutine catalogue_data_read_all


  type (result_type) function read_catalogue(isnap, cat)
!
! Read the data for one catalogue
!
    implicit none
    ! Parameters
    integer, intent(in)   :: isnap
    type (catalogue_type) :: cat
    ! Internal
    integer              :: ifile
    character(len=500)   :: fname
    type(catalogue_type) :: temp_cat
    integer              :: ios
    integer              :: nlines
    integer              :: i
    ! Line from the file
    integer, parameter         :: line_maxlen = 1000
    character(len=line_maxlen) :: line, col
    integer                    :: ncol, icol
    logical                    :: line_ok
    type (result_type)         :: res

    ! Clear any existing data
    cat%loaded = .false.
    call deallocate_catalogue(cat)

    ! Allocate temporary storage for the catalogue
    temp_cat = cat
    res = allocate_catalogue(temp_cat, max_catsize)
    if(.not.res%success)then
       read_catalogue = res
       return
    endif

    ! Determine number of columns to read
    ncol = 0
    ncol = max(ncol, cat%icol_x)
    ncol = max(ncol, cat%icol_y)
    ncol = max(ncol, cat%icol_z)
    ncol = max(ncol, cat%icol_label)
    ncol = max(ncol, cat%icol_radius)

    ! Count lines read
    nlines = 0

    ! Loop over files and read the data
    do ifile = 0, cat%nfiles-1, 1
       
       ! Get the name of the next file
       call gadget_path_generate(isnap, ifile, &
            fname=fname, path_data=cat%path_data)

       ! Open the file
       open(unit=1,file=fname,status='old',form='formatted',iostat=ios)
       if(ios.ne.0)then
          call deallocate_catalogue(temp_cat)
          read_catalogue%success = .false.
          read_catalogue%string  = "Unable to open file: "//trim(fname)
          return
       endif

       ! Skip header lines
       if(cat%header_lines.gt.0)then
          read(1,'(a)', iostat=ios)(line,i=1,cat%header_lines,1)
          if(ios.ne.0)then
             close(1)
             call deallocate_catalogue(temp_cat)
             read_catalogue%success = .false.
             read_catalogue%string  = "Unable to read file: "//trim(fname)
             return
          endif
       endif

       ! Read as many lines as we can
       ios = 0
       do while(ios.eq.0)
          read(1,'(a)', iostat=ios)line
          if(ios.eq.0.and.len_trim(line).gt.0)then

             nlines = nlines + 1
             line_ok = .true.
             if(nlines.gt.max_catsize)then
                call deallocate_catalogue(temp_cat)
                read_catalogue%success = .false.
                read_catalogue%string  = &
                     "Maximum catalogue size exceeded"
                close(1)
                return
             endif

             do icol = 1, ncol, 1
                call extract_next_column(line, col)

                ! Store the data from this line
                if(icol.eq.cat%icol_x)then
                   read(col,*,iostat=ios)temp_cat%pos(1,nlines)
                   line_ok = line_ok.and.(ios.eq.0).and.len_trim(col).gt.0
                endif
                if(icol.eq.cat%icol_y)then
                   read(col,*,iostat=ios)temp_cat%pos(2,nlines)
                   line_ok = line_ok.and.(ios.eq.0).and.len_trim(col).gt.0
                endif
                if(icol.eq.cat%icol_z)then
                   read(col,*,iostat=ios)temp_cat%pos(3,nlines)
                   line_ok = line_ok.and.(ios.eq.0).and.len_trim(col).gt.0
                endif
                if(icol.eq.cat%icol_radius)then
                   read(col,*,iostat=ios)temp_cat%radius(nlines)
                   line_ok = line_ok.and.(ios.eq.0).and.len_trim(col).gt.0
                endif
                if(icol.eq.cat%icol_label)then
                   temp_cat%label(nlines) = trim(adjustl(col))
                   line_ok = line_ok.and.len_trim(col).gt.0
                endif

                ! Next column
             end do

             ! Discard line if it couldn't be interpreted
             if(.not.line_ok)nlines = nlines - 1

          endif
          ! Next line
       end do
       ! Next file
       close(1)
    end do
    
    ! Check we read some lines
    if(nlines.eq.0)then
       read_catalogue%string = "No readable lines found"
       read_catalogue%success = .false.
       cat%loaded = .false.
       return
    endif

    ! Copy data into final catalogue
    cat         = temp_cat
    cat%npoints = nlines

    read_catalogue%success = .true.
    cat%loaded = .true.

    return
  end function read_catalogue
  

  type (result_type) function allocate_catalogue(cat, n)
!
! Allocate memory for the data for one catalogue
!
    implicit none
    type(catalogue_type) :: cat
    integer              :: n
    integer :: mstat

    allocate(cat%pos(3,n), cat%label(n), cat%radius(n), stat=mstat)
    if(mstat.ne.0)then
       allocate_catalogue%string  = "Unable to allocate memory for catalogue"
       allocate_catalogue%success = .false.
    else
       allocate_catalogue%success = .true.
    endif

    return
  end function allocate_catalogue


  subroutine deallocate_catalogue(cat)
!
! Deallocate the data for a catalogue
!
    implicit none
    type(catalogue_type) :: cat

    if(associated(cat%pos))    deallocate(cat%pos)
    if(associated(cat%label))  deallocate(cat%label)
    if(associated(cat%radius)) deallocate(cat%radius)

    nullify(cat%pos,cat%label,cat%radius)

    cat%loaded = .false.

    return
  end subroutine deallocate_catalogue


  subroutine extract_next_column(str, col)
!
! Remove the first number or (possibly quoted) string from str and return it
! in col
!
    implicit none
    integer :: i
    character(len=*) :: str, col
    ! Characters which can be separators. Multiple adjacent separators will
    ! be treated as one.
    character(len=3), parameter :: separators = char(9)  // & ! Tab
                                                " "       // & ! Space
                                                ","            ! Comma
    ! Quotes - these can be used to allow separator characters in strings
    character(len=2),  parameter :: quote_chars = '"'//"'"
    character :: quote

    ! First remove any leading separator characters from the string
    i = verify(str, separators)
    if(i.gt.0)str = str(i:)

    if(scan(str(1:1),quote_chars).eq.1)then
       ! If the first character is a quote, remove it and find the closing quote
       quote = str(1:1)
       str   = str(2:)
       i = scan(str,quote)
       select case(i)
       case(:0)
          ! No closing quote, so assume that the rest of the line is part
          ! of the string
          col = trim(adjustl(str))
          str = ""
       case(1)
          ! Closing quote is immediately after the opening one
          col = ""
          str = str(2:)
       case(2:)
          col = str(1:i-1)
          str = str(i+1:)
       end select
    else
       ! If there's no quote, look for the next separator
       i = scan(str,separators)
       select case(i)
       case(:0)
          ! No separators, so take the rest of the line
          col = trim(adjustl(str))
          str = ""
       case(1)
          ! First character is a separator so we have no string
          col = ""
       case(2:)
          ! Take everything up to the next separator
          col = str(1:i-1)
          str = str(i+1:)
       end select
    endif

    return
  end subroutine extract_next_column


  subroutine catalogue_data_open(mainwin)
!
! Open the catalogue data window
!
    implicit none
    type (gui_window) :: mainwin
    type (gui_box)    :: hbox, vbox
    type (gui_box)    :: file_box, col_box
    type (gui_label)  :: label

    if(is_open)return

    is_open = .true.
    call gui_create_window(window, "Load point catalogue", parent=mainwin, &
         resize=.false.)

    call gui_packing_mode(expand=.false., fill=.false., spacing=3, &
         position=gui_start)
    call gui_create_box(hbox, window, gui_horizontal)
    call gui_create_box(vbox, hbox, gui_vertical)

    ! Box with file name and number of files
    call gui_create_box(file_box, vbox, gui_vertical, frame=.true., &
         label="Location of text file(s)")
    ! Number of files
    call gui_create_box(hbox, file_box, gui_horizontal)
    call gui_create_label(label, hbox, "Number of files: ")
    call gui_packing_mode(position=gui_end)
    call gui_create_entrybox(nfiles_entry, hbox, length=5)
    call gui_packing_mode(position=gui_start)
    ! Filename
    call gui_create_box(hbox, file_box, gui_horizontal)
    call gui_create_label(label, hbox, "Name of one file: ")
    call gui_packing_mode(expand=.true.,fill=.true.)
    call gui_create_entrybox(fname_entry, hbox)
    call gui_packing_mode(expand=.false.,fill=.false.)
    call gui_create_button(browse_button,hbox,"Browse")
    ! Label
    call gui_create_box(hbox, file_box, gui_horizontal)
    call gui_create_label(label, hbox, &
         "(assumes Gadget naming convention)")

    ! Box with list of columns
    call gui_create_box(col_box, vbox, gui_vertical, frame=.true., &
         label="Columns in the file(s)")
    ! X
    call gui_create_box(hbox, col_box, gui_horizontal)
    call gui_create_label(label, hbox, "x coordinates in column ")
    call gui_packing_mode(position=gui_end)
    call gui_create_entrybox(x_entry, hbox, length=5)
    call gui_packing_mode(position=gui_start)
    ! Y
    call gui_create_box(hbox, col_box, gui_horizontal)
    call gui_create_label(label, hbox, "y coordinates in column ")
    call gui_packing_mode(position=gui_end)
    call gui_create_entrybox(y_entry, hbox, length=5)
    call gui_packing_mode(position=gui_start)
    ! Z
    call gui_create_box(hbox, col_box, gui_horizontal)
    call gui_create_label(label, hbox, "z coordinates in column ")
    call gui_packing_mode(position=gui_end)
    call gui_create_entrybox(z_entry, hbox, length=5)
    call gui_packing_mode(position=gui_start)
    ! Label
    call gui_create_box(hbox, col_box, gui_horizontal)
    call gui_create_checkbox(label_checkbox, hbox, "Label in column ")
    call gui_packing_mode(position=gui_end)
    call gui_create_entrybox(label_entry, hbox, length=5)
    call gui_packing_mode(position=gui_start)
    ! Radius
    call gui_create_box(hbox, col_box, gui_horizontal)
    call gui_create_checkbox(radius_checkbox, hbox, "Radius in column ")
    call gui_packing_mode(position=gui_end)
    call gui_create_entrybox(radius_entry, hbox, length=5)
    call gui_packing_mode(position=gui_start)

    ! Checkbox to set whether to clear existing data
    call gui_create_checkbox(clear_checkbox, vbox, &
         "Discard existing catalogue(s)")
    call gui_checkbox_set_state(clear_checkbox, .true.)

    ! Ok button
    call gui_create_box(hbox, vbox, gui_horizontal)
    call gui_packing_mode(position=gui_end)
    call gui_create_button(read_data_button, hbox, " Read data ")
    call gui_packing_mode(position=gui_start)

    call gui_show_window(window)

    return
  end subroutine catalogue_data_open


  logical function catalogue_data_process_events(mainwin)
!
! Process events from the window
!
    implicit none
    type (gui_window) :: mainwin
    logical :: ok
    character(len=500)     :: fname
    integer                :: icol_x, icol_y, icol_z, icol_label
    integer                :: icol_radius, header_lines, nfiles
    integer                :: ios
    logical                :: state
    type (result_type)     :: res
    character(len=10)      :: bt
    integer                :: isnap


    catalogue_data_process_events = .false.

    if(.not.is_open)return

    ! Close window if close box clicked
    if(gui_window_closed(window))then
       call catalogue_data_close()
       return
    endif

    if(gui_checkbox_changed(label_checkbox))then
       call update_window()
    endif

    if(gui_checkbox_changed(radius_checkbox))then
       call update_window()
    endif

    ! Pop up file selector if browse button is clicked
    if(gui_button_clicked(browse_button))then
       call gui_select_file(mainwin, &
            "Select file to read", &
            gui_file_open, ok, fname)
       if(ok)then
          call gui_entrybox_set_text(fname_entry, trim(fname))
       endif
    endif

    ! Load the catalogue if the read button is clicked
    if(gui_button_clicked(read_data_button))then
       call gui_entrybox_get_text(fname_entry, fname)
       call gui_entrybox_get_value(nfiles_entry, nfiles, ios)
       if(ios.ne.0.or.nfiles.lt.1)then
          bt = gui_display_dialog(mainwin,"error", &
               "Invalid number of files")
          return
       endif       
       call gui_entrybox_get_value(x_entry, icol_x, ios)
       if(ios.ne.0.or.icol_x.lt.1)then
          bt = gui_display_dialog(mainwin,"error", &
               "Invalid x coord column number")
          return
       endif
       call gui_entrybox_get_value(y_entry, icol_y, ios)
       if(ios.ne.0.or.icol_y.lt.1)then
          bt = gui_display_dialog(mainwin,"error", &
               "Invalid y coord column number")
          return
       endif
       call gui_entrybox_get_value(z_entry, icol_z, ios)
       if(ios.ne.0.or.icol_z.lt.1)then
          bt = gui_display_dialog(mainwin,"error", &
               "Invalid z coord column number")
          return
       endif

       call gui_checkbox_get_state(label_checkbox, state)
       if(state)then
          call gui_entrybox_get_value(label_entry, icol_label, ios)
          if(ios.ne.0.or.icol_label.lt.1)then
             bt = gui_display_dialog(mainwin,"error", &
                  "Invalid label column number")
             return
          endif
       else
          icol_label = -1
       endif
       call gui_checkbox_get_state(radius_checkbox, state)
       if(state)then
          call gui_entrybox_get_value(radius_entry, icol_radius, ios)
          if(ios.ne.0.or.icol_radius.lt.1)then
             bt = gui_display_dialog(mainwin,"error", &
                  "Invalid radius column number")
             return
          endif
       else
          icol_radius = -1
       endif

       ! Clear existing data if necessary
       call gui_checkbox_get_state(clear_checkbox, state)
       if(state)call catalogue_data_init()

       header_lines = 0
       res =  catalogue_data_new(nfiles, fname, &
            icol_x, icol_y, icol_z, icol_label,     &
            icol_radius, header_lines)
       if(.not.res%success)then
          bt = gui_display_dialog(mainwin,"error", res%string)
          return
       else
          call particle_store_contents(psample, get_isnap=isnap)
          res = catalogue_data_read(isnap, catalogue(ncatalogue))
          if(.not.res%success)then
             bt = gui_display_dialog(mainwin,"error", res%string)
             ncatalogue = ncatalogue - 1
          else
             bt = gui_display_dialog(mainwin,"info", "Read point catalogue")
             catalogue_data_process_events = .true.
          endif
       endif
    endif

    return
  end function catalogue_data_process_events


  subroutine catalogue_data_close()
!
! Close the window
!
    implicit none

    if(.not.is_open)return

    call gui_destroy_window(window)
    is_open = .false.

    return
  end subroutine catalogue_data_close


  subroutine update_window()
!
! Update the widgets in the window
!
    implicit none
    logical :: state
    
    call gui_checkbox_get_state(label_checkbox, state)
    call gui_set_sensitive(label_entry, state)

    call gui_checkbox_get_state(radius_checkbox, state)
    call gui_set_sensitive(radius_entry, state)

    return
  end subroutine update_window


  integer function catalogue_data_get_ncat()
!
! Return the number of sets of points
!
    catalogue_data_get_ncat = ncatalogue

    return
  end function catalogue_data_get_ncat


  subroutine catalogue_data_get_points(icat, npoints, pos, label, radius)
!
! Return pointers to the data for the specified catalogue
!
    implicit none
    integer, intent(in)                                :: icat
    integer, intent(out)                               :: npoints
    real(kind=pos_kind), dimension(:,:), pointer       :: pos
    real(kind=pos_kind), dimension(:),   pointer       :: radius
    character(len=label_length), dimension(:), pointer :: label

    if(icat.gt.ncatalogue)call terminate('icat out of range in catalogue_data_get_points')
    
    npoints =  catalogue(icat)%npoints
    pos     => catalogue(icat)%pos
    if(catalogue(icat)%have_label)then
       label => catalogue(icat)%label
    else
       nullify(label)
    endif
    if(catalogue(icat)%have_radius)then
       radius => catalogue(icat)%radius
    else
       nullify(radius)
    endif

    return
  end subroutine catalogue_data_get_points


  logical function catalogue_data_loaded(icat)
!
! Return true if a catalogue is loaded (may not be if file is missing
! for the current snapshot)
!
    implicit none
    integer :: icat

    catalogue_data_loaded = catalogue(icat)%loaded

    return
  end function catalogue_data_loaded


end module catalogue_data
