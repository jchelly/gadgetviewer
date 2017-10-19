module summary
!
! Produce and display a summary of the currently loaded snapshot.
!
  use f90_gui
  use particle_store
  use string_module

  implicit none
  private

  public :: summary_open
  public :: summary_generate
  public :: summary_close
  public :: summary_type
  public :: summary_process_events
  public :: summary_clear
  public :: summary_add_line
  public :: s

  ! Maximum length of a line
  integer, parameter :: linelength = 256
  ! Maximum number of lines
  integer, parameter :: maxlines   = 100

  type summary_type
     integer :: nlines
     character(len=linelength), dimension(maxlines) :: line
  end type summary_type

  ! Window and widgets
  type(gui_window) :: window
  logical          :: window_open = .false.
  type (gui_textview) :: textview

  ! Summary
  type(summary_type) :: s

contains

  subroutine summary_clear(s)
!
! Initialise a summary object
!
    implicit none
    type(summary_type) :: s

    s%nlines = 0

    return
  end subroutine summary_clear

  subroutine summary_add_line(s,str)
!
! Add an extra line to the end of a summary
!
    implicit none
    type(summary_type) :: s
    character(len=*) :: str

    ! Do nothing if we've run out of lines
    if(s%nlines.eq.maxlines)return

    s%nlines = s%nlines + 1
    s%line(s%nlines) = trim(str)

    return
  end subroutine summary_add_line

  subroutine summary_generate(s, pdata)
!
! Generate a description of the data in pdata
!
    implicit none
    type(summary_type) :: s
    type(pdata_type)   :: pdata
    ! Particle store info
    integer                                      :: nspecies
    integer(kind=index_kind), dimension(maxspecies)               :: np
    character(len=maxlen), dimension(maxspecies) :: species_names
    ! Particle properties
    integer :: nprops
    character(len=maxlen), dimension(maxprops)   :: propnames
    ! Loops etc
    integer :: i, j
    character(len=linelength) :: tmp

    ! Get information about the snapshot
    call particle_store_contents(pdata, get_nspecies=nspecies, get_np=np, &
         get_species_names=species_names)

    ! Write this to the summary
    call summary_add_line(s,"Total particles loaded: "//string(sum(np)))
    do i = 1, nspecies, 1
       if(np(i).gt.0)then
          call summary_add_line(s," ")
          call summary_add_line(s,trim(species_names(i)))
          tmp = trim(adjustl(string(np(i))))
          call summary_add_line(s,"Number of particles = "//trim(tmp))
          call particle_store_species(pdata, i, get_nprops=nprops, &
               get_propnames=propnames)
          do j = 1, nprops, 1
             call summary_add_line(s,"  - "//trim(propnames(j)))
          end do
       end if
    end do
    
    return
  end subroutine summary_generate

  subroutine summary_open(mainwin,s)
!
! Open a window and display the summary
!
    implicit none
    type(gui_window)   :: mainwin
    type(summary_type) :: s
    integer            :: i

    type(gui_box)      :: vbox, outer_hbox

    if(window_open.or.(.not.particle_store_loaded(psample)))return

    ! Set default packing mode
    call gui_packing_mode(expand=.false., fill=.false., spacing=0, &
         position=gui_start)

    ! Create the window
    call gui_create_window(window, "Simulation summary", parent=mainwin, &
         min_dimensions=(/400,300/))
    call gui_packing_mode(expand=.true., fill=.true.,spacing=3)
    call gui_create_box(outer_hbox, window, gui_horizontal, &
         scrollable=.true.)
    call gui_create_box(vbox, outer_hbox, gui_vertical)
    call gui_create_textview(textview, vbox)
    call gui_packing_mode(expand=.false., fill=.false.,spacing=0)

    ! Add text
    do i = 1, s%nlines, 1
       call gui_textview_add_line(textview, s%line(i))
    end do

    ! Display the window
    call gui_show_window(window)
    window_open = .true.

    return
  end subroutine summary_open

  subroutine summary_process_events()
!
! Process events in the summary window (e.g. click on close button)
!
    implicit none

    if(.not.window_open)return

    ! Close window if close box clicked
    if(gui_window_closed(window))then
       call summary_close()
       return
    endif

    return
  end subroutine summary_process_events

  subroutine summary_close()
!
! Close the window
!
    implicit none

    if(.not.window_open)return

    call gui_destroy_window(window)
    window_open = .false.

    return
  end subroutine summary_close

end module summary
