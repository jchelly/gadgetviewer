module info_window
!
! Displays info about the selected point
!
  use f90_gui
  use particle_store
  use view_parameters
  use data_types
  use format_strings

  implicit none
  private
  save

  public :: info_window_open
  public :: info_window_close
  public :: info_window_update
  public :: info_window_process_events

  type (gui_window)    :: window
  logical              :: window_open = .false.
  type (gui_combo_box) :: species_box
  type (gui_textview)  :: textview
  type (gui_entrybox)  :: nngb_box

  ! Currently selected particle type
  integer :: ispecies = 2
  integer :: nngb = 1
  integer, parameter :: nngbmax = 1000

contains
  
  subroutine info_window_open(mainwin)
!
! Open the window
!
    implicit none
    type (gui_window) :: mainwin
    type (gui_box)    :: vbox, hbox
    type (gui_label)  :: label

    if(window_open)then
       return
    else
       window_open = .true.
    end if

    ! Create the window
    call gui_create_window(window,"Evaluate properties at selected point",parent=mainwin,&
         resize=.true.)
    call gui_packing_mode(expand=.false., fill=.false., spacing=3, &
         position=gui_start)
    call gui_create_box(vbox, window, gui_vertical)
    call gui_create_box(hbox, vbox, gui_horizontal)
    call gui_create_label(label, hbox, "Evaluate properties for particle type ")
    call gui_create_combo_box(species_box, hbox, (/"<particle type>"/))

    call gui_create_box(hbox, vbox, gui_horizontal)
    call gui_create_label(label, hbox, "Average quantities over ")
    call gui_create_entrybox(nngb_box, hbox, 4)
    call gui_create_label(label, hbox, "nearest neighbours")

    call gui_packing_mode(expand=.true., fill=.true., spacing=0, &
         position=gui_start)
    call gui_create_box(vbox, window, gui_vertical)
    call gui_create_box(hbox, vbox, gui_horizontal, frame=.true.)
    call gui_create_textview(textview, hbox)
    call gui_packing_mode(expand=.false., fill=.false., spacing=3, &
         position=gui_start)
    call gui_show_window(window)

    ! Update text in window
    call info_window_update()

    return
  end subroutine info_window_open


  subroutine info_window_close()
!
! Close the window
!
    implicit none

    if(.not.window_open)return

    call gui_destroy_window(window)
    window_open = .false.

    return
  end subroutine info_window_close


  subroutine info_window_update()
!
! Update the window
!
    use data_types
    implicit none
    integer :: nspecies
    character(len=maxlen), dimension(maxspecies) :: species_names
    integer(kind=index_kind),               dimension(maxspecies) :: np
    character(len=500) :: str, ptype
    integer :: nprops
    character(len=maxlen), dimension(maxprops) :: prop_names
    integer :: iprop
    real(kind=real8byte)   :: res_real(3)
    integer(kind=int8byte) :: res_int(3)
    real :: fsample

    if(.not.window_open)return

    ! Get particle type names and update combo box
    call particle_store_contents(psample, get_nspecies=nspecies, &
         get_species_names=species_names, get_np=np, get_fsample=fsample)
    if(ispecies.lt.1.or.ispecies.gt.nspecies)then
       ispecies = max(1,min(ispecies, nspecies))
    endif
    call gui_combo_box_set_text(species_box, species_names(1:nspecies))
    call gui_combo_box_set_index(species_box, ispecies)

    ! Update text box
    call gui_textview_clear(textview)
    
    ! Display the sampling rate
    write(str, *)"Current sampling rate: ", fsample
    str = trim(adjustl(str))
    call gui_textview_add_line(textview, str)
    call gui_textview_add_line(textview, "")

    call gui_textview_add_line(textview, "Particle properties at this point:")
    call gui_textview_add_line(textview, "")

    if(np(ispecies).eq.0)return

    ! Nunber of neighbours
    write(str,"(1i4)")nngb
    call gui_entrybox_set_text(nngb_box, str)

    ! Property values
    call particle_store_species(pdata, ispecies, get_propnames=prop_names, &
         get_nprops=nprops)
    do iprop = 1, nprops, 1
       call particle_store_property(pdata, ispecies, iprop, get_type=ptype)
       call particle_store_evaluate_property(psample, ispecies, iprop, &
            real(view_transform%centre, pos_kind), nngb, res_real, res_int)

       if (nngb.eq.1)then
          ! One neighbour. Just display nearest value.
          if(ptype.eq."INTEGER")then
             write(str,'(a, ": nearest=", '//trim(iprop_fmt)//')') &
                  trim(adjustl(prop_names(iprop))), &
                  res_int(3)
          else
             write(str,'(a, ": nearest=", '//trim(rprop_fmt)//')') &
                  trim(adjustl(prop_names(iprop))), &
                  res_real(3)
          endif
       else
          ! Multiple neighbours. Display min, max and mean.
          if(ptype.eq."INTEGER")then
             write(str,'(a, ": mean=", '//trim(iprop_fmt)//', " min=", '//trim(iprop_fmt)//', " max="'//trim(iprop_fmt)//')') &
                  trim(adjustl(prop_names(iprop))), res_int(3), res_int(1), res_int(2)
          else
             write(str,'(a, ": mean=", '//trim(rprop_fmt)//', " min=", '//trim(rprop_fmt)//', " max="'//trim(rprop_fmt)//')') &
                  trim(adjustl(prop_names(iprop))), res_real(3), res_real(1), res_real(2)
          endif
       endif

       str = trim_spaces(str)
       call gui_textview_add_line(textview, trim(adjustl(str)))
    end do

    ! Coordinates
    call gui_textview_add_line(textview, "")
    write(str,'("Coordinates: ",'//trim(pos_fmt)//',",",'//trim(pos_fmt)//',",",'//trim(pos_fmt)//')')view_transform%centre
    call gui_textview_add_line(textview, trim(str))

    return
  end subroutine info_window_update


  subroutine info_window_process_events()
!
! Process events from the window
!
    implicit none
    character(len=500) :: str
    integer            :: n, ios

    if(.not.window_open)return

    ! Close window if close box clicked
    if(gui_window_closed(window))then
       call info_window_close()
       return
    endif

    ! Update if different particle type is selected
    if(gui_combo_box_changed(species_box))then
       call gui_combo_box_get_index(species_box, ispecies)
       call info_window_update()
    endif

    ! Update if nngb is changed
    if(gui_entrybox_changed(nngb_box))then
       call gui_entrybox_get_text(nngb_box, str)
       read(str, *, iostat=ios)n
       if(ios.eq.0.and.n.gt.0.and.n.lt.nngbmax)nngb = n
       call info_window_update()
    endif

    return
  end subroutine info_window_process_events


  function trim_spaces(str)
!
! Remove consecutive spaces from a string
!
    implicit none
    integer :: i, j
    character(len=*) :: str
    character(len=len_trim(str)) :: trim_spaces

    trim_spaces = str(1:1)
    j = 2
    do i = 2, len_trim(str), 1
       if(str(i-1:i-1).ne." ".or.str(i:i).ne." ")then
          trim_spaces(j:j) = str(i:i)
          j = j + 1
       endif
    end do
    
    return
  end function trim_spaces


end module info_window
