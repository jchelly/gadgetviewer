module dotplot
!
! Module to generate dotplots with particles coloured by
! type
!
! View coordinate system
!
! Viewpoint is at (0,0,-1)
!
! Z coordinates increase into the screen.
!
! Particle positions are projected onto a plane of size fov_x * fov_y
! at z=0.
!
!

  use particle_store
  use transform
  use view_parameters
  use data_types
  use f90_gui
  use stereo
  use projection
  use selection, only: draw_selected_only

  implicit none
  private
  save

  public :: dotplot_init
  public :: dotplot_make_image
  public :: dotplot_settings_open
  public :: dotplot_process_events
  public :: dotplot_update_settings
  public :: dotplot_set_keys
  public :: dotplot_get_keys

  ! Colours to use
  character, dimension(3,maxspecies) :: col
  character, dimension(3) :: bg_col

  ! Buttons for the settings window
  type (gui_colour_button), dimension(maxspecies) :: cbutton
  type (gui_colour_button) :: bg_cbutton

contains

  subroutine dotplot_init(new_snapshot)
!
! Set default colours for the dotplotter
!
    implicit none
    logical :: new_snapshot
    logical, save :: first_init = .true.

    ! If this is just another snapshot in the same simulation,
    ! don't need any more initialisation
    if(new_snapshot)return

    if(first_init)then
       col = char(64)
       col(1:3,1) = (/char(255),char(000),char(000)/)
       col(1:3,2) = (/char(000),char(255),char(255)/)
       col(1:3,3) = (/char(128),char(128),char(255)/)
       col(1:3,4) = (/char(000),char(000),char(255)/)
       col(1:3,5) = (/char(255),char(255),char(000)/)
       col(1:3,6) = (/char(000),char(255),char(000)/)
       bg_col     = (/char(000),char(000),char(000)/)
       first_init = .false.
    endif

    return
  end subroutine dotplot_init


  subroutine dotplot_make_image(width, height, trans, image, show_species)
!
! Generate a plot of the particle distribution
!
    implicit none

    ! Parameters
    integer, intent(in)                                     :: width, height
    type (transform_type), intent(in)                       :: trans
    character, dimension(0:3*width*height-1), intent(inout) :: image
    logical, dimension(:), intent(in) :: show_species
    ! Internal
    integer :: ispecies, i, j
    ! Particle data
    integer                        :: nspecies
    integer(kind=index_kind), dimension(maxspecies) :: np
    real(kind=pos_kind), dimension(:,:), pointer :: pos
    ! Transformed z coord of a particle
    real(kind=pos_kind), dimension(3) :: pos_trans
    ! Projected coordinates
    integer, dimension(2) :: ip
    ! Depth buffer
    real, dimension(0:width*height-1) :: zbuf
    ! Selected particle flags
    integer, dimension(:), pointer :: selected

    ! Fill in the background
    do i = 0, (width*height*3)-1, 3
       image(i:i+2) = bg_col(1:3)
    end do
    zbuf = huge(zbuf)
    
    ! Get number of particles in the sample to show
    call particle_store_contents(psample,get_nspecies=nspecies, get_np=np)

    ! Loop over types of particle
    do ispecies = 1, nspecies, 1
       if(show_species(ispecies))then
          ! Get a pointer to the positions for particles of this type
          call particle_store_species(psample,ispecies,get_pos=pos)
          call particle_store_species(psample,ispecies,get_selected=selected)
          ! Loop over particles of this type
          do i = 1, np(ispecies), 1
             ! Get the coordinates of this particle in the view
             ! coordinate system
             if(selected(i).eq.0.and.draw_selected_only)cycle
             !
             ! Inline version of project()
             !
             do j = 1, 3, 1
                pos_trans(j) = &
                     dot_product(pos(1:3,i)-trans%centre(1:3), &
                     trans%matrix(j,1:3)) * trans%scale
             end do
             pos_trans(1) = pos_trans(1) + stereo_shift
             pos_trans(3) = pos_trans(3) + z_offset
             if(pos_trans(3).gt.nearclip.and.pos_trans(3).lt.farclip)then
                if(perspective_projection)then
                   ip(1) = floor((pos_trans(1)/(1.0+pos_trans(3))-stereo_shift)/ &
                        fov_x*width) + width/2
                   ip(2) = floor((-pos_trans(2)/(1.0+pos_trans(3)))/fov_y*height)+&
                        height/2
                else
                   ip(1) = floor((pos_trans(1)-stereo_shift)/ &
                        fov_x*width) + width/2
                   ip(2) = floor((-pos_trans(2))/fov_y*height)+&
                        height/2
                endif
             else
                ip(1:2) = (/-1000,-1000/)
             endif
             !
             ! End of inline project()
             !
             if(ip(1).ge.0.and.ip(2).ge.0.and.&
                  ip(1).lt.width.and.ip(2).lt.height)then
                if(pos_trans(3).lt.zbuf(ip(1)+width*ip(2)))then
                   image(0+3*ip(1)+3*width*ip(2)) = col(1,ispecies)
                   image(1+3*ip(1)+3*width*ip(2)) = col(2,ispecies)
                   image(2+3*ip(1)+3*width*ip(2)) = col(3,ispecies)
                   zbuf(ip(1)+width*ip(2)) = pos_trans(3)
                endif
             endif
          end do
       endif
    end do
    
    return
  end subroutine dotplot_make_image
    

  subroutine dotplot_settings_open(mainwin,window)
!
! Open configuration window for the dotplotter
!
    implicit none
    ! Window handle
    type (gui_window) :: mainwin, window
    ! Buttons and labels
    type (gui_label) :: label
    ! Packing boxes
    type (gui_box) :: vbox
    type (gui_box) :: hbox
    ! Loop index
    integer :: i
    integer :: nspecies
    ! Labels
    character(len=maxlen), dimension(maxspecies) :: species_names

    ! Create the window
    call gui_create_window(window, "Dotplot settings",parent=mainwin, &
         resize=.false.)
    
    ! Set packing mode
    call gui_packing_mode(expand=.false., fill=.false., spacing=3, &
         position=gui_start)

    ! Background vbox
    call gui_create_box(vbox, window, gui_vertical, frame=.true.)

    ! Put a colour button for each particle type in the window
    call particle_store_contents(pdata,get_nspecies=nspecies, &
         get_species_names=species_names)
    do i = 1, nspecies, 1
       call gui_packing_mode(expand=.false., fill=.false., spacing=3, &
            position=gui_start)
       call gui_create_box(hbox, vbox, gui_horizontal)
       call gui_create_label(label, hbox, species_names(i))
       call gui_packing_mode(expand=.false., fill=.false., spacing=3, &
            position=gui_end)
       call gui_create_colour_button(cbutton(i), hbox)
       ! Set the button colour to the currently used colour
       call gui_colour_button_set_colour(cbutton(i), &
            ichar(col(1,i)), ichar(col(2,i)), ichar(col(3,i)))
    end do

    call gui_packing_mode(expand=.false., fill=.false., spacing=3, &
         position=gui_start)

    call gui_create_box(vbox, window, gui_vertical, frame=.true.)
    call gui_create_box(hbox, vbox, gui_horizontal)
    call gui_create_label(label, hbox, "Background colour")
    call gui_create_colour_button(bg_cbutton, hbox)
    call gui_colour_button_set_colour(bg_cbutton, &
         ichar(bg_col(1)), ichar(bg_col(2)), ichar(bg_col(3)))
    call gui_show_window(window)

    return
  end subroutine dotplot_settings_open


  logical function dotplot_process_events()
!
! Process events for the configuration window
!
    implicit none
    integer :: r,g,b
    integer :: i, nspecies
    logical :: res

    res = .false.
    call particle_store_contents(pdata,get_nspecies=nspecies)

    if(gui_colour_button_changed(bg_cbutton))then
       call gui_colour_button_get_colour(bg_cbutton,r,g,b)
       bg_col(1) = char(r)
       bg_col(2) = char(g)
       bg_col(3) = char(b)
       res = .true.
    endif

    do i = 1, nspecies, 1
       if(gui_colour_button_changed(cbutton(i)))then
          call gui_colour_button_get_colour(cbutton(i),r,g,b)
          col(1,i) = char(r)
          col(2,i) = char(g)
          col(3,i) = char(b)
          res = .true.
       endif
    end do

    dotplot_process_events = res

    return
  end function dotplot_process_events


  subroutine dotplot_update_settings()
!
! Dotplotter doesn't have anything that needs updating
!
    implicit none

    return
  end subroutine dotplot_update_settings


  subroutine dotplot_set_keys()

    use key_file
    implicit none

    call set_key("Dotplot", "Red",   ichar(col(1,1:maxspecies)))
    call set_key("Dotplot", "Green", ichar(col(2,1:maxspecies)))
    call set_key("Dotplot", "Blue",  ichar(col(3,1:maxspecies)))
    call set_key("Dotplot", "Background", ichar(bg_col))

    return
  end subroutine dotplot_set_keys


  subroutine dotplot_get_keys()

    use key_file
    implicit none
    integer, dimension(maxspecies) :: icol
    integer, dimension(3)          :: ibg

    call get_key("Dotplot", "Red", icol)
    col(1,1:maxspecies) = char(icol)
    call get_key("Dotplot", "Green", icol)
    col(2,1:maxspecies) = char(icol)
    call get_key("Dotplot", "Blue",  icol)
    col(3,1:maxspecies) = char(icol)
    call get_key("Dotplot", "Background", ibg)
    bg_col = char(ibg)

    return
  end subroutine dotplot_get_keys

end module dotplot
