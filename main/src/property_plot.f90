module property_plot
!
! Module to generate plot with particles colour coded by a property
! (e.g. density, temperature etc)
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
  use colour_table
  use colour_bar, only: colour_bar_draw
  use stereo
  use projection
  use selection, only: draw_selected_only
  use math

  implicit none
  private
  save

  public :: property_plot_init
  public :: property_plot_make_image
  public :: property_plot_settings_open
  public :: property_plot_process_events
  public :: property_plot_update_settings
  public :: property_plot_set_keys
  public :: property_plot_get_keys

  ! Property to plot for each type of particle
  integer, dimension(maxspecies) :: iprop 

  ! Colour table to use for each type of particle
  integer, dimension(maxspecies, maxprops) :: itab 

  ! Range of values to show
  real, dimension(maxspecies, maxprops)    :: propmin, propmax

  ! Whether to use log scaling
  logical, dimension(maxspecies, maxprops) :: log_scale
  logical, dimension(maxspecies, maxprops) :: rand_colours

  ! Method of combining images from each particle type
  integer :: image_combine
  integer, parameter :: SHOW_NEAREST = 1, SHOW_HIGHEST = 2, SHOW_LOWEST = 3

  ! Particle type to show settings for
  integer :: ispecies

  ! Whether to display colour bars
  logical, dimension(maxspecies, maxprops) :: show_colour_bar

  type (gui_notebook) :: notebook

  ! Settings for each particle type
  type (gui_combo_box), dimension(maxspecies) :: property_box
  type (gui_combo_box), dimension(maxspecies) :: ctab_box
  type (gui_entrybox),  dimension(maxspecies) :: min_entry, max_entry
  type (gui_checkbox),  dimension(maxspecies) :: log_checkbox, cbar_checkbox,&
                                                 rand_checkbox
  type (gui_button), dimension(maxspecies)    :: rand_button

  ! Widgets controlling how to combine the images
  type (gui_radio_button) :: radio_nearest, radio_highest, radio_lowest

  ! Widgets to choose particle size
  type (gui_entrybox) :: phys_size_entry, pixel_size_entry
  type (gui_box) :: size_box

  ! Format string for entry boxes
  character(len=50), parameter :: fmt_str = '(1es10.3)'

  ! Random colours
  integer, parameter :: nrandmax = 100
  integer            :: nrand
  character, dimension(3,-nrandmax:nrandmax) :: rcol

  ! 'Splats' to use for plotting particles
  integer, parameter :: nsplatmax = 256
  type splat_type
     integer :: is
     integer, pointer, dimension(:,:) :: data
  end type splat_type
  type (splat_type), dimension(0:nsplatmax) :: splat

  ! Particle size
  real               :: psize      = 0.01
  integer            :: max_pixels = 0
  integer, parameter :: max_pixels_limit = 64

  logical :: first_call = .true.

contains

  subroutine property_plot_init(new_snapshot)
!
! Set default options for the plot and calculate range of values
! for each property
!
    implicit none
    logical :: new_snapshot
    real, dimension(:,:), pointer :: pos
    real, dimension(3) :: rmin, rmax
    real :: lbox
    integer :: nspecies
    logical, save :: first_init = .true.

    ! If this is just another snapshot in the same simulation,
    ! don't need any more initialisation
    if(new_snapshot)return

    ! These settings are overridden by the default config file
    if(first_init)then
       ! Default colour tables 
       itab(1:maxspecies,1:maxprops)  = 1
       first_init = .false.
       ! Default property - show mass by default
       iprop(1:maxspecies) = 1
       ! Default range
       propmin(1:maxspecies,1:maxprops) = 0.0
       propmax(1:maxspecies,1:maxprops) = 0.0
       ! Do log scaling by default
       log_scale(1:maxspecies,1:maxprops)    = .true.
       rand_colours(1:maxspecies,1:maxprops) = .false.
       ! Show settings for first particle type
       ispecies = 1
       ! Display colour bar by default
       show_colour_bar = .true.
       ! Show highest value by default
       image_combine = SHOW_NEAREST

       first_init = .false.
    endif

    call property_plot_set_ranges()

    ! Calculate default particle size (lbox/1000)
    if(particle_store_loaded(psample))then
       rmin =  huge(rmin)
       rmax = -huge(rmax)
       call particle_store_contents(psample, get_nspecies=nspecies)
       do ispecies = 1, nspecies, 1
          call particle_store_species(psample,ispecies,get_pos=pos)
          rmin = min(rmin, minval(pos(1:3,:),2))
          rmax = max(rmax, maxval(pos(1:3,:),2))
       end do
       lbox = maxval(rmax-rmin)
       psize = lbox / 1000.0
    endif

    ! Choose a random set of colours
    call randomize_colours()
    
    return
  end subroutine property_plot_init


  subroutine randomize_colours()
    
    implicit none
    integer :: i,j
    real, dimension(3) :: r
    real :: rtmp

    ! Choose a random set of colours
    call random_number(rtmp)
    nrand = nrandmax/2 + int(rtmp*nrandmax*0.5)
    nrand = max(nrandmax/2,min(nrand,nrandmax))

    do i = -nrand, nrand, 1
       r = 0.0
       do while(sum(r).lt.1.0)
          call random_number(r)
       end do
       do j = 1, 3, 1
          rcol(j,i) = char(min(floor(r(j)*256),255))
       end do
    end do

    return
  end subroutine randomize_colours



  subroutine property_plot_make_image(width, height, trans, image, &
       show_species)
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
    integer :: ispecies, i, j, k
    ! Particle data
    integer                        :: nspecies, nprops
    integer(kind=index_kind), dimension(maxspecies) :: np
    real(kind=pos_kind), dimension(:,:), pointer :: pos
    ! Transformed z coord
    real :: z_trans
    real(kind=pos_kind), dimension(3) :: pos_trans
    ! Projected coordinates
    integer, dimension(2) :: ip
    ! z buffer
    real, dimension(0:width*height-1) :: zbuf
    character(len=maxlen) :: proptype
    ! Pointer to the data
    real(kind=r_prop_kind),    dimension(:), pointer :: rdata
    integer(kind=i_prop_kind), dimension(:), pointer :: idata
    ! Colour index to plot
    integer :: icol
    real    :: dr
    integer :: ibar
    character(len=maxlen) :: prop_name
    integer :: is
    real :: r
    ! Selected particle flags
    integer, dimension(:), pointer :: selected

    ! Initialise splats if necessary
    if(first_call)then
       do i = 0, nsplatmax, 1
          allocate(splat(i)%data(-i:i,-i:i))
          do j = -i, i, 1
             do k = -i, i, 1
                if(i.gt.0)then
                   r = sqrt(real(j*j+k*k))/real(i)
                else
                   r = 0.0
                endif
                if(r.le.1.0)then
                   splat(i)%data(j,k) = 1
                else
                   splat(i)%data(j,k) = 0
                endif
             end do
          end do
       end do
       first_call = .false.
    endif

    ! Fill in the background
    do i = 0, (width*height*3)-1, 3
       image(i:i+2) = char(0)
    end do

    ! Initialise the z buffer
    zbuf = huge(zbuf)

    ! Get number of particles in the sample to show
    call particle_store_contents(psample,get_nspecies=nspecies, get_np=np)

    ! Loop over types of particle
    do ispecies = 1, nspecies, 1
       if(show_species(ispecies))then

          ! Get a pointer to the positions for particles of this type
          call particle_store_species(psample,ispecies,get_pos=pos,&
               get_nprops=nprops)

          ! Find out the data type for the property to be plotted
          if(iprop(ispecies).gt.nprops)cycle

          call particle_store_property(psample,ispecies,iprop(ispecies), &
               get_type=proptype)

          select case(trim(proptype))
          case("REAL")

             ! Get a pointer to the property that will determine the
             ! colour to use
             call particle_store_property(psample,ispecies,iprop(ispecies), &
                  get_rdata=rdata)

             ! Figure out scale factor to convert property value to a
             ! colour index
             if(propmax(ispecies,iprop(ispecies)).gt. &
                  propmin(ispecies,iprop(ispecies)))then
                if(.not.log_scale(ispecies,iprop(ispecies)))then
                   dr = 255.0/(propmax(ispecies,iprop(ispecies))- &
                        propmin(ispecies,iprop(ispecies)))
                else
                   if(propmax(ispecies,iprop(ispecies)).gt.0.and. &
                        propmin(ispecies,iprop(ispecies)).gt.0)then
                      dr = mylog10(abs(propmax(ispecies,iprop(ispecies)))) - &
                           mylog10(abs(propmin(ispecies,iprop(ispecies))))
                      dr = 255.0/dr
                   else
                      dr = 0.0
                   endif
                endif
             else
                ! If max is less than min, will just use icol=0
                dr = 0.0
             endif
             
             call particle_store_species(psample,ispecies,&
                  get_selected=selected)

             ! Loop over particles of this type
             do i = 1, np(ispecies), 1

                if(selected(i).eq.0.and.draw_selected_only)cycle

                ! Get the coordinates of this particle in the view
                ! coordinate system
                !ip = project(pos(1:3,i),trans,width,height,fov_x,fov_y,z_trans)
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
                z_trans = pos_trans(3)
                !
                ! End of inline project()
                !
                if(ip(1).ge.0.and.ip(2).ge.0.and.&
                     ip(1).lt.width.and.ip(2).lt.height)then
                   ! Its in front of the viewpoint and on the screen.
                   ! Determine size of the particle in pixels
                   is = ((psize*trans%scale)/(1.0+z_trans))/fov_x*width
                   is = min(is, max_pixels)
                   icol = colour_index(ispecies,rdata(i))
                   icol = max(0,min(255,icol))

                   select case(image_combine)
                   case(SHOW_NEAREST)
                      call add_particle(ip, coltab(itab(ispecies,&
                           iprop(ispecies)))%cdata(1:3,icol), z_trans)
                   case(SHOW_HIGHEST)
                      call add_particle(ip, coltab(itab(ispecies,&
                           iprop(ispecies)))%cdata(1:3,icol), -rdata(i))
                   case(SHOW_LOWEST)
                      call add_particle(ip, coltab(itab(ispecies,&
                           iprop(ispecies)))%cdata(1:3,icol), rdata(i))
                   end select

                endif

                ! Next particle
             end do

          case("INTEGER")

             ! Get a pointer to the property that will determine the
             ! colour to use
             call particle_store_property(psample,ispecies,iprop(ispecies), &
                  get_idata=idata)

             ! Figure out scale factor to convert property value to a
             ! colour index
             if(propmax(ispecies,iprop(ispecies)).gt. &
                  propmin(ispecies,iprop(ispecies)))then
                if(.not.log_scale(ispecies,iprop(ispecies)))then
                   dr = 255.0/(propmax(ispecies,iprop(ispecies))- &
                        propmin(ispecies,iprop(ispecies)))
                else
                   if(propmax(ispecies,iprop(ispecies)).gt.0.and. &
                        propmin(ispecies,iprop(ispecies)).gt.0)then
                      dr = mylog10(abs(propmax(ispecies,iprop(ispecies)))) - &
                           mylog10(abs(propmin(ispecies,iprop(ispecies))))
                      dr = 255.0/dr
                   else
                      dr = 0.0
                   endif
                endif
             else
                ! If max is less than min, will just use icol=0
                dr = 0.0
             endif

             call particle_store_species(psample,ispecies,&
                  get_selected=selected)

             ! Loop over particles of this type
             do i = 1, np(ispecies), 1

                if(selected(i).eq.0.and.draw_selected_only)cycle
                
                ! Get the coordinates of this particle in the view
                ! coordinate system
                !ip = project(pos(1:3,i),trans,width,height,fov_x,fov_y,z_trans)
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
                z_trans = pos_trans(3)
                !
                ! End of inline project()
                !
                if(ip(1).ge.0.and.ip(2).ge.0.and.&
                     ip(1).lt.width.and.ip(2).lt.height)then

                   ! Its in front of the viewpoint and on the screen.
                   ! Determine size of the particle in pixels
                   is = ((psize*trans%scale)/(1.0+z_trans))/fov_x*width
                   is = min(is, max_pixels)
                   ! Figure out what colour index to use
                   icol = colour_index(ispecies,real(idata(i)))
                   ! Make sure colour index is in range
                   icol = max(0,min(255,icol))

                   select case(image_combine)
                   case(SHOW_NEAREST)
                      ! And add the particle to the image array
                      if(.not.rand_colours(ispecies,iprop(ispecies)))then
                         call add_particle(ip, &
                              coltab(itab(ispecies,iprop(ispecies)))% &
                              cdata(1:3,icol), z_trans)
                      else
                         call add_particle(ip, &
                              rcol(1:3,mod(idata(i),&
                              int(nrand,i_prop_kind))+1),&
                              z_trans)
                      endif
                   case(SHOW_HIGHEST)
                      ! And add the particle to the image array
                      if(.not.rand_colours(ispecies,iprop(ispecies)))then
                         call add_particle(ip, &
                              coltab(itab(ispecies,iprop(ispecies)))% &
                              cdata(1:3,icol),-real(idata(i)))
                      else
                         call add_particle(ip, &
                              rcol(1:3,mod(idata(i),&
                              int(nrand,i_prop_kind))+1), -real(idata(i)))
                      endif
                   case(SHOW_LOWEST)
                      ! And add the particle to the image array
                      if(.not.rand_colours(ispecies,iprop(ispecies)))then
                         call add_particle(ip, &
                              coltab(itab(ispecies,iprop(ispecies)))% &
                              cdata(1:3,icol), real(idata(i)))
                      else
                         call add_particle(ip, &
                              rcol(1:3,mod(idata(i),&
                              int(nrand,i_prop_kind))+1),real(idata(i)))
                      endif
                   end select
                endif
                ! Next particle
             end do
          case default
             call terminate('Unrecognised data type in property_plot_make_image()')
          end select

       endif
       ! Next particle type
    end do

    ! Draw colour bar(s)
    ibar = 0
    do ispecies = 1, nspecies, 1

       call particle_store_property(psample,ispecies,iprop(ispecies), &
            get_type=proptype)

       if(.not.(rand_colours(ispecies,iprop(ispecies)).and.proptype.eq."INTEGER"))then
          if(show_colour_bar(ispecies,iprop(ispecies)))then
             if(show_species(ispecies).and.itab(ispecies,iprop(ispecies)).gt.1)&
                  then       
                call particle_store_property(psample, ispecies, iprop(ispecies), &
                     get_name=prop_name)
                if(.not.log_scale(ispecies,iprop(ispecies)))then
                   call colour_bar_draw(image, width, height, &
                        width-170, 30+ibar*70, &
                        150, 20, itab(ispecies,iprop(ispecies)), &
                        rmin=propmin(ispecies,iprop(ispecies)), &
                        rmax=propmax(ispecies,iprop(ispecies)),&
                        name=prop_name)
                else if(propmin(ispecies,iprop(ispecies)).gt.0.and. &
                     propmax(ispecies,iprop(ispecies)).gt.0)then
                   call colour_bar_draw(image, width, height, &
                        width-170, 30+ibar*70, &
                        150, 20, itab(ispecies,iprop(ispecies)), &
                        rmin=mylog10(propmin(ispecies,iprop(ispecies))), &
                        rmax=mylog10(propmax(ispecies,iprop(ispecies))), &
                        name="Log("//trim(adjustl(prop_name))//")")
                endif
                ibar = ibar + 1
             endif
          endif
       endif
    end do

    return

  contains

    integer function colour_index(ispecies, rdata)
      !
      ! Calculate colour index to use
      !
      implicit none
      integer :: ispecies
      real    :: rdata
      real(kind=r_prop_kind) :: rcol

      ! Figure out what colour index to use
      if(.not.log_scale(ispecies,iprop(ispecies)))then
         rcol = (rdata - &
              propmin(ispecies,iprop(ispecies)))*dr
      else if(rdata.gt.0.0.and.propmin(ispecies, iprop(ispecies)).gt.0.0)then
         rcol = (mylog10(abs(rdata)) - &
              mylog10(abs(propmin(ispecies, &
              iprop(ispecies)))))*dr
      else
         rcol = 0
      endif
      ! Make sure colour index is in range
      colour_index = max(0.0_r_prop_kind,min(255.0_r_prop_kind,rcol))

      return
    end function colour_index

    
    subroutine add_particle(ip, col, zval)
!
! Draw a particle at position (ip(1),ip(2)) on the screen
!
      implicit none
      integer,   dimension(2) :: ip
      character, dimension(3) :: col
      integer :: k, l
      real :: zval
      
      do l = max(0,ip(2)-is), min(height-1,ip(2)+is), 1
         do k = max(0,ip(1)-is), min(width-1,ip(1)+is), 1
            if(splat(is)%data(k-ip(1),l-ip(2)).gt.0.and. &
                 zval.lt.zbuf(k+width*l))then
               image(0+3*k+3*width*l:2+3*k+3*width*l) = col
               zbuf(k+width*l) = zval
            endif
         end do
      end do

      return
    end subroutine add_particle

  end subroutine property_plot_make_image
    

  subroutine property_plot_settings_open(mainwin, window,last_type_selected)
!
! Open configuration window for the property_plotter
!
    implicit none
    ! Window handle
    type (gui_window) :: mainwin, window
    integer :: last_type_selected
    ! Top level boxes
    type (gui_box)    :: hbox, vbox, settings_box
    type (gui_label)  :: label
    ! Loop index
    integer :: i
    character(len=50) :: str
    ! Colour tables
    integer :: ntab
    character(len=coltab_name_length), dimension(ncoltabmax) :: ctab_names
    integer :: nspecies

    ! Set packing mode
    call gui_packing_mode(expand=.false., fill=.false., spacing=3, &
         position=gui_start)

    ! Get colour table names
    call colour_table_list(ntab, ctab_names)

    ! Create the window
    call gui_create_window(window,"Property plot settings",parent=mainwin, &
         resize=.false.)

    ! Top level vbox
    call gui_create_box(vbox, window, gui_vertical)

    ! Notebook for particle types
    call gui_create_box(hbox, vbox, gui_horizontal)
    call gui_create_notebook(notebook, hbox)

    call particle_store_contents(pdata, get_nspecies=nspecies)
    do i = 1, nspecies, 1
       write(str,'(i8)')i-1
       str = "Type "//trim(adjustl(str))
       call gui_create_box_in_notebook(settings_box, notebook, str)
    
       ! Property to show
       call gui_create_box(hbox, settings_box, gui_horizontal)
       call gui_create_label(label, hbox, "Colour particles by: ")
       call gui_create_combo_box(property_box(i), hbox, (/ "<property>" /))
    
       ! Range of values to show
       call gui_create_box(hbox, settings_box, gui_horizontal)
       call gui_create_label(label, hbox, "Minimum value: ")
       call gui_create_entrybox(min_entry(i), hbox, 10)
       call gui_create_label(label, hbox, "Maximum value: ")
       call gui_create_entrybox(max_entry(i), hbox, 10)

       ! Colour table to use
       call gui_create_box(hbox, settings_box, gui_horizontal)
       call gui_create_label(label, hbox, "Colour table to use: ")
       call gui_create_combo_box(ctab_box(i), hbox, (/ "<colour table>" /))

       ! Log scaling
       call gui_create_box(hbox, settings_box, gui_horizontal)
       call gui_create_checkbox(log_checkbox(i), hbox, "Logarithmic scaling")
       call gui_create_checkbox(cbar_checkbox(i), hbox, "Show colour bar")
    
       ! Checkbox for random colours
       call gui_create_box(hbox, settings_box, gui_horizontal)
       call gui_create_checkbox(rand_checkbox(i), hbox, &
            "Use random colours for integer properties")
       call gui_packing_mode(position=gui_end)
       call gui_create_button(rand_button(i), hbox, "Randomize")
       call gui_packing_mode(position=gui_start)

    end do

    ! How to combine images
    call gui_create_box(settings_box, vbox, gui_vertical, frame=.true., &
         expander=.true., label="When several particles map to one pixel...")
    call gui_create_radio_button(radio_nearest, settings_box, &
         "Display particle closest to viewpoint")
    call gui_create_radio_button(radio_highest, settings_box, &
         "Display particle with highest value", radio_nearest)
    call gui_create_radio_button(radio_lowest,  settings_box, &
         "Display particle with lowest value", radio_highest)

    ! Particle size settings
    call gui_create_box(size_box, vbox, gui_vertical, frame=.true., &
         expander=.true., label="Displayed particle sizes...")
    call gui_create_box(hbox, size_box, gui_horizontal)
    call gui_create_label(label, hbox, "Physical size: ")
    call gui_create_entrybox(phys_size_entry, hbox, 10)
    call gui_create_label(label, hbox, "Max size (pixels): ")
    call gui_create_entrybox(pixel_size_entry, hbox, 10)

    call gui_show_window(window)

    if(last_type_selected.ge.1.and.last_type_selected.le.nspecies) &
         call gui_notebook_set_page(notebook, last_type_selected-1)

    call property_plot_update_settings()

    return
  end subroutine property_plot_settings_open


  subroutine property_plot_update_settings()
!
! Update the various widgets in the settings window
!
    implicit none
    integer :: nspecies, nprops, ntab
    character(len=maxlen), dimension(maxspecies) :: species_names
    character(len=coltab_name_length), dimension(ncoltabmax) :: ctab_names
    character(len=maxlen), dimension(maxprops)   :: prop_names
    integer :: i

    character(len=50)  :: str
    character(len=maxlen) :: proptype
    logical :: random_colours

    call particle_store_contents(pdata, get_nspecies=nspecies, &
         get_species_names=species_names)

    ! Property box
    do i = 1, nspecies, 1
       call particle_store_species(pdata, i, get_nprops=nprops, &
            get_propnames=prop_names)
       if(iprop(i).gt.nprops)iprop(i) = 1
       call gui_combo_box_set_text(property_box(i), prop_names(1:nprops))
       call gui_combo_box_set_index(property_box(i), iprop(i))

       ! Colour table box
       call colour_table_list(ntab, ctab_names)
       call gui_combo_box_set_text(ctab_box(i), ctab_names(1:ntab))
       call gui_combo_box_set_index(ctab_box(i), itab(i,iprop(i)))

       ! Min and max value boxes
       write(str,fmt_str)propmin(i, iprop(i))
       call gui_entrybox_set_text(min_entry(i), trim(str))
       write(str,fmt_str)propmax(i, iprop(i))
       call gui_entrybox_set_text(max_entry(i), trim(str))

       ! Log checkbox
       call gui_checkbox_set_state(log_checkbox(i), &
            log_scale(i,iprop(i)))
    
       ! Random colours checkbox
       call gui_checkbox_set_state(rand_checkbox(i), &
            rand_colours(i,iprop(i)))

       ! Display colour bar checkbox
       call gui_checkbox_set_state(cbar_checkbox(i), &
            show_colour_bar(i,iprop(i)))

       ! Particle size boxes
       write(str,fmt_str)psize
       call gui_entrybox_set_text(phys_size_entry, trim(str))
       write(str,'(1i8)')max_pixels
       str = trim(adjustl(str))
       call gui_entrybox_set_text(pixel_size_entry, trim(str))
       
       ! Gray out irrelevant options if using random colours
       call particle_store_property(psample,i,iprop(i), &
            get_type=proptype)
       random_colours = (rand_colours(i,iprop(i)).and.proptype.eq."INTEGER")
       call gui_set_sensitive(min_entry(i), .not.random_colours)
       call gui_set_sensitive(max_entry(i), .not.random_colours)
       call gui_set_sensitive(ctab_box(i),  .not.random_colours)
       call gui_set_sensitive(log_checkbox(i),  .not.random_colours)
       call gui_set_sensitive(cbar_checkbox(i), .not.random_colours)
       call gui_set_sensitive(rand_button(i), random_colours)

    end do

    if(image_combine.eq.SHOW_HIGHEST)&
         call gui_radio_button_set_state(radio_highest, .true.)
    if(image_combine.eq.SHOW_LOWEST)&
         call gui_radio_button_set_state(radio_lowest, .true.)
    if(image_combine.eq.SHOW_NEAREST)&
         call gui_radio_button_set_state(radio_nearest, .true.)

    return
  end subroutine property_plot_update_settings


  logical function property_plot_process_events()
!
! Process events for the configuration window
!
    implicit none
    logical :: need_update
    character(len=50) :: str
    integer :: ios
    real :: rval
    integer :: ival
    logical :: set
    integer :: nspecies
    integer :: i

    call particle_store_contents(pdata, get_nspecies=nspecies)

    ! Set to true if widgets in settings window need updating
    need_update = .false.

    ! Set to true if graphics window should be redrawn
    property_plot_process_events = .false.

    do i = 1, nspecies, 1
       ! Check if a different particle property has been selected
       if(gui_combo_box_changed(property_box(i)))then
          call gui_combo_box_get_index(property_box(i), iprop(i))
          property_plot_process_events = .true.
          need_update = .true.
       endif

       ! Check if colour table is changed
       if(gui_combo_box_changed(ctab_box(i)))then
          call gui_combo_box_get_index(ctab_box(i), itab(i,iprop(i)))
          property_plot_process_events = .true.
       endif

       ! Check if log tickbox is changed
       if(gui_checkbox_changed(log_checkbox(i)))then
          call gui_checkbox_get_state(log_checkbox(i), &
               log_scale(i,iprop(i)))
          property_plot_process_events = .true.
       endif

       ! Check if random colours tickbox changed
       if(gui_checkbox_changed(rand_checkbox(i)))then
          call gui_checkbox_get_state(rand_checkbox(i), &
               rand_colours(i,iprop(i)))
          need_update = .true.
          property_plot_process_events = .true.
       endif

       ! Check if lower limit is changed
       if(gui_entrybox_changed(min_entry(i)))then
          call gui_entrybox_get_text(min_entry(i),str)
          read(str,*,iostat=ios)rval
          if(ios.eq.0)then
             propmin(i,iprop(i)) = rval
             property_plot_process_events = .true.
             write(str,fmt_str)rval
             call gui_entrybox_set_text(min_entry(i),str)
          endif
          write(str,fmt_str)propmin(i,iprop(i))
          call gui_entrybox_set_text(min_entry(i),str)
       endif

       ! Check if upper limit is changed
       if(gui_entrybox_changed(max_entry(i)))then
          call gui_entrybox_get_text(max_entry(i),str)
          read(str,*,iostat=ios)rval
          if(ios.eq.0)then
             propmax(i,iprop(i)) = rval
             property_plot_process_events = .true.
             write(str,fmt_str)rval
             call gui_entrybox_set_text(max_entry(i),str)
          endif
          write(str,fmt_str)propmax(i,iprop(i))
          call gui_entrybox_set_text(max_entry(i),str)
       endif

       if(gui_button_clicked(rand_button(i)))then
          call randomize_colours()
          property_plot_process_events = .true.
       end if

       if(gui_checkbox_changed(cbar_checkbox(i)))then
          call gui_checkbox_get_state(cbar_checkbox(i), &
               show_colour_bar(i,iprop(i)))
          property_plot_process_events = .true.
       endif

    end do

    ! Check if radio button state is changed
    if(  gui_radio_button_changed(radio_nearest).or. &
         gui_radio_button_changed(radio_highest).or. &
         gui_radio_button_changed(radio_lowest))then
       call gui_radio_button_get_state(radio_nearest, set)
       if(set)image_combine = SHOW_NEAREST
       call gui_radio_button_get_state(radio_highest, set)
       if(set)image_combine = SHOW_HIGHEST
       call gui_radio_button_get_state(radio_lowest, set)
       if(set)image_combine = SHOW_LOWEST
       property_plot_process_events = .true.
    endif

    ! Check if physical size is changed
    if(gui_entrybox_changed(phys_size_entry))then
       call gui_entrybox_get_text(phys_size_entry,str)
       read(str,*,iostat=ios)rval
       if(ios.eq.0)then
          psize = rval
          write(str,fmt_str)rval
          call gui_entrybox_set_text(phys_size_entry,str)
          property_plot_process_events = .true.
       endif
    endif

    ! Check if max size in pixels is changed
    if(gui_entrybox_changed(pixel_size_entry))then
       call gui_entrybox_get_text(pixel_size_entry,str)
       read(str,*,iostat=ios)ival
       if(ios.eq.0)then
          max_pixels = min(ival, max_pixels_limit)
          write(str,'(1i8)')ival
          str = trim(adjustl(str))
          call gui_entrybox_set_text(pixel_size_entry,str)
          property_plot_process_events = .true.
       endif
    endif

    if(need_update)call property_plot_update_settings()

    return
  end function property_plot_process_events


  subroutine property_plot_set_ranges()
!
! Initialise the range of values to show
!
    implicit none
    integer :: i, j
    integer :: nspecies, nprops
    real, dimension(2) :: rng
    integer(kind=index_kind) :: np

    call particle_store_contents(pdata, get_nspecies=nspecies)
    do i = 1, nspecies, 1
       call particle_store_species(pdata, i, get_nprops=nprops, get_np=np)
       if(np.gt.0)then
          do j = 1, nprops, 1
             call particle_store_property(pdata, i, j, get_range=rng)
             propmin(i,j) = rng(1)
             propmax(i,j) = rng(2)
          end do
       else
          propmin(i,:) = 0.0
          propmax(i,:) = 1.0
       endif
    end do

    return
  end subroutine property_plot_set_ranges


  subroutine property_plot_set_keys()

    use key_file
    implicit none
    character(len=100) :: str
    integer            :: ispecies
    integer            :: nprops

    call set_key("Property Plot", "Combine Images", image_combine)
    do ispecies = 1, maxspecies, 1
       write(str,*)ispecies-1
       str = ", Particle Type "//trim(adjustl(str))
       call particle_store_species(psample,ispecies,get_nprops=nprops)
       call set_key("Property Plot"//trim(str), "Property",       iprop(ispecies))
       call set_key("Property Plot"//trim(str), "Colour Table",   itab(ispecies,1:nprops))
       call set_key("Property Plot"//trim(str), "Minimum Value",  propmin(ispecies,1:nprops))
       call set_key("Property Plot"//trim(str), "Maximum Value",  propmax(ispecies,1:nprops))
       call set_key("Property Plot"//trim(str), "Log Scale",      log_scale(ispecies,1:nprops))
       call set_key("Property Plot"//trim(str), "Colour Bar",     show_colour_bar(ispecies,1:nprops))
       call set_key("Property Plot"//trim(str), "Random Colours", rand_colours(ispecies,1:nprops))
    end do

    return
  end subroutine property_plot_set_keys


  subroutine property_plot_get_keys()

    use key_file
    implicit none
    character(len=100) :: str
    integer            :: ispecies

    call get_key("Property Plot", "Combine Images", image_combine)
    do ispecies = 1, maxspecies, 1
       write(str,*)ispecies-1
       str = ", Particle Type "//trim(adjustl(str))
       call get_key("Property Plot"//trim(str), "Property",       iprop(ispecies))
       call get_key("Property Plot"//trim(str), "Colour Table",   itab(ispecies,:))
       call get_key("Property Plot"//trim(str), "Minimum Value",  propmin(ispecies,:))
       call get_key("Property Plot"//trim(str), "Maximum Value",  propmax(ispecies,:))
       call get_key("Property Plot"//trim(str), "Log Scale",      log_scale(ispecies,:))
       call get_key("Property Plot"//trim(str), "Colour Bar",     show_colour_bar(ispecies,:))
       call get_key("Property Plot"//trim(str), "Random Colours", rand_colours(ispecies,:))
    end do

    return
  end subroutine property_plot_get_keys


end module property_plot
