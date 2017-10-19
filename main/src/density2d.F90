module density2d
!
! Module to generate plot showing surface density of particles
! on the screen.
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
  use stereo
  use projection
  use selection, only: draw_selected_only
  use math

  implicit none
  private
  save

  public :: density2d_init
  public :: density2d_make_image
  public :: density2d_settings_open
  public :: density2d_process_events
  public :: density2d_update_settings
  public :: density2d_set_keys
  public :: density2d_get_keys

  ! Colour table to use for each type of particle
  integer, dimension(maxspecies) :: itab

  ! Scale factor to apply for each type of particle
  real, dimension(maxspecies)    :: min_scale, max_scale
  
  ! Scale factor for particle masses
  real, dimension(maxspecies) :: mass_scale

  ! Box with settings for one type
  type (gui_box)       :: settings_box

  ! Widgets for each particle type
  type (gui_combo_box),    dimension(maxspecies) :: ctab_box
  type (gui_slider),       dimension(maxspecies) :: min_slider, max_slider
  type (gui_drawing_area), dimension(maxspecies) :: drawing_area

  ! Notebook widget
  type (gui_notebook)  :: notebook

contains

  subroutine density2d_init(new_snapshot)
!
! Set default options for the 2d density plot
!
    implicit none
    logical :: new_snapshot
    real    :: mtot
    integer :: i
    integer                         :: nspecies
    integer(kind=index_kind), dimension(maxspecies)  :: np
    real(kind=r_prop_kind), dimension(:), pointer :: mass
    logical, save :: first_init = .true.

    ! If this is a new simulation, flag mass_scale as not calculated yet
    if(.not.new_snapshot)mass_scale = -1.0

    ! Figure out mass density normalisation for species that haven't been
    ! done yet
    call particle_store_contents(psample,get_nspecies=nspecies, get_np=np)
    do i = 1, nspecies, 1
       if(np(i).gt.0.and.mass_scale(i).le.0.0)then
          call particle_store_species(psample,i,get_mass=mass)
          mtot = sum(mass(1:np(i)))
          mass_scale(i) = 1.0/(mtot/np(i)) ! = 1/mean particle mass
       endif
    end do

    ! If this is just another snapshot in the same simulation,
    ! don't need any more initialisation
    if(new_snapshot)return

    ! Default colour tables (don't override default config file)
    if(first_init)then
       itab(1:maxspecies) = 1
       itab(1) = 3
       itab(2) = 4
       itab(3) = 4
       itab(4) = 4 
       itab(5) = 5
       itab(6) = 2
       first_init = .false.
    endif

    ! Default scale factors
    min_scale(1:maxspecies) = 30.0
    max_scale(1:maxspecies) = 70.0

    return
  end subroutine density2d_init


  subroutine density2d_make_image(width, height, trans, image, show_species)
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
    integer :: nthreads, ithread, offset
    ! Particle data
    integer                        :: nspecies
    integer(kind=index_kind), dimension(maxspecies) :: np
    real(kind=pos_kind), dimension(:,:), pointer :: pos
    ! Transformed positions
    real(kind=pos_kind), dimension(3) :: pos_trans
    ! Range in log10(count) to map onto the colour table
    real, dimension(maxspecies) :: lcmin, lcmax, dlc
    ! Projected coordinates
    integer, dimension(2) :: ip
    ! Count of mass per pixel
    !real, dimension(0:width*height-1) :: mcount
    real, dimension(:), allocatable, save :: mcount
    integer :: icol, jcol
    ! Factor to scale count by
    real :: sfac
    ! Pointer to the mass array
    real(kind=r_prop_kind), dimension(:), pointer :: mass
    ! Selected particle flags
    integer, dimension(:), pointer :: selected
    ! OpenMP routines
#ifdef _OPENMP
    integer, external :: omp_get_max_threads, omp_get_thread_num
#endif

#ifdef _OPENMP
    nthreads = omp_get_max_threads()
#else
    nthreads = 1
#endif

    ! Set size of buffer for generating images
    if(.not.allocated(mcount))then
       allocate(mcount(0:width*height*nthreads-1))
    else if(size(mcount).lt.width*height*nthreads)then
       deallocate(mcount)
       allocate(mcount(0:width*height*nthreads-1))
    endif

    ! Decide range of log10(m*mass_scale) to display
    lcmin = 1.0 + 5.0*((min_scale-50.0)/50.0)
    lcmax = 1.0 + 5.0*((max_scale-50.0)/50.0)
    dlc   = lcmax - lcmin

    ! Fill in the background
    do i = 0, (width*height*3)-1, 3
       image(i:i+2) = char(0)
    end do
    
    ! Get number of particles in the sample to show
    call particle_store_contents(psample,get_nspecies=nspecies, get_np=np)

    ! Loop over types of particle
    do ispecies = 1, nspecies, 1
       if(show_species(ispecies))then

          ! Get a pointer to the positions, masses for particles of this type
          call particle_store_species(psample, ispecies, get_pos=pos, &
               get_mass=mass, get_selected=selected)

          ! Initialise mass in each pixel
          mcount(:) = 0.0

          ! Take into account sampling rate in mass scaling. Also
          ! increase scaling when zoomed in to compensate for lower
          ! surface density of particles. Should use square of the scale
          ! factor here, but in practice this seems to be too extreme
          ! so use **0.5 instead.
          sfac = mass_scale(ispecies) * (trans%scale/scalefac)**0.5 / fdisplay / ((pixelsize_x*pixelsize_y)/4.3e-6)

          ! Loop over particles of this type
!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(ithread, offset, i, j, ip, pos_trans)
#ifdef _OPENMP
          ithread = omp_get_thread_num()
#else
          ithread = 0
#endif          
          offset = ithread * width * height
!$OMP DO SCHEDULE(DYNAMIC,1000)
          do i = 1, np(ispecies), 1
             if(.not.(selected(i).eq.0.and.draw_selected_only))then
                ! Get the coordinates of this particle in the view
                ! coordinate system
                !ip = project(pos(1:3,i),trans,width,height,fov_x,fov_y)
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
                ! Add its mass to the total
                if(ip(1).ge.0.and.ip(2).ge.0.and. &
                     ip(1).lt.width.and.ip(2).lt.height)then
                   mcount(offset+ip(1)+width*ip(2)) = &
                        mcount(offset+ip(1)+width*ip(2)) + &
                        mass(i)*sfac
                endif
             endif
          end do
!$OMP END DO
!$OMP END PARALLEL

          ! Add up the images from different threads
#ifdef _OPENMP
          do i = 1, nthreads-1, 1
             mcount(0:width*height-1) = mcount(0:width*height-1) + &
                  mcount(width*height*i:width*height*(i+1)-1)
          end do
#endif

          ! Add up the images
          do j = 0, height-1, 1
             do i = 0, width-1, 1
                if(mcount(i+width*j).gt.0)then
                   icol = min(255,max(0,floor(((mylog10(mcount(i+width*j))-&
                        lcmin(ispecies))/dlc(ispecies))*255.0)))
                   do k = 0, 2, 1
                      jcol = ichar(image(k+3*i+3*width*j)) + &
                           coltab(itab(ispecies))%data(k+1,icol)
                      image(k+3*i+3*width*j) = &
                           char(min(ichar(image(k+3*i+3*width*j))+jcol,255))
                   end do
                endif
             end do
          end do

       endif
       ! Next particle type
    end do
    
    return
  end subroutine density2d_make_image
    

  subroutine density2d_settings_open(mainwin,window,last_type_selected)
!
! Open configuration window for the density2d plotter
!
    implicit none
    ! Window handle
    type (gui_window) :: mainwin, window
    integer :: last_type_selected
    ! Top level vbox
    type (gui_box)    :: vbox, outer_hbox, settings_box, hbox
    type (gui_label)  :: label
    ! Number of particle types
    integer :: nspecies
    ! Loop index
    integer :: i
    ! Colour tables
    integer :: ntab
    character(len=coltab_name_length), dimension(ncoltabmax) :: ctab_names

    ! Type label
    character(len=500) :: str
    
    ! Set packing mode
    call gui_packing_mode(expand=.false., fill=.false., spacing=3, &
         position=gui_start)

    ! Get colour table names
    call colour_table_list(ntab, ctab_names)

    ! Create the window
    call gui_create_window(window,"2D Density plot settings",parent=mainwin,&
         resize=.false.)

    ! Top level vbox
    call gui_create_box(outer_hbox, window, gui_horizontal)
    call gui_create_box(vbox, outer_hbox, gui_vertical)

    ! Vbox with settings for this particle type
    call gui_create_notebook(notebook, vbox)

    call particle_store_contents(pdata, get_nspecies=nspecies)

    do i = 1, nspecies, 1
       write(str,'(i8)')i-1
       str = "Type "//trim(adjustl(str))
       call gui_create_box_in_notebook(settings_box, notebook, str)

       ! Box to select colour table
       call gui_create_box(hbox, settings_box, gui_horizontal, frame=.false.)
       call gui_create_label(label,hbox,"Colour table to use: ")
       call gui_create_combo_box(ctab_box(i), hbox, (/ "-" /))
    
       ! Range of values to show
       call gui_create_box(hbox, settings_box, gui_horizontal, frame=.false.)
       call gui_create_label(label,hbox, &
            "Mapping of surface density to colours: ")
       call gui_create_box(hbox, settings_box, gui_horizontal, frame=.false.)
    
       call gui_create_label(label,hbox,"Max. value to show ")
       call gui_packing_mode(expand=.false., fill=.true., spacing=3, &
            position=gui_end)
       call gui_create_slider(max_slider(i), hbox, range=(/0.0,200.0/), &
            step=1.0, orientation=gui_horizontal, min_size=250)
       call gui_packing_mode(expand=.false., fill=.false., spacing=3, &
            position=gui_start)

       call gui_create_box(hbox, settings_box, gui_horizontal, frame=.false.)
       call gui_packing_mode(position=gui_end)
       call gui_create_drawing_area(drawing_area(i),hbox,width=250,height=30,&
            double_buffered=.true.)
       call gui_packing_mode(position=gui_start)

       call gui_create_box(hbox, settings_box, gui_horizontal, frame=.false.)
       call gui_create_label(label,hbox,"Min. value to show ")
       call gui_packing_mode(expand=.false., fill=.true., spacing=3, &
            position=gui_end)
       call gui_create_slider(min_slider(i), hbox, range=(/0.0,200.0/), &
            step=1.0, orientation=gui_horizontal, min_size=250)
       call gui_packing_mode(expand=.false., fill=.false., spacing=3, &
            position=gui_start)
    end do

    ! Display the window
    call gui_show_window(window)
    if(last_type_selected.ge.1.and.last_type_selected.le.nspecies) &
         call gui_notebook_set_page(notebook, last_type_selected-1)
    call density2d_update_settings()
    do i = 1, nspecies, 1
       call redraw_colourbar(i)
    end do

    return
  end subroutine density2d_settings_open


  subroutine density2d_update_settings()
!
! Update the various widgets in the settings window
!
    implicit none
    integer :: nspecies, ntab
    character(len=maxlen), dimension(maxspecies) :: species_names
    character(len=coltab_name_length), dimension(ncoltabmax) :: ctab_names
    integer :: i


    call particle_store_contents(pdata, get_nspecies=nspecies, &
         get_species_names=species_names)
    call colour_table_list(ntab, ctab_names)

    ! Colour table box
    do i = 1, nspecies, 1
       call gui_combo_box_set_text(ctab_box(i), ctab_names(1:ntab))
       call gui_combo_box_set_index(ctab_box(i), itab(i))
    end do

    ! Min and max value sliders
    do i = 1, nspecies, 1
       call gui_slider_set_value(min_slider(i), min_scale(i))
       call gui_slider_set_value(max_slider(i), max_scale(i))
    end do

    return
  end subroutine density2d_update_settings


  logical function density2d_process_events()
!
! Process events for the configuration window
!
    implicit none

    integer :: i, nspecies
    logical :: res, need_update
    real    :: val

    res         = .false. ! Return true if screen needs to be redrawn
    need_update = .false. ! Set true if widget states need to be updated
    
    call particle_store_contents(pdata, get_nspecies=nspecies)

    ! Check if the colour table box has been changed
    do i = 1, nspecies, 1
       if(gui_combo_box_changed(ctab_box(i)))then
          call gui_combo_box_get_index(ctab_box(i), itab(i))
          res = .true.
          call redraw_colourbar(i)
       endif
    end do

    ! Check if the sliders have changed
    do i = 1, nspecies, 1
       if(gui_slider_changed(min_slider(i)))then
          call gui_slider_get_value(min_slider(i), min_scale(i))
          res = .true.
          call gui_slider_get_value(max_slider(i), val)
          if(val.le.min_scale(i))then
             max_scale(i) = min_scale(i)+1
             call gui_slider_set_value(max_slider(i), min_scale(i))
          endif
          call redraw_colourbar(i)
       endif
       if(gui_slider_changed(max_slider(i)))then
          call gui_slider_get_value(max_slider(i), max_scale(i))
          res = .true.
          call gui_slider_get_value(min_slider(i), val)
          if(val.ge.max_scale(i))then
             min_scale(i) = max_scale(i) - 1
             call gui_slider_set_value(min_slider(i), max_scale(i))
          endif
          call redraw_colourbar(i)
       endif
       if(gui_drawing_area_resized(drawing_area(i))) &
            call redraw_colourbar(i)
    end do

    density2d_process_events = res

    if(need_update)call density2d_update_settings()

    return
  end function density2d_process_events


  subroutine redraw_colourbar(ispecies)

    implicit none
    character(len=1), dimension(:), allocatable :: image
    integer                                     :: width, height
    integer                                     :: i, j, icol
    real                                        :: rmin, rmax
    integer                                     :: imin, imax
    integer                                     :: iheight
    integer                                     :: ispecies

    call gui_slider_get_value(min_slider(ispecies), rmin)
    call gui_slider_get_value(max_slider(ispecies), rmax)

    call gui_drawing_area_get_size(drawing_area(ispecies),width,height)

    ! Do nothing if size is zero
    if(height.le.1.or.height.le.1)return

    allocate(image(0:3*width*height-1))
    image = char(0)

    imin = min(width-1,max(0,int(width * rmin/200.0)))
    imax = min(width-1,max(0,int(width * rmax/200.0)))

    ! Draw the image in the image buffer
    iheight = height-1
    do i = 0, imin-1, 1
       do j = 0, height-1, 1
          image(3*i+3*j*width+0) = char(coltab(itab(ispecies))%data(1,0))
          image(3*i+3*j*width+1) = char(coltab(itab(ispecies))%data(2,0))
          image(3*i+3*j*width+2) = char(coltab(itab(ispecies))%data(3,0))
       end do
       image(3*i+3*iheight*width+0) = char(255)
       image(3*i+3*iheight*width+1) = char(0)
       image(3*i+3*iheight*width+2) = char(0)
    end do
    do i = imin, imax, 1
       icol = min(floor(real(i-imin)/real(imax-imin+1)*256.0),255)
       do j = 0, height-1, 1
          image(3*i+3*j*width+0) = char(coltab(itab(ispecies))%data(1,icol))
          image(3*i+3*j*width+1) = char(coltab(itab(ispecies))%data(2,icol))
          image(3*i+3*j*width+2) = char(coltab(itab(ispecies))%data(3,icol))
       end do
       iheight = height - floor(real(i-imin)/real(imax-imin+1)*height) - 1
       iheight = min(height-1,max(0,iheight))
       image(3*i+3*iheight*width+0) = char(255)
       image(3*i+3*iheight*width+1) = char(0)
       image(3*i+3*iheight*width+2) = char(0)
    end do
    iheight = 0
    do i = imax, width-1, 1
       do j = 0, height-1, 1
          image(3*i+3*j*width+0) = char(coltab(itab(ispecies))%data(1,255))
          image(3*i+3*j*width+1) = char(coltab(itab(ispecies))%data(2,255))
          image(3*i+3*j*width+2) = char(coltab(itab(ispecies))%data(3,255))
       end do
       image(3*i+3*iheight*width+0) = char(255)
       image(3*i+3*iheight*width+1) = char(0)
       image(3*i+3*iheight*width+2) = char(0)
    end do

    call gui_draw_image(drawing_area(ispecies),image,width,height,0,0)
    call gui_drawing_area_redraw(drawing_area(ispecies))

    deallocate(image)
    
    return
  end subroutine redraw_colourbar


  subroutine density2d_set_keys()
!
! Set keys in the settings file
!
    use key_file
    implicit none

    call set_key("Density2D", "Colour Tables",  itab)
    call set_key("Density2D", "Scale Min",  min_scale)
    call set_key("Density2D", "Scale Max",  max_scale)
    call set_key("Density2D", "Mass Scale", mass_scale)

    return
  end subroutine density2d_set_keys


  subroutine density2d_get_keys()
!
! Get keys from the settings file
!
    use key_file
    implicit none

    call get_key("Density2D", "Colour Tables",  itab)
    call get_key("Density2D", "Scale Min",      min_scale)
    call get_key("Density2D", "Scale Max",      max_scale)
    call get_key("Density2D", "Mass Scale",     mass_scale)

    return
  end subroutine density2d_get_keys

end module density2d
