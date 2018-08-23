module fancyplot
!
! Module to generate plot showing surface density of particles
! on the screen. Now with fancy smoothing algorithm!
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
  use string_module
  use projection
  use selection, only: draw_selected_only
  use math

  implicit none
  private
  save

  public :: fancyplot_init
  public :: fancyplot_make_image
  public :: fancyplot_settings_open
  public :: fancyplot_process_events
  public :: fancyplot_update_settings
  public :: fancyplot_set_keys
  public :: fancyplot_get_keys

  ! Colour table to use for each type of particle
  integer, dimension(maxspecies) :: itab

  ! Scale factor to apply for each type of particle
  real, dimension(maxspecies)    :: min_scale, max_scale

  ! Particle type to adjust settings for
  integer :: selected_species = 1

  type (gui_notebook) :: notebook

  ! Widgets for the settings window
  type (gui_combo_box), dimension(maxspecies)    :: ctab_box
  type (gui_slider), dimension(maxspecies)       :: min_slider, max_slider
  type (gui_entrybox), dimension(maxspecies)     :: scale_box, maxsmooth_box, &
                                                    hsml_box
  type (gui_radio_button), dimension(maxspecies) :: adaptive_button, &
                                                    fixed_button
  type (gui_button), dimension(maxspecies)       :: apply_all_button
  type (gui_entrybox), dimension(maxspecies)     :: maxphyssmooth_box
  type (gui_drawing_area), dimension(maxspecies) :: drawing_area

  ! 'Splats' to use for plotting particles
  integer, parameter :: nsplatmax = 256
  type splat_type
     integer :: is
     real, pointer, dimension(:,:) :: data
  end type splat_type
  type (splat_type), dimension(0:nsplatmax) :: splat

  ! Maximum smoothing length in pixels
  integer, dimension(maxspecies) :: maxsmooth     = 64
  real,    dimension(maxspecies) :: maxphyssmooth = 1.0e20

  ! Factor to scale smoothing lengths by
  real, dimension(maxspecies)    :: scale_smooth = 1.0

  ! Scale factor for particle masses
  real, dimension(maxspecies) :: mass_scale

  ! Fixed smoothing lengths
  logical, dimension(maxspecies) :: use_fixed_hsml = .false.
  real, dimension(maxspecies)    :: fixed_hsml     = 0.001

  logical :: first_call = .true.

contains

  subroutine fancyplot_init(new_snapshot)
!
! Set default options for the plot
!
    implicit none
    integer :: i
    logical :: new_snapshot
    integer                         :: nspecies
    integer(kind=index_kind), dimension(maxspecies)  :: np
    real(kind=r_prop_kind), dimension(:), pointer :: mass
    real :: mtot
    logical, save :: first_init = .true.

    if(.not.new_snapshot)then
       ! Default colour tables 
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
    endif

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

    return
  end subroutine fancyplot_init


  subroutine fancyplot_make_image(width, height, trans, image, show_species)
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
    integer :: ispecies, i, j, k, l
    ! Particle data
    integer                        :: nspecies
    integer(kind=index_kind), dimension(maxspecies) :: np
    real(kind=pos_kind), dimension(:,:), pointer :: pos
    ! Transformed z coord of a particle
    real :: z_trans
    real(kind=pos_kind), dimension(3) :: pos_trans
    ! Projected coordinates
    integer, dimension(2) :: ip
    ! Count of particles per pixel
    real, dimension(:), allocatable, save :: mcount
    integer :: icol, jcol
    integer :: is
    real :: mass_factor
    ! Smoothing lengths
    real, dimension(:), pointer :: hsml
    real :: clipfac, hnorm, hs
    ! Fade out at near clipping plane
    real :: nearclip_fade, dzfade, farclip_fade, invdzfade
    ! Pointer to the mass array
    real(kind=r_prop_kind), dimension(:), pointer :: mass
    real :: mp
    ! Range in log10(count) to map onto the colour table
    real, dimension(maxspecies) :: lcmin, lcmax, dlc
    real :: r
    ! Number of openmp threads
    integer :: nthreads, ithread, offset
#ifdef _OPENMP
    integer, external :: omp_get_max_threads, omp_get_thread_num
#endif
    ! Selected particle flags
    integer, dimension(:), pointer :: selected

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
                splat(i)%data(j,k) = kernel(2*r)
             end do
          end do
          ! Normalise
          splat(i)%data = splat(i)%data / sum(splat(i)%data)
       end do
       first_call = .false.
    endif

    ! Decide range of log10(m*mass_scale) to display
    lcmin = 1.0 + 5.0*((min_scale-50.0)/50.0)
    lcmax = 1.0 + 5.0*((max_scale-50.0)/50.0)
    dlc   = lcmax - lcmin

    ! Clipping planes
    dzfade        = 0.1
    nearclip_fade = nearclip + dzfade
    farclip_fade  = farclip  - dzfade
    invdzfade     = 1.0/dzfade

    ! Fill in the background
    do i = 0, (width*height*3)-1, 3
       image(i:i+2) = char(0)
    end do

    ! Get number of particles in the sample to show
    call particle_store_contents(psample,get_nspecies=nspecies, get_np=np)

    ! Loop over types of particle
    do ispecies = 1, nspecies, 1
       if(show_species(ispecies))then

          ! Get a pointer to the positions for particles of this type
          if(.not.use_fixed_hsml(ispecies))then
             call particle_store_species(psample,ispecies,get_pos=pos, &
                  get_hsml=hsml)
          endif
          call particle_store_species(psample,ispecies,get_pos=pos, &
               get_mass=mass, get_selected=selected)

          ! Initialise mcount, which will accumulate density in each pixel
          mcount(:) = 0.0
          mass_factor = mass_scale(ispecies)/fdisplay * &
               (trans%scale/scalefac)**2.0 / ((pixelsize_x*pixelsize_y)/4.3e-6) ! Constant to preserve scaling for default window size

          ! Use OpenMP to parallelise image generation
!$OMP PARALLEL DEFAULT(SHARED) &
!$OMP& PRIVATE(i,j,z_trans,clipfac,hnorm,ip,is,l,k,hs,mp,ithread,offset,pos_trans)
#ifdef _OPENMP
          ithread = omp_get_thread_num()
#else
          ithread = 0
#endif          
          offset = ithread * width * height
!$OMP DO SCHEDULE(DYNAMIC,1000)
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
             !
             ! End of inline project()
             !
             ! Get scaled mass of this particle, taking into account the
             ! sampling rate
             mp = mass(i)*mass_factor

             ! Check if particle is in front of the view point
             if(z_trans.gt.nearclip.and.z_trans.lt.farclip)then
                
                ! Fade out as the particle approaches clipping planes
                if(z_trans.lt.nearclip_fade)then
                   clipfac = 1.0 - abs(z_trans-nearclip_fade)*invdzfade
                else if(z_trans.gt.farclip-dzfade)then
                   clipfac = 1.0 - abs(z_trans-farclip_fade)*invdzfade
                else
                   clipfac = 1.0
                endif

                ! Get smoothing length in pixels
                if(use_fixed_hsml(ispecies))then
                   is = ((fixed_hsml(ispecies)*scale_smooth(ispecies)* &
                        trans%scale)/ &
                        (1.0+z_trans))/fov_x*width
                else
                   hs = min(hsml(i),maxphyssmooth(ispecies))
                   is = ((hs*scale_smooth(ispecies)*trans%scale)/ &
                        (1.0+z_trans))/fov_x*width
                endif

                ! Normalisation for 'splat' to add to the image
                hnorm = clipfac*mp
                
                ! Add this particle to the image
                is = max(0,min(is,maxsmooth(ispecies)))
                do l = max(0,ip(2)-is), min(height-1,ip(2)+is), 1
                   do k = max(0,ip(1)-is), min(width-1,ip(1)+is), 1
                      mcount(k+width*l+offset) = mcount(k+width*l+offset) + &
                           splat(is)%data(k-ip(1),l-ip(2))*hnorm
                   end do
                end do
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

          ! Add up the images of different particle types
          do j = 0, height-1, 1
             do i = 0, width-1, 1
                if(mcount(i+width*j).gt.0)then
                   icol = min(255,max(0,floor(((mylog10(mcount(i+width*j))-&
                        lcmin(ispecies))/dlc(ispecies))*255.0)))
                else
                   icol = 0
                endif
                do k = 0, 2, 1
                   jcol = ichar(image(k+3*i+3*width*j)) + &
                        coltab(itab(ispecies))%data(k+1,icol)
                   image(k+3*i+3*width*j) = char(min(jcol,255))
                end do
             end do
          end do

       endif
       ! Next particle type
    end do

    return
  end subroutine fancyplot_make_image
    

  subroutine fancyplot_settings_open(mainwin,window,last_type_selected)
!
! Open configuration window for the fancyplotter
!
    implicit none
    ! Window handle
    type (gui_window) :: mainwin,window
    integer :: last_type_selected
    ! Top level vbox
    type (gui_box)    :: vbox, hbox, inner_vbox, outer_hbox
    type (gui_label)  :: label
    ! Number of particle types
    integer :: nspecies
    ! Loop index
    integer :: i
    character(len=50) :: str
    ! Colour tables
    integer :: ntab
    character(len=coltab_name_length), dimension(ncoltabmax) :: ctab_names
    character(len=maxlen), dimension(maxspecies) :: species_names

    ! Get particle type names
    call particle_store_contents(pdata,get_nspecies=nspecies, &
         get_species_names=species_names)

    ! Set packing mode
    call gui_packing_mode(expand=.false., fill=.false., spacing=3, &
         position=gui_start)

    ! Get colour table names
    call colour_table_list(ntab, ctab_names)

    ! Create the window
    call gui_create_window(window,"Smoothed plot settings",parent=mainwin, &
         resize=.false.)
    call gui_create_box(outer_hbox, window, gui_horizontal)
    call gui_create_notebook(notebook, outer_hbox)

    do i = 1, nspecies, 1
       write(str,'(i8)')i-1
       str = "Type "//trim(adjustl(str))
       call gui_create_box_in_notebook(vbox, notebook, str)

       ! Settings for this particle type
       call gui_create_box(inner_vbox, vbox, gui_vertical, frame=.true.)
       call gui_create_box(hbox, inner_vbox, gui_horizontal)
       call gui_create_label(label, hbox, "Colour table to use: ")
       call gui_create_combo_box(ctab_box(i), hbox, ctab_names(1:nspecies))

       call gui_create_box(hbox, inner_vbox, gui_horizontal)
       call gui_create_label(label, hbox, "Max. value to show: ")
       call gui_packing_mode(expand=.false., fill=.false.,position=gui_end)
       call gui_create_slider(max_slider(i), hbox, &
            range=(/0.0,200.0/), step=1.0, orientation=gui_horizontal, &
            min_size=250)
       call gui_packing_mode(expand=.false., fill=.false.,position=gui_start)

       call gui_create_box(hbox, inner_vbox, gui_horizontal, frame=.false.)
       call gui_packing_mode(position=gui_end)
       call gui_create_drawing_area(drawing_area(i),hbox,width=250,height=30, &
            double_buffered=.true.)
       call gui_packing_mode(position=gui_start)

       call gui_create_box(hbox, inner_vbox, gui_horizontal)
       call gui_create_label(label, hbox, "Min. value to show: ")
       call gui_packing_mode(expand=.false., fill=.false.,position=gui_end)
       call gui_create_slider(min_slider(i), hbox, &
            range=(/0.0,200.0/), step=1.0, orientation=gui_horizontal, &
            min_size=250)
       call gui_packing_mode(expand=.false., fill=.false.,position=gui_start)

       ! Smoothing settings
       call gui_create_box(inner_vbox, vbox, gui_vertical, frame=.true., &
            label="Smoothing parameters", expander=.true.)
       call gui_create_box(hbox, inner_vbox, gui_horizontal)
       call gui_create_label(label,hbox,&
            "Max. smoothing length in pixels (1-"// &
            trim(adjustl(string(nsplatmax)))//"): ")
       call gui_packing_mode(expand=.false., fill=.false.,position=gui_end)
       call gui_create_entrybox(maxsmooth_box(i), hbox, 5)
       call gui_packing_mode(expand=.false., fill=.false.,position=gui_start)

       call gui_create_box(hbox, inner_vbox, gui_horizontal)
       call gui_create_label(label,hbox, "Max. physical smoothing length:")
       call gui_packing_mode(expand=.false., fill=.false.,position=gui_end)
       call gui_create_entrybox(maxphyssmooth_box(i), hbox, 12)
       call gui_packing_mode(expand=.false., fill=.false.,position=gui_start)

       call gui_create_box(hbox, inner_vbox, gui_horizontal)
       call gui_create_radio_button(adaptive_button(i), hbox, &
            "Adaptive smoothing lengths")
       call gui_packing_mode(expand=.false., fill=.false.,position=gui_end)
       call gui_create_entrybox(scale_box(i), hbox, 5)
       call gui_create_label(label, hbox, "Scale by: ")
       call gui_packing_mode(expand=.false., fill=.false.,position=gui_start)
       call gui_create_box(hbox, inner_vbox, gui_horizontal)
       call gui_create_radio_button(fixed_button(i), hbox, &
            "Fixed smoothing lengths", previous=adaptive_button(i))
       call gui_packing_mode(expand=.false., fill=.false.,position=gui_end)
       call gui_create_entrybox(hsml_box(i), hbox, 12)
       call gui_create_label(label, hbox, "Size: ")
       call gui_packing_mode(expand=.false., fill=.false.,position=gui_start)

       call gui_create_box(hbox, inner_vbox, gui_horizontal)
       call gui_packing_mode(expand=.false., fill=.false.,position=gui_end)
       call gui_create_button(apply_all_button(i), hbox, &
            "Apply smoothing settings to all particle types")
       call gui_packing_mode(expand=.false., fill=.false.,position=gui_start)

    end do

    call gui_show_window(window)

    if(last_type_selected.ge.1.and.last_type_selected.le.nspecies) &
         call gui_notebook_set_page(notebook, last_type_selected-1)

    call fancyplot_update_settings()
    do i = 1, nspecies, 1
       call redraw_colourbar(i)
    end do

    return
  end subroutine fancyplot_settings_open


  logical function fancyplot_process_events()
!
! Process events for the configuration window
!
    implicit none
    integer :: i, nspecies, ispecies, itmp
    logical :: res, need_update, need_redraw
    real    :: val
    character(len=100) :: str
    integer            :: ios
    real               :: rtmp

    call particle_store_contents(pdata,get_nspecies=nspecies)

    ispecies    = selected_species
    res         = .false. ! Return true if screen needs to be redrawn
    need_update = .false. ! Set true if widget states need to be updated

    do i = 1, nspecies, 1

       need_redraw = .false. ! Set if colour bar needs to be redrawn

       ! Check if the colour table box has been changed
       if(gui_combo_box_changed(ctab_box(i)))then
          call gui_combo_box_get_index(ctab_box(i), itab(i))
          res = .true.
          need_redraw = .true.
       endif

       ! Check if the sliders have changed
       if(gui_slider_changed(min_slider(i)))then
          call gui_slider_get_value(min_slider(i), min_scale(i))
          res = .true.
          call gui_slider_get_value(max_slider(i), val)
          if(val.le.min_scale(i))then
             max_scale(i) = min_scale(i) + 1
             call gui_slider_set_value(max_slider(i), min_scale(i))
          endif
          need_redraw = .true.
       endif
       if(gui_slider_changed(max_slider(i)))then
          call gui_slider_get_value(max_slider(i), max_scale(i))
          res = .true.
          call gui_slider_get_value(min_slider(i), val)
          if(val.ge.max_scale(i))then
             min_scale(i) = max_scale(i) - 1
             call gui_slider_set_value(min_slider(i), max_scale(i))
          endif
          need_redraw = .true.
       endif

       ! Check if text entry boxes have been altered
       if(gui_entrybox_changed(scale_box(i)))then
          call gui_entrybox_get_text(scale_box(i), str)
          read(str,*,iostat=ios)rtmp
          if(ios.eq.0)then
             if(rtmp.gt.0.0.and.rtmp.le.10.0)then
                scale_smooth(i) = rtmp
                res = .true.
             endif
          endif
          need_update = .true.
       endif

       ! Check if text entry boxes have been altered
       if(gui_entrybox_changed(maxsmooth_box(i)))then
          call gui_entrybox_get_text(maxsmooth_box(i), str)
          read(str,*,iostat=ios)itmp
          if(ios.eq.0)then
             if(itmp.gt.0.and.itmp.le.nsplatmax)then
                maxsmooth(i) = itmp
                res = .true.
             endif
          endif
          need_update = .true.
       endif

       ! Check if text entry boxes have been altered
       if(gui_entrybox_changed(maxphyssmooth_box(i)))then
          call gui_entrybox_get_text(maxphyssmooth_box(i), str)
          read(str,*,iostat=ios)rtmp
          if(ios.eq.0)then
             if(rtmp.gt.0.0)then
                maxphyssmooth(i) = rtmp
                res = .true.
             endif
          endif
          need_update = .true.
       endif

       ! Check if text entry boxes have been altered
       if(gui_entrybox_changed(hsml_box(i)))then
          call gui_entrybox_get_text(hsml_box(i), str)
          read(str,*,iostat=ios)rtmp
          if(ios.eq.0)then
             fixed_hsml(i) = rtmp
             res = .true.
          endif
          need_update = .true.
       endif

       ! Check if radio buttons have been changed
       if(gui_radio_button_changed(adaptive_button(i)).or. &
            gui_radio_button_changed(fixed_button(i)))then
          call gui_radio_button_get_state(fixed_button(i), &
               use_fixed_hsml(i))
          res = .true.
          need_update = .true.
       endif

       ! Apply smoothing settings to all particles if button is pressed
       if(gui_button_clicked(apply_all_button(i)))then
          maxsmooth(1:maxspecies)      = maxsmooth(i)
          maxphyssmooth(1:maxspecies)  = maxphyssmooth(i)
          scale_smooth(1:maxspecies)   = scale_smooth(i)
          fixed_hsml(1:maxspecies)     = fixed_hsml(i)
          use_fixed_hsml(1:maxspecies) = use_fixed_hsml(i)
          need_update = .true.
          res         = .true.
       endif

       if(gui_drawing_area_resized(drawing_area(i)))need_redraw=.true.

       if(need_redraw)call redraw_colourbar(i)

    end do

    if(need_update)call fancyplot_update_settings()

    fancyplot_process_events = res

    return
  end function fancyplot_process_events



  subroutine fancyplot_update_settings()
!
! Update the various widgets in the settings window
!
    implicit none
    integer :: nspecies, ntab
    character(len=maxlen), dimension(maxspecies) :: species_names
    character(len=coltab_name_length), dimension(ncoltabmax) :: ctab_names
    integer :: i, ispecies


    ispecies = selected_species

    ! Particle type box
    call particle_store_contents(pdata, get_nspecies=nspecies, &
         get_species_names=species_names)
    if(ispecies.gt.nspecies)ispecies = 1

    do i = 1, nspecies, 1

       ! Colour table box
       call colour_table_list(ntab, ctab_names)
       call gui_combo_box_set_text(ctab_box(i), ctab_names(1:ntab))
       call gui_combo_box_set_index(ctab_box(i), itab(i))

       ! Min and max value sliders
       call gui_slider_set_value(min_slider(i), min_scale(i))
       call gui_slider_set_value(max_slider(i), max_scale(i))

       ! Text entry boxes
       call gui_entrybox_set_text(maxsmooth_box(i),&
            trim(string(maxsmooth(i))))
       call gui_entrybox_set_text(maxphyssmooth_box(i),&
            trim(string(maxphyssmooth(i),'(1es12.2)')))
       call gui_entrybox_set_text(scale_box(i), &
            trim(string(scale_smooth(i),fmt='(1f5.2)')))

       ! Radio buttons
       if(use_fixed_hsml(i))then
          call gui_radio_button_set_state(fixed_button(i), .true.)
       else
          call gui_radio_button_set_state(adaptive_button(i), .true.)
       endif

       ! Fixed smoothing length
       call gui_entrybox_set_text(hsml_box(i), &
            trim(string(fixed_hsml(i),'(1es12.2)')))

    end do

    return
  end subroutine fancyplot_update_settings



  real function kernel(r)
!
! Smoothing kernel used to make the plot.
! This should be a function which drops to zero at r=2.0
! and is scaled such that the smoothing length is 1.0
!
    implicit none
    real, intent(in)   :: r
    integer, parameter :: ikernel = 2
    real :: w

    select case(ikernel)
    case(1)
       ! Gaussian truncated at r=2.0
       if(r.lt.0.001)then
          w = 1.0
       else if(r.lt.2.0)then
          w = exp(-(r**2))
       else
          w = 0.0
       endif
    case(2)
       ! SPH spline kernel
       if(r.lt.1.0)then
          w = 1.0 - (3.0/2.0)*(r**2)+(3.0/4.0)*(r**3)
       else if(r.lt.2.0) then
          w = 0.25 * (2.0-r)**3
       else
          w = 0.0
       endif
    case default
       call terminate('Invalid value of ikernel')
    end select

    kernel = w
    return
  end function kernel



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

    if(width.le.1.or.height.le.1)return

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


  subroutine fancyplot_set_keys()

    use key_file
    implicit none

    call set_key("Smoothed Density", "Colour Table", itab)
    call set_key("Smoothed Density", "Scale Min",      min_scale)
    call set_key("Smoothed Density", "Scale Max",      max_scale)
    call set_key("Smoothed Density", "Mass Scale",     mass_scale)
    call set_key("Smoothed Density", "Maximum Smoothing (Pixels)",   maxsmooth)
    call set_key("Smoothed Density", "Maximum Smoothing (Physical)", maxphyssmooth)
    call set_key("Smoothed Density", "Smoothing Scale Factor",       scale_smooth)
    call set_key("Smoothed Density", "Fix Smoothing Length",         use_fixed_hsml)
    call set_key("Smoothed Density", "Smoothing Length",             fixed_hsml)

    return
  end subroutine fancyplot_set_keys


  subroutine fancyplot_get_keys()

    use key_file
    implicit none

    call get_key("Smoothed Density", "Colour Table", itab)
    call get_key("Smoothed Density", "Scale Min",      min_scale)
    call get_key("Smoothed Density", "Scale Max",      max_scale)
    call get_key("Smoothed Density", "Mass Scale",     mass_scale)
    call get_key("Smoothed Density", "Maximum Smoothing (Pixels)",   maxsmooth)
    call get_key("Smoothed Density", "Maximum Smoothing (Physical)", maxphyssmooth)
    call get_key("Smoothed Density", "Smoothing Scale Factor",       scale_smooth)
    call get_key("Smoothed Density", "Fix Smoothing Length",         use_fixed_hsml)
    call get_key("Smoothed Density", "Smoothing Length",             fixed_hsml)

    return
  end subroutine fancyplot_get_keys

end module fancyplot
