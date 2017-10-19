module overlay

#include "../../config.h"

  use view_parameters
  use projection
  use stereo
  use string_module
  use selection
  use particle_store
  use catalogue_data
  use drawing
  use sampling

  implicit none
  private
  save

  ! Whether to draw selected particles
  logical, public :: overlay_show_selection = .true.

  ! Whether to draw concentric circles with radius labels
  logical, public :: overlay_show_scale     = .false.

  ! Whether to draw the crosshair etc
  logical, public :: overlay_show_crosshair = .true.
  logical, public :: overlay_show_coords    = .true.
  logical, public :: overlay_show_time      = .false.
  logical, public :: overlay_show_redshift  = .true.
  logical, public :: overlay_show_expansion = .false.
  logical, public :: overlay_show_fps       = .false.
  logical, public :: overlay_show_points    = .true.

  public :: overlay_add_to_image
  public :: overlay_set_keys
  public :: overlay_get_keys

contains

  subroutine overlay_add_to_image(width, height, trans, image, show_species)
!
! Add annotations to the supplied image
!    
    implicit none
    integer :: width, height
    character, dimension(0:3*width*height-1) :: image
    type(transform_type)                     :: trans
    logical, dimension(:) :: show_species

    call draw_cross()
    call draw_selected_particles()
    call draw_init_mem(image, width, height)
    call draw_labels()
    call draw_labelled_points()
    call draw_end()

    return

  contains

    subroutine draw_cross()
      
      implicit none
      integer :: i
      integer, dimension(2) :: ipos

      ! Put a cross at the selected point
      if(overlay_show_crosshair)then
         ipos = &
              project(real(trans%centre,pos_kind),trans,width,height,fov_x,fov_y)
         do i = -8, 8, 1
            if(ipos(1)+i.gt.0.and.ipos(1)+i.lt.width.and. &
                 ipos(2).gt.0.and.ipos(2).lt.height)then
               image(3*width*ipos(2)+3*(ipos(1)+i)+0: &
                    3*width*ipos(2)+3*(ipos(1)+i)+2) = char(255)
            endif
            if(ipos(1).gt.0.and.ipos(1).lt.width.and. &
                 ipos(2)+i.gt.0.and.ipos(2)+i.lt.height)then
               image(3*width*(ipos(2)+i)+3*ipos(1)+0: &
                    3*width*(ipos(2)+i)+3*ipos(1)+2) = char(255)
            endif
         end do
      endif
    
      return
    end subroutine draw_cross


    subroutine draw_labels()

      implicit none
      
      character(len=255) :: str, fmt, time_unit, xlab, ylab
      real :: time, redshift, expansion
      real :: cx, cy
      ! Points for drawing a circle
      integer, parameter                 :: ncirc = 50
      real,    parameter                 :: circle_size = 0.1
      real,    dimension(ncirc)          :: xcirc, ycirc
      real    :: xsize, ysize, radius
      integer :: icirc
      integer :: i
      ! FPS
      integer :: count, count_rate, count_max
      integer, save :: old_count = 0
      real :: fps
      real :: y
      real :: screen_width, screen_height

      ! Write the current coordinates at the top left
      if(overlay_show_coords)then
         str = "Centre: "//&
              trim(string(trans%centre(1),fmt='(1f10.3)'))//", "// &
              trim(string(trans%centre(2),fmt='(1f10.3)'))//", "// &
              trim(string(trans%centre(3),fmt='(1f10.3)'))
         call draw_text(str, 0.02, 0.05, &
              colour=1)
      endif

      ! Write time at top left
      call particle_store_get_time(pdata, time, redshift, expansion, &
           time_unit)
      y = 0.95
      if(overlay_show_time)then
         if(abs(time).lt.100.0.and.abs(time).gt.0.01)then
            fmt = '(1f10.3)'
         else
            fmt = '(1es10.3)'
         endif
         str = "Time: "//trim(string(time,fmt))
         if(len_trim(time_unit).gt.0)str = trim(str)//" "//trim(time_unit)
         call draw_text(str, 0.02, y, colour=1)
         y = y - 0.04
      endif
      if(overlay_show_redshift)then
         if(abs(redshift).lt.100.0.and.abs(redshift).gt.0.01)then
            fmt = '(1f10.3)'
         else
            fmt = '(1es10.3)'
         endif
         str = "Redshift: "//trim(string(redshift,fmt))
         call draw_text(str, 0.02, y, colour=1)
         y = y - 0.04
      endif
      if(overlay_show_expansion)then
         if(abs(expansion).lt.100.0.and.abs(expansion).gt.0.01)then
            fmt = '(1f10.3)'
         else
            fmt = '(1es10.3)'
         endif
         str = "Expansion: "//trim(string(expansion,fmt))
         call draw_text(str, 0.02, y, colour=1)
      endif

      if(overlay_show_scale)then
         ! Draw a set of circles to indicate scale
         do icirc = 1, 2, 1
            ! Radius in simulation coordinate system
            if (perspective_projection)then
               radius    = icirc*circle_size*(fov_x*(1.0+z_offset)) / &
                    trans%scale
            else
               radius    = icirc*circle_size*fov_x / &
                    trans%scale
            endif
            ! Size on the screen - 
            ! circle is at same z coord. as selected point
            xsize     = icirc*circle_size
            ysize     = xsize*(fov_x/fov_y)
            do i = 1, ncirc, 1
               xcirc(i) = (xsize*sin(2*3.14159/(ncirc-1)*(i-1))+0.5)
               ycirc(i) = (ysize*cos(2*3.14159/(ncirc-1)*(i-1))+0.5)
            end do
            call draw_line(xcirc,ycirc,colour=1)
            if(radius.gt.0.1.and.radius.lt.100.0)then
               write(str,'(1f20.3)')radius
            else
               write(str,'(1es20.2)')radius
            endif
            str = trim(adjustl(str))
            call draw_text(str, xcirc(1), ycirc(1)+0.02, &
                 colour=1)
         end do
      endif

      if(trans%axis_aligned.gt.0)then
         if(.not.perspective_projection)then
            ! Draw axes to indicate scale
            select case(trans%axis_aligned)
            case(1)
               xlab = "X"
               ylab = "Y"
               cx = trans%centre(1)
               cy = trans%centre(2)
            case(2)
               xlab = "Y"
               ylab = "Z"
               cx = trans%centre(2)
               cy = trans%centre(3)
            case(3)
               xlab = "X"
               ylab = "Z"
               cx = trans%centre(1)
               cy = trans%centre(3)
            end select
            screen_width  = fov_x / trans%scale
            screen_height = fov_y / trans%scale
            call draw_box(x=0.1, y=0.15, width=0.8, height=0.7, &
                 xmin=cx-0.4*screen_width, &
                 xmax=cx+0.4*screen_width, &
                 ymin=cy-0.35*screen_height, &
                 ymax=cy+0.35*screen_height, &
                 colour=1, xlabel=xlab, ylabel=ylab)
         endif
      endif

      if(overlay_show_fps)then
         ! Record time to find FPS
         call system_clock(count, count_rate, count_max)
         fps = real(count_rate)/real(count-old_count)
         old_count = count
         ! Add FPS to overlay
         write(str,'(1f5.1,"fps")')fps
         call draw_text(str, 0.95, 0.05)
      endif

    end subroutine draw_labels


    subroutine draw_selected_particles()
    
      implicit none
      integer(kind=index_kind), dimension(maxspecies) :: np(maxspecies)
      integer :: nspecies
      real(kind=pos_kind), dimension(:,:), pointer :: pos
      integer, dimension(:), pointer :: selected
      integer, dimension(3) :: icol
      integer :: isel, ispecies, i
      integer, dimension(2) :: ipos

      ! Draw any selected particles on top of the image
      if(overlay_show_selection)then
         call particle_store_contents(psample,get_nspecies=nspecies, &
              get_np=np)
         do isel = 0, nselmax-1, 1
            if(selection_is_empty(isel))cycle
            if(.not.display_selection(isel))cycle
            icol = selection_get_colour(isel)
            ! Loop over types of particle
            do ispecies = 1, nspecies, 1
               if(show_species(ispecies))then
                  ! Get a pointer to the positions for particles of this type
                  call particle_store_species(psample,ispecies,get_pos=pos, &
                       get_selected=selected)
                  ! Loop over particles of this type
                  do i = 1, np(ispecies), 1
                     if(.not.btest(selected(i),isel))cycle
                     ! Get the coordinates of this particle in the view
                     ! coordinate system
                     ipos = project(pos(1:3,i),trans,width,height,fov_x,fov_y)
                     if(ipos(1).ge.0.and.ipos(2).ge.0.and. &
                          ipos(1).lt.width.and.ipos(2).lt.height)then
                        image(0+3*ipos(1)+3*width*ipos(2)) = char(icol(1))
                        image(1+3*ipos(1)+3*width*ipos(2)) = char(icol(2))
                        image(2+3*ipos(1)+3*width*ipos(2)) = char(icol(3))
                     endif
                  end do
               endif
            end do
         end do
      endif
    
      return
    end subroutine draw_selected_particles


    subroutine draw_labelled_points()

      implicit none
      integer :: icat, ncat
      integer :: npoints
      real(kind=pos_kind),         dimension(:),   pointer :: point_radius
      character(len=label_length), dimension(:),   pointer :: label
      real(kind=pos_kind), dimension(:,:), pointer :: pos
      integer :: i

      if(.not.overlay_show_points)return

      ! Draw point catalogue(s) on the image
      ncat = catalogue_data_get_ncat()
      if(ncat.gt.0)then
         do icat = 1, ncat, 1
            if(catalogue_data_loaded(icat))then
               call catalogue_data_get_points(icat, npoints, pos, label, &
                    point_radius)
               if(associated(point_radius))then
                  do i = 1, npoints, 1
                     if(sum((pos(1:3,i)-current_pos)**2).lt.current_radius**2) &
                          call draw_marker(pos(1:3,i), point_radius(i))
                  end do
               else
                  do i = 1, npoints, 1
                     if(sum((pos(1:3,i)-current_pos)**2).lt.current_radius**2) &
                          call draw_marker(pos(1:3,i))
                  end do
               endif
               if(associated(label))then
                  do i = 1, npoints, 1
                     if(sum((pos(1:3,i)-current_pos)**2).lt.current_radius**2) &
                          call draw_label(pos(1:3,i), label(i))
                  end do
               endif
            endif
         end do
      endif
      
      return
    end subroutine draw_labelled_points


    subroutine draw_marker(pos, radius)
!
! Draw a cross with the specified physical size
!
      implicit none
      real(kind=pos_kind), dimension(3) :: pos
      real(kind=pos_kind), optional     :: radius
      integer, dimension(2)             :: ipos, jpos
      real                              :: r_proj
      integer                           :: i, irad
      ! Points for drawing a circle
      integer, parameter                 :: ncirc = 30
      real,    dimension(ncirc)          :: xcirc, ycirc
      real                               :: xsize, ysize
      ! Points for drawing a cross
      real, dimension(2)                 :: xcross, ycross

      ipos = project(pos(1:3),trans,width,height,fov_x,fov_y)
      if(present(radius))then
         ! Draw circles
         r_proj    = project_radius(pos,radius,trans,width,fov_x)
         xsize     = real(r_proj) / real(width)
         ysize     = xsize*(fov_x/fov_y)
         do i = 1, ncirc, 1
            xcirc(i) = (xsize*sin(2*3.14159/(ncirc-1)*(i-1)) + real(ipos(1))/real(width))
            ycirc(i) = 1.0 - (ysize*cos(2*3.14159/(ncirc-1)*(i-1)) + real(ipos(2))/real(height))
         end do
         call draw_line(xcirc,ycirc,colour=1)   
      else
         ! Draw fixed size (in pixels) crosses
         xsize = 4.0 / real(width)
         ysize = xsize*(fov_x/fov_y)
         xcross(1:2) = real(ipos(1))/real(width)
         ycross(1:2) = 1.0 - real(ipos(2))/real(height)
         xcross(1)   = xcross(1) - xsize
         xcross(2)   = xcross(2) + xsize
         ycross(1)   = ycross(1) - ysize
         ycross(2)   = ycross(2) + ysize
         call draw_line(xcross,ycross,colour=1)   
         xcross(1:2) = real(ipos(1))/real(width)
         ycross(1:2) = 1.0 - real(ipos(2))/real(height)
         xcross(1)   = xcross(1) + xsize
         xcross(2)   = xcross(2) - xsize
         ycross(1)   = ycross(1) - ysize
         ycross(2)   = ycross(2) + ysize
         call draw_line(xcross,ycross,colour=1)   
      endif

      return
    end subroutine draw_marker


    subroutine draw_label(pos, label)
!
! Draw a label at the specified location
!
      implicit none
      real(kind=pos_kind), dimension(3) :: pos
      character(len=*)                  :: label
      integer, dimension(2)             :: ipos

      ipos = project(pos(1:3),trans,width,height,fov_x,fov_y)
      if(ipos(1).ge.0.and.ipos(2).ge.0.and. &
           ipos(1).lt.width.and.ipos(2).lt.height)then
         call draw_text(label, real(ipos(1))/real(width), &
              1.0 - real(ipos(2))/real(height))
      endif

      return
    end subroutine draw_label


  end subroutine overlay_add_to_image


  subroutine overlay_set_keys()

    use key_file
    implicit none

    call set_key("Overlay","Show Selection",    overlay_show_selection)
    call set_key("Overlay","Show Radius Scale", overlay_show_scale)
    call set_key("Overlay","Show Crosshair",    overlay_show_crosshair)
    call set_key("Overlay","Show Position",     overlay_show_coords)
    call set_key("Overlay","Show Time",         overlay_show_time)
    call set_key("Overlay","Show Redshift",     overlay_show_redshift)
    call set_key("Overlay","Show Expansion",    overlay_show_expansion)
    call set_key("Overlay","Show FPS",          overlay_show_fps)
    call set_key("Overlay","Show Points",       overlay_show_points)

    return
  end subroutine overlay_set_keys


  subroutine overlay_get_keys()

    use key_file
    implicit none

    call get_key("Overlay","Show Selection",    overlay_show_selection)
    call get_key("Overlay","Show Radius Scale", overlay_show_scale)
    call get_key("Overlay","Show Crosshair",    overlay_show_crosshair)
    call get_key("Overlay","Show Position",     overlay_show_coords)
    call get_key("Overlay","Show Time",         overlay_show_time)
    call get_key("Overlay","Show Redshift",     overlay_show_redshift)
    call get_key("Overlay","Show Expansion",    overlay_show_expansion)
    call get_key("Overlay","Show FPS",          overlay_show_fps)
    call get_key("Overlay","Show Points",       overlay_show_points)

    return
  end subroutine overlay_get_keys

end module overlay
