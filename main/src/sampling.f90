module sampling
!
! Routines to select a sample of particles either in a specific region
! or over the whole volume.
!
  use particle_store
  use view_parameters
  use data_types
  use return_status
  use selection
  use graph

  implicit none
  private
  save

  public :: sample_region
  public :: sample_reset
  public :: sample_auto_resample
  public :: sample_set_keys
  public :: sample_get_keys

  ! Centre and radius from previous call
  real(kind=pos_kind), public :: current_pos(3) = &
       (/ 0.0_pos_kind, 0.0_pos_kind, 0.0_pos_kind /)
  real(kind=pos_kind), public :: current_radius = 1.0e20 

  ! Whether automatic resampling is active
  logical, public :: auto_resample = .false.

  ! Whether to do periodic wrap
  logical, public :: wrap_sample = .false.

contains

  type (result_type) function sample_region(whole_volume,keep_coords)
!
! Make a new particle sample that just contains particles near the
! selected point
!
    implicit none
    real(kind=pos_kind), dimension(3) :: pos
    real(kind=pos_kind)               :: radius
    logical, optional                 :: whole_volume
    logical, optional                 :: keep_coords
    logical :: whole
    type (result_type)                :: res

    whole = .false.
    if(present(whole_volume))whole = whole_volume

    ! Override these values if we want the full volume
    if(whole)then
       pos(1) = 0.0
       pos(2) = 0.0
       pos(3) = 0.0
       radius = 1.0e20
    else
       ! Determine the centre and radius of a sphere in the
       ! simulation coordinate system that contains the area we're 
       ! looking at
       pos    = view_transform%centre(1:3)
       radius = 1.0/view_transform%scale
    endif

    ! Keep the coordinates from the previous call if necessary
    if(present(keep_coords))then
       if(keep_coords)then
          pos    = current_pos
          radius = current_radius
       endif
    endif

    ! Throw away the old sample
    call particle_store_empty(psample)

    ! Make a new sample
    if(whole)then
       sample_region = particle_store_sample(pdata, psample, npmax_display, wrap=wrap_sample)
    else
       sample_region = particle_store_sample(pdata, psample, npmax_display, &
            pos, radius, wrap=wrap_sample)
    endif

    if(sample_region%success)then
       ! Store coordinates for next time
       current_pos    = pos
       current_radius = radius
       ! Locate any selected particles in the sample
       res = selection_apply_all(psample)
       ! Update the graph if necessary
       call graph_update()
       ! Update selection summary 
       call selection_update_summary()
    endif

    return
  end function sample_region


  subroutine sample_reset()
!
! Reset so that the next call to sample_region() will display the whole
! volume even if keep_coords=.true. This should be called when reading a
! new simulation, but NOT when loading another snapshot from the same run.
!
    implicit none

    current_pos    = (/ 0.0_pos_kind, 0.0_pos_kind, 0.0_pos_kind /)
    current_radius = 1.0e20_pos_kind

    return
  end subroutine sample_reset


  type (result_type) function sample_auto_resample(did_sample)
!
! Check if the visible region corresponds to the displayed sample.
! If not, resample the particles.
!
    implicit none
    logical :: need_resample
    real               :: radius, dpos
    real, dimension(3) :: pos
    real :: fsample
    type(result_type) :: res
    logical :: did_sample

    need_resample = .false.

    ! Centre of current view
    pos           = view_transform%centre(1:3)
    ! Get radius of a sphere enclosing the current view
    radius        = (0.5*max(fov_x,fov_y))/view_transform%scale
    ! Distance between centre of current view and centre of displayed sample
    dpos          = sqrt(sum((pos-current_pos)**2))

    ! Get the current sampling rate
    call particle_store_contents(psample, get_fsample=fsample)

    ! Check for zooming out - need to display a larger region if the displayed
    ! area is larger than the sample
    if(radius.gt.current_radius) need_resample = .true.

    ! Check for zooming in - resample if there are more particles
    ! to display and the displayed region is significantly smaller than
    ! the sample
    if(radius.lt.current_radius/ar_zoomfac**2.and.fsample.lt.1.0) &
         need_resample = .true.

    ! Check if the centre has been moved so that we can see outside
    ! the displayed region
    if(current_radius.lt.dpos+radius)need_resample = .true.
    
    ! Resample the particles if necessary
    if(need_resample)then
       call particle_store_empty(psample)
       current_radius = radius*ar_zoomfac
       current_pos    = pos
       did_sample     = .true.
       res = particle_store_sample(pdata, psample, &
            npmax_display, current_pos, current_radius, wrap=wrap_sample)
       if(res%success)then
          ! Locate any selected particles in the new sample
          res = selection_apply_all(psample)
          if(res%success)then
             ! Update the graph if necessary
             call graph_update()
             call selection_update_summary()
          endif
       endif
    else
       res%success = .true.
       did_sample = .false.
    endif

    sample_auto_resample = res
       
    return
  end function sample_auto_resample


  subroutine sample_set_keys()

    use key_file
    implicit none
    real, dimension(3) :: pos
    real               :: radius
    
    radius = current_radius
    pos    = current_pos
    call set_key("Sampling", "Auto Resample", auto_resample)
    call set_key("Sampling", "Position",      pos)
    call set_key("Sampling", "Radius",        radius)
    call set_key("Sampling", "Wrap",          wrap_sample)

    return
  end subroutine sample_set_keys


  subroutine sample_get_keys()

    use key_file
    implicit none
    real, dimension(3) :: pos
    real               :: radius

    call get_key("Sampling", "Auto Resample", auto_resample)
    call get_key("Sampling", "Position",      pos)
    call get_key("Sampling", "Radius",        radius)
    call get_key("Sampling", "Wrap",          wrap_sample)
    current_radius = radius
    current_pos    = pos

    return
  end subroutine sample_get_keys

end module sampling
