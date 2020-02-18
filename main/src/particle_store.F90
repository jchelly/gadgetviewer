module particle_store
!
! Module to store a set of particles from a simulation,
! including an arbitrary set of properties which may
! be of real or integer type.
!
  use data_types
  use return_status
  use progress_bar
  use octreemod
  use string_module
  use data_array
  use f90_util

  implicit none
  private
  save

  !
  ! Public components of this module
  ! --------------------------------
  !

  ! Routines to fill/empty the particle store
  public :: particle_store_init
  public :: particle_store_new_species
  public :: particle_store_new_property
  public :: particle_store_add_data
  public :: particle_store_empty

  ! Random sampling
  public :: particle_store_sample

  ! Store snapnum and time variable
  public :: particle_store_set_snapnum
  public :: particle_store_set_time

  ! Store size of IDs in the snapshots
  public :: particle_store_set_idsize
  public :: particle_store_get_idsize

  ! Store size of simulation box (zero if not periodic)
  public :: particle_store_set_boxsize
  public :: particle_store_get_boxsize

  ! Routine to deallocate any partially read properties
  ! (used for recovery after errors)
  public :: particle_store_cleanup

  ! Routine to check that all data has been loaded
  public :: particle_store_verify

  ! Routines to query the store
  public :: particle_store_contents       ! General info about loaded dataset
  public :: particle_store_species        ! Get data for one species
  public :: particle_store_property       ! Get data for one property
  public :: particle_store_loaded         ! Determine if data is loaded
  public :: particle_store_get_time       ! Return the time of this snapshot

  ! Build octrees used for fast random sampling
  public :: particle_store_build_trees
  
  ! Get property values at some point in space
  public :: particle_store_evaluate_property

  ! Type to store a set of particle data
  public :: pdata_type

  ! Maximum length of a species/property name
  integer, parameter, public :: maxlen = 500
  ! Maximum no. of species
  integer, parameter, public :: maxspecies = 6
  ! Maximum number of additional properties
  integer, parameter, public :: maxprops = 20

  !
  ! -------------- End of public part of module -------------
  !

  ! Type to describe a particle property
  type propertytype
     private
     character(len=maxlen)     :: name
     character(len=maxlen)     :: type
     integer(kind=index_kind)   :: nloaded
     integer(kind=i_prop_kind), dimension(:), pointer :: idata
     real   (kind=r_prop_kind), dimension(:), pointer :: rdata
     real                      :: val_min, val_max
     type (data_array_type)    :: data
  end type propertytype

  ! Information about one species
  type speciestype
     private
     ! Name and number of particles
     character(len=maxlen) :: name
     integer(kind=index_kind) :: np
     ! Particle positions, IDs and smoothing lengths
     real(kind=pos_kind),   pointer, dimension(:,:) :: pos
     real(kind=vel_kind),   pointer, dimension(:,:) :: vel
     real,                  pointer, dimension(:)   :: hsml
     ! Storage for positions and velocities while data is being loaded
     type (data_array_type)    :: posdata
     type (data_array_type)    :: veldata
     ! Whether each particle is selected
     integer, pointer, dimension(:) :: selected
     ! Number of positions, masses etc loaded so far
     integer(kind=index_kind) :: npos, nvel
     ! Additional particle properties
     integer               :: nprops
     type (propertytype), dimension(maxprops) :: property
     ! Fraction of particles included, if this is a sample
     real :: fpart
     ! Octree used for sampling
     type(octree_type) :: octree
     ! If this is a sample, the index of the particle in the full
     ! dataset
     integer(kind=index_kind), pointer, dimension(:) :: idx
     ! Indexes of the mass and id properties
     integer                   :: id_index
     integer                   :: mass_index
  end type speciestype

  ! Type to store a complete set of particle data
  type pdata_type
     private
     integer :: isnap
     integer :: nspecies
     type(speciestype), dimension(maxspecies) :: species
     logical :: loaded
     logical :: is_sample
     real    :: fsample
     logical :: trees_built
     real    :: time, redshift, expansion
     character(len=maxlen) :: time_unit
     integer :: idsize
     real    :: boxsize
  end type pdata_type

  ! These need to be public for pdata_type to be public.
  ! They should never be used directly.
  public :: speciestype, propertytype

  ! These variables will contain the actual particle data
  type (pdata_type), public :: pdata
  type (pdata_type), public :: psample

contains

!
! Return true if any particles are loaded
!
  logical function particle_store_loaded(pdata)

    implicit none
    type (pdata_type) :: pdata

    particle_store_loaded = pdata%loaded

    return
  end function particle_store_loaded

!
! Initialise the particle store to an empty state.
! Nullify all the pointers just in case...
!
  subroutine particle_store_init(pdata)
    
    implicit none
    integer :: i, j
    type (pdata_type) :: pdata

    pdata%nspecies = 0
    pdata%loaded   = .false.
    pdata%isnap    = -1

    do i = 1, maxspecies, 1
       pdata%species(i)%np     = 0
       pdata%species(i)%nprops = 0
       pdata%species(i)%name   = "none"
       nullify(pdata%species(i)%pos)
       nullify(pdata%species(i)%vel)
       nullify(pdata%species(i)%hsml)
       nullify(pdata%species(i)%selected)
       do j = 1, maxprops, 1
          pdata%species(i)%property(j)%name = "none"
          pdata%species(i)%property(j)%type = "none"
          pdata%species(i)%property(j)%val_min = 1.0
          pdata%species(i)%property(j)%val_max = 0.0
          nullify(pdata%species(i)%property(j)%idata)
          nullify(pdata%species(i)%property(j)%rdata)
          call data_array_init(pdata%species(i)%property(j)%data)
       end do
       call data_array_init(pdata%species(i)%posdata)
       call data_array_init(pdata%species(i)%veldata)
    end do

    pdata%is_sample   = .false.
    pdata%trees_built = .false.
    pdata%time        = 0.0
    pdata%redshift    = 0.0
    pdata%expansion   = 0.0
    pdata%idsize      = 4
    pdata%time_unit   = ""
    pdata%boxsize     = 0.0

    return
  end subroutine particle_store_init

!
! This adds a new type of particle
!
  type (result_type) function particle_store_new_species(pdata, name, np)

    implicit none
    ! Parameters
    character(len=*), intent(in) :: name
    integer(kind=index_kind), intent(in), optional :: np
    type (pdata_type) :: pdata
    ! Internal
    integer :: i, istat
    
    ! Return status - assume failure until we get to the end of the 
    ! routine
    particle_store_new_species%success = .false.
    particle_store_new_species%string  = &
         "Unable to allocate memory for particle type "//trim(name)

    ! Get index for this species
    pdata%nspecies = pdata%nspecies + 1
    i = pdata%nspecies
    if(i.gt.maxspecies)call terminate('Increase MAXSPECIES!')

    ! Allocate storage, if we know how many particles there are
    if(present(np))then
       call data_array_init(pdata%species(i)%posdata, np)
#ifdef READ_VEL
       call data_array_init(pdata%species(i)%veldata, np)
#endif
    else
       call data_array_init(pdata%species(i)%posdata)
#ifdef READ_VEL
       call data_array_init(pdata%species(i)%veldata)
#endif
    endif

    if(associated(pdata%species(i)%pos).or.&
         associated(pdata%species(i)%vel))then
       write(0,*)"Memory leak in particle_store_new_species()!"
    endif

    nullify(pdata%species(i)%pos)
    nullify(pdata%species(i)%vel)

    ! Store the species name and particle number
    if(present(np))then
       pdata%species(i)%np          = np
    else
       pdata%species(i)%np          = -1 ! Not known yet
    endif
    pdata%species(i)%name        = trim(adjustl(name))

    ! No data has been loaded yet for this species
    pdata%species(i)%npos = 0
    pdata%species(i)%nvel = 0

    ! Don't have ID or mass arrays yet
    pdata%species(i)%id_index = -1
    pdata%species(i)%mass_index = -1

    ! Set return status
    particle_store_new_species%success = .true.

    return
  end function particle_store_new_species

!
! This adds a new particle property to the named
! species
!
  type (result_type) function particle_store_new_property(pdata,species_name, &
       prop_name, prop_type, is_id, is_mass)

    implicit none
    ! Parameters
    character(len=*) :: species_name, prop_name, prop_type
    type (pdata_type) :: pdata
    logical, intent(in), optional :: is_id, is_mass
    ! Internal
    integer :: i, j, istat
    integer :: ispecies

    ! Return status - assume failure until we get to the end of the 
    ! routine
    particle_store_new_property%success = .false.
    particle_store_new_property%string  = &
         "Unable to allocate memory for property "//trim(prop_name)

    ! Determine which species this property applies to
    ispecies = -1
    do i = 1, pdata%nspecies, 1
       if(trim(adjustl(pdata%species(i)%name)).eq. &
            trim(adjustl(species_name))) ispecies = i
    end do
    if(ispecies.eq.-1)call terminate('Unable to find species index!')
    i = ispecies
    
    ! Get index of the new species
    pdata%species(i)%nprops = pdata%species(i)%nprops + 1
    j = pdata%species(i)%nprops
    if(j.gt.maxprops)call terminate('Increase MAXPROPS!')

    ! Record if this is the particle ID or mass
    if(present(is_id))then
       if(is_id)pdata%species(i)%id_index = j
    endif
    if(present(is_mass))then
       if(is_mass)pdata%species(i)%mass_index = j
    endif

    ! Determine whether this is a real or integer property
    select case(prop_type)
    case ("INTEGER")
       pdata%species(i)%property(j)%type = "INTEGER"
    case ("REAL")
       pdata%species(i)%property(j)%type = "REAL"
    case default
       call terminate("Data type must be REAL or INTEGER!")
    end select

    nullify(pdata%species(i)%property(j)%idata)
    nullify(pdata%species(i)%property(j)%rdata)

    ! Allocate storage, if we know the particle number
    if(pdata%species(i)%np.gt.-1)then
       call data_array_init(pdata%species(i)%property(j)%data, &
            pdata%species(i)%np)
    else
       call data_array_init(pdata%species(i)%property(j)%data)
    endif

    ! No data has been loaded yet for this property
    pdata%species(i)%property(j)%nloaded = 0

    ! Store the name
    pdata%species(i)%property(j)%name = trim(prop_name)

    ! Set return status
    particle_store_new_property%success = .true.

    return
  end function particle_store_new_property


!
! This adds data to the particle store
!
  type (result_type) function particle_store_add_data(pdata, &
       species_name, prop_name, pos, vel, idata, rdata)

    implicit none
    ! Parameters
    character(len=*)           :: species_name
    character(len=*), optional :: prop_name
    real   (kind=pos_kind),    dimension(:,:), optional :: pos
    real   (kind=vel_kind),    dimension(:,:), optional :: vel
    integer(kind=i_prop_kind), dimension(:),   optional :: idata
    real   (kind=r_prop_kind), dimension(:),   optional :: rdata
    type (pdata_type) :: pdata
    ! Internal
    integer :: i, j, k
    integer :: ispecies
    integer(kind=index_kind) :: np
    integer :: first, last,stat

    particle_store_add_data%success = .false.

    ! Determine which species we want to add data for
    ispecies = -1
    do i = 1, pdata%nspecies, 1
       if(trim(adjustl(pdata%species(i)%name)).eq. &
            trim(adjustl(species_name))) ispecies = i
    end do
    if(ispecies.eq.-1)call terminate('Unable to find species index!')
    i = ispecies

    ! Add pos/vel data
    if(present(pos))then
       np = ubound(pos,2,kind=index_kind)-lbound(pos,2,kind=index_kind)+1
       call data_array_add_elements(pdata%species(i)%posdata, pos, stat)
       if(stat.ne.0)then
          particle_store_add_data%string=&
               "Insufficient memory to load particle data"
          write(0,*)"stat = ", stat
          return
       endif
       pdata%species(i)%npos = pdata%species(i)%npos + np
    endif
    if(present(vel))then
#ifndef READ_VEL
       call terminate( 'Attempt to load velocities when compiled without -DREAD_VEL')
#endif
       np = ubound(vel,2,kind=index_kind)-lbound(vel,2,kind=index_kind)+1
       call data_array_add_elements(pdata%species(i)%veldata, vel, stat)
       if(stat.ne.0)then
          particle_store_add_data%string=&
               "Insufficient memory to load particle data"
          return
       endif
       pdata%species(i)%nvel = pdata%species(i)%nvel + np
    endif

    ! Add property data if specified
    if(present(prop_name))then

       ! Find out which property this is
       j = -1
       do k = 1, pdata%species(i)%nprops, 1
          if(pdata%species(i)%property(k)%name.eq.prop_name) j = k
       end do
       if(j.lt.1)then
          call terminate('Unable to identify property '//trim(prop_name))
       endif

       ! Check we don't have both types of data simultaneously
       if(present(idata).and.present(rdata)) &
            call terminate('Cannot have idata and rdata simultaneously')

       ! Check data type is consistent and add the data
       select case(pdata%species(i)%property(j)%type)
       case("INTEGER")
          if(present(idata))then
             np = ubound(idata,1,kind=index_kind)-lbound(idata,1,kind=index_kind)+1
             call data_array_add_elements( &
                  pdata%species(i)%property(j)%data, idata, stat)
             if(stat.ne.0)then
                particle_store_add_data%string=&
                     "Insufficient memory to load particle data"
                return
             endif
             pdata%species(i)%property(j)%nloaded = &
                  pdata%species(i)%property(j)%nloaded+np
          else
             call terminate('Attempt to add non-integer data to integer property!')
          endif
       case("REAL")
          if(present(rdata))then
             np = ubound(rdata,1,kind=index_kind)-lbound(rdata,1,kind=index_kind)+1
             call data_array_add_elements( &
                  pdata%species(i)%property(j)%data, rdata, stat)
             if(stat.ne.0)then
                particle_store_add_data%string=&
                     "Insufficient memory to load particle data"
                return
             endif
             pdata%species(i)%property(j)%nloaded = &
                  pdata%species(i)%property(j)%nloaded+np
          else
             call terminate('Attempt to add non-real data to real property!')
          endif
       case default
          call terminate('Unrecognised property type!')
       end select
    else
       ! Abort if we have a property name but no data
       if(present(idata).or.present(rdata))then
          call terminate('Property data supplied but no property name specified!')
       endif
    endif

    particle_store_add_data%success = .true.

    return
  end function particle_store_add_data

!
! This empties the particle store and deallocates
! memory
!
  subroutine particle_store_empty(pdata)

    implicit none
    integer :: i, j, ispecies
    type (pdata_type) :: pdata

    ! Deallocate octrees if they've been built
    if(pdata%trees_built)then
       do ispecies = 1, pdata%nspecies, 1
          if(pdata%species(ispecies)%np.gt.0)then
             call deallocate_octree(pdata%species(ispecies)%octree)
          endif
       end do
       pdata%trees_built = .false.
    endif

    pdata%nspecies = 0
    pdata%loaded   = .false.
    pdata%isnap    = -1
    
    do i = 1, maxspecies, 1
       pdata%species(i)%np     = 0
       pdata%species(i)%nprops = 0
       pdata%species(i)%name   = "none"
       if(associated(pdata%species(i)%pos)) deallocate(pdata%species(i)%pos)
       if(associated(pdata%species(i)%vel)) deallocate(pdata%species(i)%vel)
       if(associated(pdata%species(i)%hsml))deallocate(pdata%species(i)%hsml)
       if(associated(pdata%species(i)%selected))&
            deallocate(pdata%species(i)%selected)
       nullify(pdata%species(i)%pos)
       nullify(pdata%species(i)%vel)
       nullify(pdata%species(i)%hsml)
       nullify(pdata%species(i)%selected)
       call data_array_dealloc(pdata%species(i)%posdata)
       call data_array_dealloc(pdata%species(i)%veldata)
       do j = 1, maxprops, 1
          pdata%species(i)%property(j)%name = "none"
          pdata%species(i)%property(j)%type = "none"
          if(associated(pdata%species(i)%property(j)%idata)) &
               deallocate(pdata%species(i)%property(j)%idata)
          if(associated(pdata%species(i)%property(j)%rdata)) &
               deallocate(pdata%species(i)%property(j)%rdata)
          nullify(pdata%species(i)%property(j)%idata)
          nullify(pdata%species(i)%property(j)%rdata)
          call data_array_dealloc(pdata%species(i)%property(j)%data)
       end do
    end do

    pdata%is_sample = .false.

    return
  end subroutine particle_store_empty

!
! This returns the number of types of particle are
! loaded and how many there are of each type.
!
  subroutine particle_store_contents(pdata, get_nspecies, get_np, &
       get_species_names, get_fsample, get_isnap)

    implicit none
    ! Parameters
    integer,                        optional :: get_nspecies
    integer(kind=index_kind), dimension(:), optional :: get_np
    character(len=*), dimension(:), optional :: get_species_names
    real,                           optional :: get_fsample
    integer,                        optional :: get_isnap
    type (pdata_type) :: pdata
    ! Internal
    integer :: i
    
    ! Sampling rate
    if(present(get_fsample))then
       if(.not.pdata%is_sample)then
          call terminate('Attempted to get sampling rate for non-sampled dataset!')
       endif
       get_fsample = pdata%fsample
    endif

    ! Number of species
    if(present(get_nspecies)) get_nspecies = pdata%nspecies

    ! Number of particles per species
    if(present(get_np)) then
       get_np = 0
       get_np(1:pdata%nspecies) = pdata%species(1:pdata%nspecies)%np
    endif

    ! Names of the species
    if(present(get_species_names))then
       do i = 1, pdata%nspecies, 1
          get_species_names(i) = trim(adjustl(pdata%species(i)%name))
       end do
    endif

    ! Snapshot number
    if(present(get_isnap))then
       get_isnap = pdata%isnap
    endif

    return
  end subroutine particle_store_contents

!
! This returns information about a particle species
!
  subroutine particle_store_species(pdata, ispecies, get_name, get_np, &
       get_nprops, get_propnames, get_pos, get_vel, get_hsml, get_mass, &
       get_selected, get_id)

    implicit none

    ! Parameters
    integer, optional :: ispecies, get_nprops
    integer(kind=index_kind), optional :: get_np
    real(kind=pos_kind),    dimension(:,:), pointer, optional :: get_pos
    real(kind=vel_kind),    dimension(:,:), pointer, optional :: get_vel
    real,                   dimension(:),   pointer, optional :: get_hsml
    real(kind=r_prop_kind), dimension(:),   pointer, optional :: get_mass
    integer,                dimension(:),   pointer, optional :: get_selected
    integer(kind=i_prop_kind), dimension(:),   pointer, optional :: get_id
    character(len=*), optional :: get_name
    character(len=*), dimension(:), optional :: get_propnames
    type (pdata_type) :: pdata
    ! Internal
    integer :: nprops
    integer :: i, j

    if(present(get_name))then
       get_name = trim(pdata%species(ispecies)%name)
    endif

    if(present(get_np))then
       get_np = pdata%species(ispecies)%np
    endif

    if(present(get_nprops))then
       get_nprops = pdata%species(ispecies)%nprops
    endif

    if(present(get_pos))then
       get_pos => pdata%species(ispecies)%pos
    endif

    if(present(get_vel))then
#ifndef READ_VEL
       call terminate( 'Attempt to get velocity data when compiled without -DREAD_VEL')
#endif
       get_vel => pdata%species(ispecies)%vel
    endif

    if(present(get_selected))then
       if(.not.associated(pdata%species(ispecies)%selected))then
          call terminate("Attempt to get selection status when not allocated")
       endif
       get_selected => pdata%species(ispecies)%selected
    endif

    ! Smoothing lengths are only calculated if needed
    if(present(get_hsml))then
       if(.not.associated(pdata%species(ispecies)%hsml))then
          allocate(pdata%species(ispecies)%hsml(pdata%species(ispecies)%np))
          pdata%species(ispecies)%hsml = -1
       endif
       if(any(pdata%species(ispecies)%hsml.lt.0.0))then
          call particle_store_calculate_hsml_octree(pdata, ispecies)
       endif
       get_hsml => pdata%species(ispecies)%hsml
    endif

    if(present(get_propnames))then
       nprops = pdata%species(ispecies)%nprops
       do i = 1, nprops, 1
          get_propnames(i) = &
               trim(pdata%species(ispecies)%property(i)%name)
       end do
    endif

    ! Masss can be accessed as a particle property or via the get_mass keyword
    if(present(get_mass))then
       if(pdata%species(ispecies)%np.eq.0)then
          ! No particles, so return a null pointer
          nullify(get_mass)
       else
          ! Find the mass array
          i = pdata%species(ispecies)%mass_index
          if(i.lt.1)then
             write(0,*)ispecies
             call terminate('particle_store_species() - Unable to find Mass in snapshot data!')
          endif
          get_mass => pdata%species(ispecies)%property(i)%rdata
       endif
    endif

    ! IDs can be accessed as a particle property or via the get_id keyword
    if(present(get_id))then
       if(pdata%species(ispecies)%np.eq.0)then
          ! No particles, so return a null pointer
          nullify(get_id)
       else
          ! Find the ID array
          i = pdata%species(ispecies)%id_index
          if(i.lt.1)call terminate('particle_store_species() - Unable to find ID in snapshot data!')
          get_id => pdata%species(ispecies)%property(i)%idata
       endif
    endif

    return
  end subroutine particle_store_species

!
! This returns information about a particle property
!
  subroutine particle_store_property(pdata, ispecies, iprop, propname, &
       get_name, get_type, get_idata, get_rdata, get_range)
    
    implicit none
    integer :: ispecies
    integer, optional :: iprop
    integer :: jprop
    character(len=*), optional :: propname
    character(len=*), optional :: get_name, get_type
    real(kind=r_prop_kind),    dimension(:), pointer, optional :: get_rdata
    integer(kind=i_prop_kind), dimension(:), pointer, optional :: get_idata
    type (pdata_type) :: pdata
    real, dimension(2), optional :: get_range
    integer :: i

    if(present(iprop).eqv.present(propname))call terminate( &
         'Must specify property name OR index - not both')

    if(present(propname))then
       jprop = -1
       do i = 1, pdata%species(ispecies)%nprops, 1
          if(pdata%species(ispecies)%property(i)%name.eq.propname) &
               jprop = i
       end do
       if(jprop.lt.0)call terminate('Unknown property name in particle_store_property()')
    else
       jprop = iprop
    endif

    if(present(get_name))get_name = &
         trim(pdata%species(ispecies)%property(jprop)%name)

    if(present(get_type))get_type = &
         trim(pdata%species(ispecies)%property(jprop)%type)

    if(present(get_idata))then
       get_idata => pdata%species(ispecies)%property(jprop)%idata
    endif

    if(present(get_rdata))then
       get_rdata => pdata%species(ispecies)%property(jprop)%rdata
    endif

    if(present(get_range))then
       get_range(1) = pdata%species(ispecies)%property(jprop)%val_min
       get_range(2) = pdata%species(ispecies)%property(jprop)%val_max
    endif

    return
  end subroutine particle_store_property
  
!
! This does a consistency check on the particle data. It should not
! be called until all the data has been loaded.
!
  subroutine particle_store_verify(pdata, check_count)

    implicit none
    integer(kind=index_kind) :: i, j, np
    type (pdata_type) :: pdata

    logical, optional :: check_count
    logical :: check
    integer :: stat

    if(present(check_count))then
       check = check_count
    else
       check = .true.
    endif

    do i = 1, pdata%nspecies, 1

       ! May have to get particle number from size of data array if it wasn't
       ! known in advance
       if(pdata%species(i)%np.lt.0)then
          pdata%species(i)%np = data_array_size(pdata%species(i)%posdata)
       endif

       if(check)then
          ! Check that all pos/mass/vel/id loaded
          if(pdata%species(i)%npos .ne.pdata%species(i)%np)then
             write(0,*)'npos = ',pdata%species(i)%npos
             write(0,*)'np   = ',pdata%species(i)%np
             call terminate('Positions not all loaded!')
          endif
#ifdef READ_VEL
          if(pdata%species(i)%nvel .ne.pdata%species(i)%np) &
               call terminate('Velocities not all loaded!')
#endif
       endif
       ! Copy pos/vel to contiguous arrays if necessary
       if(.not.associated(pdata%species(i)%pos))then
          call data_array_get_data(pdata%species(i)%posdata, &
               pdata%species(i)%pos, stat)
       endif
#ifdef READ_VEL
       if(.not.associated(pdata%species(i)%vel))then
          call data_array_get_data(pdata%species(i)%veldata, &
               pdata%species(i)%vel, stat)
       endif
#endif

       ! Allocate smoothing lengths, selection status etc
       np = pdata%species(i)%np

       ! Check properties all loaded
       do j = 1, pdata%species(i)%nprops, 1
          if(check)then
             if(pdata%species(i)%property(j)%nloaded.ne.&
                  pdata%species(i)%np)then
                call terminate('Property data not all loaded!')
             endif
          else
             ! If instructed to skip check, just assume all is well and
             ! set nloaded to reflect this.
             pdata%species(i)%property(j)%nloaded = pdata%species(i)%np
          endif
          ! Find range of values of this property
          if(pdata%species(i)%np.gt.0)then
             select case(pdata%species(i)%property(j)%type)
             case("REAL")
                if(.not.associated(pdata%species(i)%property(j)%rdata))then
                   call data_array_get_data( &
                        pdata%species(i)%property(j)%data, &
                        pdata%species(i)%property(j)%rdata, stat)
                endif
                pdata%species(i)%property(j)%val_min = &
                     minval(pdata%species(i)%property(j)%rdata)
                pdata%species(i)%property(j)%val_max = &
                     maxval(pdata%species(i)%property(j)%rdata)
             case("INTEGER")
                if(.not.associated(pdata%species(i)%property(j)%idata))then
                   call data_array_get_data( &
                        pdata%species(i)%property(j)%data, &
                        pdata%species(i)%property(j)%idata, stat)
                endif
                pdata%species(i)%property(j)%val_min = &
                     minval(pdata%species(i)%property(j)%idata)
                pdata%species(i)%property(j)%val_max = &
                     maxval(pdata%species(i)%property(j)%idata)
             case default
                call terminate('Unrecognised type value in particle_store_verify()')
             end select
          else
             pdata%species(i)%property(j)%val_min = 0.0
             pdata%species(i)%property(j)%val_max = 0.0
          endif
       end do
    end do

    pdata%loaded = .true.

    return
  end subroutine particle_store_verify

!
! Generate a random sample of the particles in one particle store
! and return it as a new particle store.
! 
! The sampling rate is chosen such that the total number of particles
! is approximately npmax.
!
! The sample will only contain particles within the specified radius
! about coordinates pos(1:3).
!
  type (result_type) function particle_store_sample(pdata, psample, npmax, &
       pos, radius, wrap)

    implicit none
    ! Parameters
    type (pdata_type) :: pdata, psample
    integer           :: npmax
    real(kind=pos_kind), dimension(3), optional :: pos
    real(kind=pos_kind), optional :: radius
    logical           :: wrap
    ! Internal
    integer(kind=index_kind), dimension(maxspecies) :: np_area, np_sample
    integer :: nspecies
    integer(kind=index_kind) :: i, j, k, ip
    real    :: fsample
    ! Function call result
    type (result_type) :: fresult
    ! Array of particle indexes
    type index_list_type
       integer(kind=index_kind) :: n
       integer(kind=index_kind), dimension(:), pointer :: idx
    end type index_list_type
    type(index_list_type), dimension(maxspecies) :: index_list
    real(pos_kind), dimension(3) :: sample_pos
    real(pos_kind) :: sample_radius
    ! Pointers to data arrays
    integer(kind=i_prop_kind), pointer, dimension(:) :: iptr_src, iptr_dest
    real(kind=r_prop_kind),    pointer, dimension(:) :: rptr_src, rptr_dest

    ! Check parameters
    if((present(pos).and.(.not.present(radius))).or. &
         (present(radius).and.(.not.present(pos)))) &
         call terminate('Must specify both or neither of pos and radius')

    ! Deallocate any existing particles in the output data set
    if(particle_store_loaded(psample))call particle_store_empty(psample)

    ! Get number of particle types
    nspecies = pdata%nspecies

    ! Get centre and radius of selected region
    if(present(pos))then
       sample_pos = pos
    else
       sample_pos = (/ 0.0, 0.0, 0.0 /)
    endif
    if(present(radius))then
       sample_radius = radius
    else
       sample_radius = 1.0e15
    endif

    ! Count particles in this region
    do i = 1, pdata%nspecies, 1
       if(pdata%species(i)%np.gt.0)then
          if(present(pos).and.pdata%boxsize.gt.0.0)then
             call periodic_radius_search( &
                  pdata%species(i)%octree, &
                  pdata%species(i)%pos,  &
                  pdata%boxsize, &
                  sample_pos(1:3),&
                  0.0_pos_kind,sample_radius, np_area(i))
          else
             call radius_search(         &
                  pdata%species(i)%octree, &
                  pdata%species(i)%pos,  &
                  sample_pos(1:3),0.0_pos_kind,sample_radius, np_area(i))
          endif
       else
          np_area(i) = 0
       endif
    end do

    ! Calculate sampling rate
    fsample = min(1.0, real(npmax, kind=real8byte) / &
         real(sum(np_area(1:nspecies)), kind=real8byte))

    ! Get indices of particles in the sample
    do i = 1, pdata%nspecies, 1
       if(pdata%species(i)%np.gt.0)then
          allocate(index_list(i)%idx(np_area(i)))
          if(present(pos).and.pdata%boxsize.gt.0.0)then
             call periodic_radius_search(         &
                  pdata%species(i)%octree, &
                  pdata%species(i)%pos,  &
                  pdata%boxsize, &
                  sample_pos(1:3), &
                  0.0_pos_kind,sample_radius,   &
                  index_list(i)%n,index_list(i)%idx,fsample)
          else
             call radius_search(         &
                  pdata%species(i)%octree, &
                  pdata%species(i)%pos,  &
                  sample_pos(1:3),0.0_pos_kind,sample_radius,   &
                  index_list(i)%n,index_list(i)%idx,fsample)
          endif
       else
          index_list(i)%n = 0
          nullify(index_list(i)%idx)
       endif
    end do

    ! Store sampling rate
    psample%is_sample = .true.
    psample%fsample   = min(1.0,fsample)

    ! Number of particles in the sample
    np_sample = index_list%n

    ! Allocate storage for the new data set
    do i = 1, nspecies, 1
       fresult = particle_store_new_species(psample,pdata%species(i)%name, &
            np_sample(i))
       if(.not.fresult%success)then
          call particle_store_empty(psample)
          particle_store_sample = fresult
          return
       endif
       ! Allocate storage for particle properties
       do j = 1, pdata%species(i)%nprops, 1
          fresult = particle_store_new_property(psample, &
               pdata%species(i)%name, &
               pdata%species(i)%property(j)%name, &
               pdata%species(i)%property(j)%type)
          if(.not.fresult%success)then
             call particle_store_empty(psample)
             particle_store_sample = fresult
             return
          endif
       end do
    end do
    
    ! Load the data into the new data set
    do i = 1, nspecies, 1

       ! Allocate storage
       allocate(psample%species(i)%pos(1:3,np_sample(i)))
       allocate(psample%species(i)%vel(1:3,np_sample(i)))
       allocate(psample%species(i)%selected(np_sample(i)))
       psample%species(i)%selected = 0
       psample%species(i)%id_index   = pdata%species(i)%id_index
       psample%species(i)%mass_index = pdata%species(i)%mass_index

       do ip = 1, np_sample(i), 1

          ! Get index of this particle in array
          j = index_list(i)%idx(ip)

          ! Store basic particle data
          psample%species(i)%pos(1:3,ip)  = pdata%species(i)%pos(1:3,j)
#ifdef READ_VEL
          psample%species(i)%vel(1:3,ip)  = pdata%species(i)%vel(1:3,j)
#endif
          ! Box wrap coordinates to copy nearest specified centre if periodic
          if(present(pos).and.pdata%boxsize.gt.0.0.and.wrap)then
             do j = 1, 3, 1
                if(psample%species(i)%pos(j,ip)-sample_pos(j).gt.0.5*pdata%boxsize) &
                     psample%species(i)%pos(j,ip) = psample%species(i)%pos(j,ip) - pdata%boxsize 
                if(psample%species(i)%pos(j,ip)-sample_pos(j).lt.-0.5*pdata%boxsize) &
                     psample%species(i)%pos(j,ip) = psample%species(i)%pos(j,ip) + pdata%boxsize 
             end do
          endif
       end do

       ! Load other particle properties
       do k = 1, pdata%species(i)%nprops, 1
          select case(pdata%species(i)%property(k)%type)
          case("INTEGER")
             allocate(psample%species(i)%property(k)%idata(np_sample(i)))
             iptr_src  =>   pdata%species(i)%property(k)%idata
             iptr_dest => psample%species(i)%property(k)%idata
             do ip = 1, np_sample(i), 1
                j = index_list(i)%idx(ip)
                iptr_dest(ip) = iptr_src(j)
             end do
          case("REAL")
             allocate(psample%species(i)%property(k)%rdata(np_sample(i)))
             rptr_src  =>   pdata%species(i)%property(k)%rdata
             rptr_dest => psample%species(i)%property(k)%rdata
             do ip = 1, np_sample(i), 1
                j = index_list(i)%idx(ip)
                rptr_dest(ip) = rptr_src(j)
             end do
          case default
             call terminate('particle_store_sample() - unrecognised type value')
          end select
       end do
       
       ! Next particle type
    end do

    ! Set the total particle number in the sample
    psample%species(1:nspecies)%np = np_sample(1:nspecies)
    
    ! Set return status to success
    particle_store_sample%success = .true.

    ! Mark this set as loaded
    psample%loaded = .true.

    ! Deallocate lists of IDs
    do i = 1, nspecies, 1
       if(associated(index_list(i)%idx))deallocate(index_list(i)%idx)
    end do

    ! Set snapshot number
    psample%isnap     = pdata%isnap
    psample%time      = pdata%time
    psample%redshift  = pdata%redshift
    psample%expansion = pdata%expansion
    psample%time_unit = trim(pdata%time_unit)
    psample%idsize    = pdata%idsize
    psample%boxsize   = pdata%boxsize

    return
  end function particle_store_sample


  subroutine particle_store_calculate_hsml_octree(pdata, ispecies)
!
! Calculate smoothing lengths for a set of particles
!
    implicit none
    integer :: ispecies
    integer(kind=index_kind) :: i
    type (octree_type) :: tree
    integer, parameter :: nb = 32
    real :: rdist
    type (pdata_type) :: pdata
    character(len=maxlen), dimension(maxspecies) :: name
    integer :: myid
! Stuff for OpenMP function calls
#ifdef _OPENMP
    integer, external :: OMP_GET_THREAD_NUM, OMP_GET_MAX_THREADS
#endif

    if(pdata%species(ispecies)%np.gt.0)then
       ! Build the tree structure
       call build_octree(tree,pdata%species(ispecies)%np, &
            pdata%species(ispecies)%pos,nb)
       ! Loop over particles finding neighbours
       call alloc_tree_workspace(tree, multithreaded=.true.)
!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i,rdist,myid)
#ifdef _OPENMP
       myid = OMP_GET_THREAD_NUM()
#else
       myid = 0
#endif
!$OMP DO SCHEDULE(DYNAMIC, 10000)
       do i = 1, pdata%species(ispecies)%np, 1
          pdata%species(ispecies)%hsml(i) = leaf_node_size(tree, pdata%species(ispecies)%pos(1:3,i))
       end do
!$OMP END DO
!$OMP END PARALLEL
       call free_tree_workspace(tree)
       call deallocate_octree(tree)
    endif

    return
  end subroutine particle_store_calculate_hsml_octree


  subroutine particle_store_calculate_hsml_neighbours(pdata, ispecies)
!
! Calculate smoothing lengths for a set of particles
!
    implicit none
    integer :: ispecies
    integer(kind=index_kind) :: i
    type (octree_type) :: tree
    integer :: nb
    integer(kind=index_kind), parameter         :: nngb = 16
    integer(kind=index_kind), dimension(nngb+1) :: ngb_idx
    real :: rdist
    type (pdata_type) :: pdata
    character(len=200) :: message
    character(len=maxlen), dimension(maxspecies) :: name
    integer :: myid
    integer(kind=index_kind) :: nextupdate
    integer(kind=index_kind) :: interval
! Stuff for OpenMP function calls
#ifdef _OPENMP
    integer, external :: OMP_GET_THREAD_NUM, OMP_GET_MAX_THREADS
#endif
    ! Number of  neighbours to find - may be less than nngb if
    ! there are fewer than nngb particles
    integer(kind=index_kind) :: nfind

    call particle_store_contents(pdata, get_species_names=name)
    write(message,*)"Calculating smoothing lengths: "//trim(name(ispecies))
    call progress_bar_display(message)
    call progress_bar_update(0.0)

    if(pdata%species(ispecies)%np.gt.0)then
       ! Build the tree structure
       nb   = nngb * 3
       call build_octree(tree,pdata%species(ispecies)%np, &
            pdata%species(ispecies)%pos,nb)
       ! Loop over particles finding neighbours
       call alloc_tree_workspace(tree, multithreaded=.true.)
!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i,ngb_idx,rdist,nextupdate,myid)
#ifdef _OPENMP
       myid = OMP_GET_THREAD_NUM()
#else
       myid = 0
#endif
       interval = max(pdata%species(ispecies)%np/20, 10000)
       nextupdate = interval
!$OMP DO SCHEDULE(DYNAMIC, 10000)
       do i = 1, pdata%species(ispecies)%np, 1
          nfind = min(nngb+1, pdata%species(ispecies)%np)
          call neighbour_search(tree, pdata%species(ispecies)%pos, &
               pdata%species(ispecies)%pos(1:3,i),nfind,ngb_idx)
          ! Get distance to nngb'th nearest neighbour
          rdist = sqrt(sum((pdata%species(ispecies)%pos(1:3,i)- &
               pdata%species(ispecies)%pos(1:3,ngb_idx(nfind)))**2))
          rdist = leaf_node_size(tree, pdata%species(ispecies)%pos(1:3,i))
          pdata%species(ispecies)%hsml(i) = 2.0*rdist
          if(i.gt.nextupdate.and.myid.eq.0)then
             call progress_bar_update(real(i)/real(pdata%species(ispecies)%np))
             nextupdate = nextupdate + interval
          endif
       end do
!$OMP END DO
!$OMP END PARALLEL
       call free_tree_workspace(tree)
       call deallocate_octree(tree)
    endif

    call progress_bar_close()

    return
  end subroutine particle_store_calculate_hsml_neighbours


  subroutine particle_store_build_trees(pdata)
!
! Build octree for particles in the supplied pdata object
!
    implicit none
    integer :: ispecies
    integer, parameter :: nb = 1000
    type(pdata_type) :: pdata
    integer(kind=int8byte) :: ntot, ndone

    call progress_bar_display("Indexing particle data...")
    call progress_bar_update(0.0)
    ntot  = sum(pdata%species%np)
    ndone = 0
    do ispecies = 1, pdata%nspecies
       if(pdata%species(ispecies)%np.gt.0)then
          call build_octree( &
               pdata%species(ispecies)%octree, &
               pdata%species(ispecies)%np,   &
               pdata%species(ispecies)%pos,  &
               nb, random_order=.true.)
          ndone = ndone + pdata%species(ispecies)%np
          call progress_bar_update(real(ndone)/real(ntot))
       endif
    end do
    call progress_bar_close()

    pdata%trees_built = .true.

    return
  end subroutine particle_store_build_trees


  subroutine particle_store_set_snapnum(pdata, isnap)
!
! Record the snapshot number for a pdata object
!
    implicit none
    type(pdata_type) :: pdata
    integer :: isnap

    pdata%isnap = isnap

    return
  end subroutine particle_store_set_snapnum


  subroutine particle_store_set_time(pdata, time, redshift, expansion, &
       time_unit)
!
! Record the name and value of the time variable
!
    implicit none
    type(pdata_type) :: pdata
    real             :: time, redshift, expansion
    character(len=*), optional :: time_unit

    pdata%time      = time
    pdata%redshift  = redshift
    pdata%expansion = expansion
    if(present(time_unit))then
       pdata%time_unit = trim(time_unit)
    else
       pdata%time_unit = ""
    endif

    return
  end subroutine particle_store_set_time
  

  subroutine particle_store_get_time(pdata, time, redshift, expansion, &
       time_unit)
!
! Record the name and value of the time variable
!
    implicit none
    type(pdata_type) :: pdata
    real             :: time, redshift, expansion
    character(len=*) :: time_unit

    time      = pdata%time
    redshift  = pdata%redshift
    expansion = pdata%expansion
    time_unit = trim(pdata%time_unit)
    
    return
  end subroutine particle_store_get_time


  subroutine particle_store_set_idsize(pdata, idsize)
!
! Record the number of bytes used to store an ID
!
    implicit none
    type(pdata_type) :: pdata
    integer          :: idsize

    pdata%idsize      = idsize

    return
  end subroutine particle_store_set_idsize


  subroutine particle_store_get_idsize(pdata, idsize)
!
! Record the number of bytes used to store an ID
!
    implicit none
    type(pdata_type) :: pdata
    integer          :: idsize

    idsize      = pdata%idsize

    return
  end subroutine particle_store_get_idsize


  subroutine particle_store_set_boxsize(pdata, boxsize)
!
! Record the number of bytes used to store an ID
!
    implicit none
    type(pdata_type) :: pdata
    real             :: boxsize

    pdata%boxsize      = boxsize

    return
  end subroutine particle_store_set_boxsize


  subroutine particle_store_get_boxsize(pdata, boxsize)
!
! Record the number of bytes used to store an ID
!
    implicit none
    type(pdata_type) :: pdata
    real             :: boxsize

    boxsize      = pdata%boxsize

    return
  end subroutine particle_store_get_boxsize


  subroutine particle_store_cleanup(pdata)
!
! Deallocate any properties that have not been fully loaded. This is
! called if an attempt to read supplementary files fails.
!
    implicit none
    type(pdata_type) :: pdata
    integer          :: i, j
    integer          :: nprops

    do i = 1, pdata%nspecies, 1
       if(pdata%species(i)%nprops.gt.0)then
          j = 1
          do while(j.le.pdata%species(i)%nprops)
             if(pdata%species(i)%property(j)%nloaded.ne.pdata%species(i)%np)&
                  then
                ! This property has not been loaded correctly and should
                ! be removed.
                ! Deallocate memory if necessary.
                if(associated(pdata%species(i)%property(j)%idata))then
                   deallocate(pdata%species(i)%property(j)%idata)
                   nullify(pdata%species(i)%property(j)%idata)
                endif
                if(associated(pdata%species(i)%property(j)%rdata))then
                   deallocate(pdata%species(i)%property(j)%rdata)
                   nullify(pdata%species(i)%property(j)%rdata)
                endif
                call data_array_dealloc(pdata%species(i)%property(j)%data)
                ! Remove it from the array of properties
                nprops = pdata%species(i)%nprops
                if(j.lt.nprops)then
                   pdata%species(i)%property(j:nprops-1) = &
                        pdata%species(i)%property(j+1:nprops)
                   j = j - 1
                endif
                pdata%species(i)%nprops = pdata%species(i)%nprops - 1
             endif
             j = j + 1
          end do
       endif
    end do

    return
  end subroutine particle_store_cleanup


  subroutine particle_store_evaluate_property(pdata, ispecies, iprop, pos, &
       nngb, res)
!
! Evaluate the specified property at the specified position by averaging
! over nearest neighbours 
!
    implicit none
    ! Parameters
    type(pdata_type),                  intent(in)  :: pdata
    integer,                           intent(in)  :: ispecies, iprop
    real(kind=pos_kind), dimension(3), intent(in)  :: pos
    integer,                           intent(in)  :: nngb
    real(kind=real8byte),              intent(out) :: res(3)
    ! Internal
    integer(kind=index_kind)                  :: n
    integer(kind=index_kind), dimension(nngb) :: idx
    real(kind=real8byte) :: total, propmin, propmax
    integer(kind=index_kind) :: i, ipart

    ! Build octrees if necessary
    if(.not.pdata%trees_built) &
         call particle_store_build_trees(pdata)

    ! Find neighbours
    if(pdata%species(ispecies)%np.ge.nngb)then
       call alloc_tree_workspace(pdata%species(ispecies)%octree,&
            multithreaded=.false.)
       call neighbour_search(pdata%species(ispecies)%octree, &
            pdata%species(ispecies)%pos, pos, int(nngb,index_kind), idx)
       call free_tree_workspace(pdata%species(ispecies)%octree)
       n = nngb
    else if (pdata%species(ispecies)%np.gt.0) then
       ! If there are fewer than nngb neighbours, use all particles
       n = pdata%species(ispecies)%np
       do i = 1, n, 1
          idx(i) = i
       end do
    else
       ! If there are no particles can't do anything
        res = -1
       return
    endif

    ! Loop over particles
    propmin = huge(propmin)
    propmax = -huge(propmax)
    total   = 0
    select case(pdata%species(ispecies)%property(iprop)%type)
    case("INTEGER")
       do i = 1, n, 1
          ipart = idx(i)
          propmin = min(propmin, &
               real(pdata%species(ispecies)%property(iprop)%idata(ipart), &
               kind=real8byte))
          propmax = max(propmax, &
               real(pdata%species(ispecies)%property(iprop)%idata(ipart), &
               kind=real8byte))
          total = total + &
               pdata%species(ispecies)%property(iprop)%idata(ipart)
       end do
    case("REAL")
       do i = 1, n, 1
          ipart = idx(i)
          propmin = min(propmin, &
               real(pdata%species(ispecies)%property(iprop)%rdata(ipart), &
               kind=real8byte))
          propmax = max(propmax, &
               real(pdata%species(ispecies)%property(iprop)%rdata(ipart), &
               kind=real8byte))
          total = total + &
               pdata%species(ispecies)%property(iprop)%rdata(ipart)
       end do
    end select
    
    total = total / n
    res = (/propmin, propmax, total/)

    return
  end subroutine particle_store_evaluate_property


end module particle_store
