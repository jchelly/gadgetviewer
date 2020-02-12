module dummy_reader
!
! Generates large 'fake' snapshots for testing
!
#include "../../config.h"
  use data_types
  use return_status
  use particle_store
  use progress_bar
  use summary
  use string_module
  use partial_read_info

  implicit none
  private
  save

  public :: dummy_open
  public :: dummy_read

contains

!
! Scan a simulation directory and store information
! required for reading snapshots
!
  type(result_type) function dummy_open(fname, isnap, rinfo)

    implicit none
    ! Parameters
    character(len=*)     :: fname
    integer, intent(out) :: isnap
    type (read_info)     :: rinfo
    ! Internal
    type(result_type) :: res

    dummy_open%success = .false.

    ! Check we haven't been asked to use spatial indexing
    if(rinfo%use_index)then
       dummy_open%string  = &
            "No spatial indexing in dummy snapshot"
       return
    endif

    ! Only make dummy snapshot if right filename is specified
    if(fname.ne."DUMMY_SNAPSHOT")then
       dummy_open%string  = "Wrong filename!"
       return
    endif

    isnap = 0

    dummy_open%success = .true.
    return

  end function dummy_open

!
! Read the specified snapshot, if possible
!
  type (result_type) function dummy_read(isnap, rinfo)

    implicit none
    ! Parameters
    integer :: isnap
    type (read_info) :: rinfo
    ! Internal
    integer :: ifile
    integer :: nmass
    integer(kind=int8byte), dimension(6) :: nptot
    integer :: ios, i, j, istat, nblock, np, ispecies
    integer(kind=index_kind) :: ipart
    integer(kind=index_kind) :: iid
    ! Temporary storage for particles
    ! Positions
    real(kind=real4byte),    dimension(:,:), allocatable :: pos4
    real(kind=real8byte),    dimension(:,:), allocatable :: pos8
    ! Velocities
#ifdef READ_VEL
    real(kind=real4byte),    dimension(:,:), allocatable :: vel4
    real(kind=real8byte),    dimension(:,:), allocatable :: vel8
#endif
    ! Masses
    real(kind=real4byte),    dimension(:),   allocatable :: mass4
    real(kind=real8byte),    dimension(:),   allocatable :: mass8
    ! IDs
    integer(kind=int4byte),  dimension(:),   allocatable :: id4
    integer(kind=int8byte),  dimension(:),   allocatable :: id8
    ! SPH properties
    real(kind=real4byte),    dimension(:),   allocatable :: rho4, u4
    real(kind=real8byte),    dimension(:),   allocatable :: rho8, u8
    ! Particle type names
    character(len=50), dimension(6) :: species_name
    ! Result from function calls
    type (result_type) :: res
    real :: time, redshift

    dummy_read%success = .false.

    ! Check we haven't been asked to use spatial indexing
    if(rinfo%use_index)then
       dummy_read%success = .false.
       dummy_read%string  = &
            "No spatial indexing in Gadget binary-1 snapshot"
       return
    endif

    ! Check we're not doing a partial read
    if(rinfo%do_sphere.or.rinfo%do_sampling.or.rinfo%just_this_file)then
       dummy_read%success = .false.
       dummy_read%string  = &
            "Can't do partial read of dummy snapshot!"
       return
    endif

    ! At this point we assume the file is readable, so deallocate the old
    ! snapshot
    call particle_store_empty(pdata)
    call particle_store_empty(psample)

    ! Pick a particle number
    nptot    = 0
    nptot(2) = 3000000000_int8byte

    ! Now we know how many particles, allocate storage
    do i = 1, 6, 1
       select case(i)
       case(1)
          species_name(i)="Type 0 - Gas"
       case(2)
          species_name(i)="Type 1 - Dark matter"
       case(3)
          species_name(i)="Type 2 - Boundary"
       case(4)
          species_name(i)="Type 3 - Boundary"
       case(5)
          species_name(i)="Type 4 - Boundary/stars"
       case(6)
          species_name(i)="Type 5 - Other"
       end select

       ! May not know in advance how many particles we'll have if sampling
       if(rinfo%do_sampling.or.rinfo%do_sphere.or.rinfo%just_this_file)then
          res = particle_store_new_species(pdata,species_name(i))
       else
          res = particle_store_new_species(pdata,species_name(i),&
               int(nptot(i),index_kind))
       endif
       ! Abort if memory allocation fails
       if(.not.res%success)then
          dummy_read = res ! Pass back return status
          call particle_store_empty(pdata) ! Clean up any memory allocated
          return
       endif
    end do

    ! Allocate storage for masses and IDs
    do i = 1, 6, 1

       res = particle_store_new_property(pdata,species_name(i),"Mass", &
            "REAL", is_mass=.true.)
       if(.not.res%success)then
          dummy_read = res
          call particle_store_empty(pdata)
          return
       endif
       res = particle_store_new_property(pdata,species_name(i),"ID", &
            "INTEGER", is_id=.true.)
       if(.not.res%success)then
          dummy_read = res
          call particle_store_empty(pdata)
          return
       endif
    end do

    call progress_bar_update(0.0)

    ! Generate fake data in chunks to avoid using lots of extra memory
    nblock = 10000000
    do ispecies = 1, 6, 1
       do ipart = 1, nptot(ispecies), nblock
          np = min(nblock, nptot(ispecies)-ipart+1)

          ! Generate random positions
          allocate(pos4(3,np), stat=istat)
          if(istat.ne.0)then
             dummy_read%string = "Failed to allocate memory"
             call particle_store_empty(pdata)
             return
          endif
          call random_number(pos4)
          res = particle_store_add_data(pdata, species_name(ispecies), &
               pos=real(pos4,pos_kind))
          deallocate(pos4)
          if(.not.res%success)then
             dummy_read = res
             call particle_store_empty(pdata)
             return
          endif

#ifdef READ_VEL
          ! Generate random velocities
          allocate(vel4(3,np), stat=istat)
          if(istat.ne.0)then
             dummy_read%string = "Failed to allocate memory"
             call particle_store_empty(pdata)
             return
          endif
          call random_number(vel4)
          res = particle_store_add_data(pdata, species_name(ispecies), &
               vel=real(vel4,vel_kind))
          deallocate(vel4)
          if(.not.res%success)then
             dummy_read = res
             call particle_store_empty(pdata)
             return
          endif
#endif

          ! Generate sequential IDs
          allocate(id8(np), stat=istat)
          if(istat.ne.0)then
             dummy_read%string = "Failed to allocate memory"
             call particle_store_empty(pdata)
             return
          endif
          do iid = ipart, ipart+np-1, 1
             id8(iid-ipart+1) = iid
          end do          
          res = particle_store_add_data(pdata, species_name(ispecies), &
               idata=int(id8,i_prop_kind), prop_name="ID")
          deallocate(id8)
          if(.not.res%success)then
             dummy_read = res
             call particle_store_empty(pdata)
             return
          endif

          ! Generate masses
          allocate(mass4(np), stat=istat)
          if(istat.ne.0)then
             dummy_read%string = "Failed to allocate memory"
             call particle_store_empty(pdata)
             return
          endif
          mass4 = 1.0
          res = particle_store_add_data(pdata, species_name(ispecies), &
               rdata=real(mass4,r_prop_kind), prop_name="Mass")
          deallocate(mass4)
          if(.not.res%success)then
             dummy_read = res
             call particle_store_empty(pdata)
             return
          endif

          call progress_bar_update(&
               real(sum(nptot(1:ispecies-1))+ipart+np-1)/real(sum(nptot)))

       end do
    end do

    call progress_bar_update(1.0)

    ! Record the redshift
    time     = real(isnap+1)
    redshift = 100.0 / real(isnap+1)
    call particle_store_set_time(pdata, real(time), real(redshift), real(time))

    dummy_read%success = .true.
    
    call summary_add_line(s,"Fake snapshot!")

    return
  end function dummy_read

end module dummy_reader
