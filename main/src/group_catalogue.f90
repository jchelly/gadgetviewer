module group_catalogue

  use data_types
  use gadget_path
  use particle_store
  use return_status
  use gadget_groups
  use velociraptor_groups
  use gadget4_groups
  use sort
  use progress_bar

  implicit none

  public :: group_catalogue_init
  public :: group_catalogue_add
  public :: group_catalogue_read
  public :: group_catalogue_read_all

  ! Format types
  integer, parameter, public :: FORMAT_TYPE_SUBFIND      = 0
  integer, parameter, public :: FORMAT_TYPE_VELOCIRAPTOR = 1
  integer, parameter, public :: FORMAT_TYPE_GADGET4      = 2

  ! List of currently loaded catalogues
  integer, parameter :: ngroupcatmax = 10
  integer            :: ngroupcat
  type groupcat_type
     integer               :: format_type
     integer               :: format_subtype
     type(path_data_type)  :: path_data
  end type groupcat_type
  type (groupcat_type), dimension(ngroupcatmax) :: groupcat

contains

  subroutine group_catalogue_init()
!
! This resets the module and should be called whenever a new simulation
! is loaded.
!
    implicit none

    ngroupcat = 0

    return
  end subroutine group_catalogue_init


  type (result_type) function group_catalogue_add(isnap, format_type, &
       format_subtype, input_filename)
!
! Add a new group catalogue
!
    implicit none
    integer               :: format_type, format_subtype
    character(len=*)      :: input_filename
    integer               :: isnap, jsnap
    type(result_type)     :: res
    integer               :: icat

    ! Check we don't have too many catalogues
    icat = ngroupcat + 1
    if(icat.gt.ngroupcatmax)then
       group_catalogue_add%success = .false.
       group_catalogue_add%string  = "Too many group catalogues loaded!"
       return
    endif

    ! Try to interpret the filename
    res = gadget_path_extract(input_filename, jsnap, groupcat(icat)%path_data)
    if(.not.res%success)then
       group_catalogue_add%success = .false.
       group_catalogue_add%string  = "Group file name format not recognised"
       return
    endif

    ! Store format info
    groupcat(icat)%format_type    = format_type
    groupcat(icat)%format_subtype = format_subtype

    ! Try to read the files
    res = group_catalogue_read(icat, isnap)
    if(.not.res%success)then
       group_catalogue_add = res
       return
    endif

    ! If that worked, record this group catalogue so that it will
    ! be read in if we change snapshots
    ngroupcat = ngroupcat + 1

    group_catalogue_add%success = .true.

    return
  end function group_catalogue_add


  subroutine group_catalogue_read_all(isnap)
!
! Try to read all of the group catalogues
!
    implicit none
    integer :: icat, isnap
    type (result_type) :: res

    do icat = 1, ngroupcat, 1
       res = group_catalogue_read(icat, isnap)
    end do

    return
  end subroutine group_catalogue_read_all


  type (result_type) function group_catalogue_read(icat, isnap)
!
! Read the specified group catalogue
!
    implicit none
    integer :: icat, isnap
    integer :: nfof, nsub, nids, id_size
    integer, dimension(:), allocatable :: foflen, foffset, sublen, suboffset
    integer(kind=i_prop_kind), dimension(:), allocatable :: groupids
    integer(kind=i_prop_kind), dimension(:), allocatable :: fofgrnr, subgrnr
    integer(kind=i_prop_kind), dimension(:), allocatable :: ID, hostHaloID
    integer, dimension(:), allocatable :: groupids_idx, ids_idx
    integer(kind=i_prop_kind), dimension(:), pointer :: ids
    integer, dimension(:), allocatable :: nr
    integer :: istat
    integer :: nspecies, ispecies
    integer(kind=index_kind) :: np
    type (result_type) :: res
    character(len=10) :: str

    write(str,'(1i3)')icat
    call progress_bar_display("Reading group catalogue "// &
         trim(adjustl(str))//"...")
    call progress_bar_update(0.0)

    ! Get size of particle IDs
    call particle_store_get_idsize(pdata, id_size)

    ! Read the data from the file
    select case(groupcat(icat)%format_type)
    case(FORMAT_TYPE_SUBFIND)
       res = gadget_groups_read(groupcat(icat)%format_subtype, isnap, &
            groupcat(icat)%path_data, id_size, nfof, nsub, nids, &
            foflen, foffset, sublen, suboffset, groupids)
    case(FORMAT_TYPE_VELOCIRAPTOR)
       res = velociraptor_groups_read(isnap, groupcat(icat)%path_data, &
            nfof, nsub, nids, foflen, foffset, sublen, suboffset, groupids, &
            ID, hostHaloID)
    case(FORMAT_TYPE_GADGET4)
       res = gadget4_groups_read(isnap, groupcat(icat)%path_data, icat)
       ! There's no need for matching particle IDs if we have a group sorted snapshot
       if(res%success)then
          group_catalogue_read%success = .true.
          call particle_store_verify(pdata)
          call progress_bar_close()
          return
       endif
    case default
       call terminate("Unrecognised subhalo format index!")
    end select
    if(.not.res%success)then
       group_catalogue_read = res
       call cleanup()
       call progress_bar_close()
       return
    endif

    call progress_bar_update(0.4)

    ! Get sorting index for group IDs    
    allocate(groupids_idx(nids), stat=istat)
    if(istat.ne.0)then
       group_catalogue_read%success = .false.
       group_catalogue_read%string  = "Unable to allocate memory"
       call cleanup()
       call progress_bar_close()
       return
    endif
    call openmp_sort_index(groupids, groupids_idx)

    call progress_bar_update(0.7)

    ! For each particle type determine group membership
    call particle_store_contents(pdata, get_nspecies=nspecies)
    do ispecies = 1, nspecies, 1
       call particle_store_species(pdata, ispecies, get_np=np, get_id=ids)

       ! Not implemented for >=2**31 particles yet
       if(np.ge.2_int8byte**31)then
          group_catalogue_read%success = .false.
          group_catalogue_read%string  = "Not implemented for >=2**31 particles!"
          call cleanup()
          call progress_bar_close()
          return
       endif

       ! Ignore particle types with no particles
       if(np.eq.0)cycle

       ! Allocate grnr and workspace arrays
       allocate(nr(nids), fofgrnr(np), subgrnr(np), ids_idx(np), stat=istat)
       if(istat.ne.0)then
          group_catalogue_read%success = .false.
          group_catalogue_read%string  = "Unable to allocate memory"
          call cleanup()
          call progress_bar_close()
          return
       endif

       ! Get sorting indexes for particle IDs
       call openmp_sort_index(ids, ids_idx)

       ! Determine group membership for each particle
       if(allocated(foflen))then
          call find_grnr(nfof, foflen, foffset, fofgrnr)
       endif
       if(allocated(sublen))then
          call find_grnr(nsub, sublen, suboffset, subgrnr)
       endif

       ! Add the new particle properties
       select case(groupcat(icat)%format_type)
       case(FORMAT_TYPE_SUBFIND)
          res = gadget_groups_add_properties(ispecies, icat, fofgrnr, subgrnr)
       case(FORMAT_TYPE_VELOCIRAPTOR)
          res = velociraptor_groups_add_properties(groupcat(icat)%path_data, isnap, &
               ispecies, icat, subgrnr, ID, hostHaloID)
       case default
          call terminate("Unrecognised subhalo format index!")
       end select
       deallocate(nr, fofgrnr, subgrnr, ids_idx)
       if(.not.res%success)then
          group_catalogue_read = res
          call particle_store_cleanup(pdata)
          call progress_bar_close()
          call cleanup()
          return
       endif
    
       ! Next particle type
    end do
    
    call progress_bar_update(1.0)

    deallocate(groupids_idx)

    call particle_store_verify(pdata)   
    group_catalogue_read%success = .true.

    call progress_bar_close()

    return
    
  contains

    subroutine cleanup()

      implicit none

      if(allocated(foflen))     deallocate(foflen)
      if(allocated(foffset))    deallocate(foffset)
      if(allocated(sublen))     deallocate(sublen)
      if(allocated(suboffset))  deallocate(suboffset)
      if(allocated(groupids))   deallocate(groupids)
      if(allocated(fofgrnr))    deallocate(fofgrnr)
      if(allocated(subgrnr))    deallocate(subgrnr)
      if(allocated(ID))         deallocate(ID)
      if(allocated(hostHaloID)) deallocate(hostHaloID)
      if(allocated(groupids_idx)) deallocate(groupids_idx)
      if(allocated(ids_idx))      deallocate(ids_idx)
      if(allocated(nr))           deallocate(nr)

      return
    end subroutine cleanup


    subroutine find_grnr(ngroup, len, offset, grnr)

      implicit none
      integer :: ngroup
      integer, dimension(:) :: len, offset
      integer(kind=i_prop_kind), dimension(:) :: grnr
      integer :: i, j
      integer :: nmatched

      nmatched = 0

      ! Determine group membership for each particle ID in groupids
      nr   = 0
      grnr = 0
      do i = 1, ngroup, 1
         do j = offset(i), offset(i)+len(i)-1, 1
            if(j.gt.0.and.j.le.nids)nr(j) = i
         end do
      end do

      ! Match up IDs
      i = 1
      do j = 1, nids, 1
         do while(ids(ids_idx(i)).lt.groupids(groupids_idx(j)).and.i.lt.np)
            i = i + 1
         end do
         if(ids(ids_idx(i)).eq.groupids(groupids_idx(j)))then
            grnr(ids_idx(i)) = nr(groupids_idx(j))
            nmatched = nmatched + 1
         endif
      end do

      return
    end subroutine find_grnr

  end function group_catalogue_read

end module group_catalogue
