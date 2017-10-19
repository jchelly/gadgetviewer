module group_catalogue

  use data_types
  use gadget_path
  use particle_store
  use return_status
  use gadget_groups
  use sort
  use progress_bar

  implicit none

  public :: group_catalogue_init
  public :: group_catalogue_add
  public :: group_catalogue_read
  public :: group_catalogue_read_all

  ! List of currently loaded catalogues
  integer, parameter :: ngroupcatmax = 10
  integer            :: ngroupcat
  type groupcat_type
     integer         :: iformat
     type(path_data_type) :: tab_path
     type(path_data_type) :: ids_path
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


  type (result_type) function group_catalogue_add(isnap, iformat, tab_file)
!
! Add a new group catalogue
!
    implicit none
    integer               :: iformat
    character(len=*)      :: tab_file
    character(len=maxlen) :: ids_file
    integer               :: isnap, jsnap
    type(result_type)     :: res
    integer               :: icat
    integer               :: i

    icat = ngroupcat + 1
    if(icat.gt.ngroupcatmax)then
       group_catalogue_add%success = .false.
       group_catalogue_add%string  = "Too many group catalogues loaded!"
       return
    endif

    ! Guess ids file name by replacing "tab" with "ids" in tab_file
    i = index(tab_file, "tab", back=.true.)
    if(i.gt.0)then
       ids_file = trim(tab_file)
       ids_file(i:i+2) = "ids"
    else
       group_catalogue_add%success = .false.
       group_catalogue_add%string  = "Tab file name format not recognised"
       return
    endif

    res = gadget_path_extract(tab_file, jsnap, groupcat(icat)%tab_path)
    if(.not.res%success)then
       group_catalogue_add%success = .false.
       group_catalogue_add%string  = "Tab file name format not recognised"
       return
    endif

    res = gadget_path_extract(ids_file, jsnap, groupcat(icat)%ids_path)
    if(.not.res%success)then
       group_catalogue_add%success = .false.
       group_catalogue_add%string  = "IDs file name format not recognised"
       return
    endif

    groupcat(icat)%iformat = iformat

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
    integer, dimension(:), pointer :: foflen, foffset, sublen, suboffset
    integer(kind=i_prop_kind), dimension(:), pointer :: groupids
    integer(kind=i_prop_kind), dimension(:), pointer :: fofgrnr, subgrnr
    integer, dimension(:), pointer :: groupids_idx, ids_idx
    integer(kind=i_prop_kind), dimension(:), pointer :: ids
    integer, dimension(:), pointer :: nr
    integer :: istat
    integer :: nspecies, ispecies
    integer(kind=index_kind) :: np
    type (result_type) :: res
    character(len=maxlen) :: propname
    character(len=maxlen), dimension(maxspecies) :: species_name
    character(len=10) :: str

    write(str,'(1i3)')icat
    call progress_bar_display("Reading group catalogue "// &
         trim(adjustl(str))//"...")
    call progress_bar_update(0.0)

    nullify(foflen, foffset, sublen, suboffset, groupids, fofgrnr, subgrnr)

    ! Get size of particle IDs
    call particle_store_get_idsize(pdata, id_size)

    ! Read the data from the file
    res = gadget_groups_read(groupcat(icat)%iformat, isnap, &
         groupcat(icat)%tab_path, groupcat(icat)%ids_path, &
         id_size, nfof, nsub, nids, &
         foflen, foffset, sublen, suboffset, groupids)
    if(.not.res%success)then
       group_catalogue_read = res
       call progress_bar_close()
       return
    endif

    call progress_bar_update(0.4)

    ! Get sorting index for group IDs    
    allocate(groupids_idx(nids), stat=istat)
    if(istat.ne.0)then
       group_catalogue_read%success = .false.
       group_catalogue_read%string  = "Unable to allocate memory"
       return
    endif
    call openmp_sort_index(groupids, groupids_idx)

    call progress_bar_update(0.7)

    ! For each particle type determine group membership
    call particle_store_contents(pdata, get_nspecies=nspecies, &
         get_species_names=species_name)
    do ispecies = 1, nspecies, 1
       call particle_store_species(pdata, ispecies, get_np=np, get_id=ids)

       ! Not implemented for >=2**31 particles yet
       if(np.ge.2_int8byte**31)then
          group_catalogue_read%success = .false.
          group_catalogue_read%string  = "Not implemented for >=2**31 particles!"
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
          call progress_bar_close()
          return
       endif

       ! Get sorting indexes for particle IDs
       call openmp_sort_index(ids, ids_idx)

       ! Determine group membership for each particle
       if(associated(foflen))then
          call find_grnr(nfof, foflen, foffset, fofgrnr)
       endif
       if(associated(sublen))then
          call find_grnr(nsub, sublen, suboffset, subgrnr)
       endif

       ! Add the new particle properties
       ! Subgroups
       if(associated(sublen))then
          write(propname,'(1a,1i3.3)')"SubGroupIndex",icat
          res =  particle_store_new_property(pdata,species_name(ispecies), &
               propname, "INTEGER")
          if(.not.res%success)then
             deallocate(nr, fofgrnr, subgrnr, ids_idx)
             deallocate(groupids_idx)
             call particle_store_cleanup(pdata)
             group_catalogue_read = res
             call progress_bar_close()
             return
          endif
          res = particle_store_add_data(pdata, species_name(ispecies), &
               prop_name=propname, idata=subgrnr)
          if(.not.res%success)then
             deallocate(nr, fofgrnr, subgrnr, ids_idx)
             deallocate(groupids_idx)
             call particle_store_cleanup(pdata)
             group_catalogue_read = res
             call progress_bar_close()
             return
          endif
       endif
       
       ! FoF groups
       if(associated(foflen))then
          write(propname,'(1a,1i3.3)')"FoFGroupIndex",icat
          res =  particle_store_new_property(pdata,species_name(ispecies), &
               propname, "INTEGER")
          if(.not.res%success)then
             deallocate(nr, fofgrnr, subgrnr, ids_idx)
             deallocate(groupids_idx)
             call particle_store_cleanup(pdata)
             group_catalogue_read = res
             call progress_bar_close()
             return
          endif
          res = particle_store_add_data(pdata, species_name(ispecies), &
               prop_name=propname, idata=fofgrnr)
          if(.not.res%success)then
             deallocate(nr, fofgrnr, subgrnr, ids_idx)
             deallocate(groupids_idx)
             call particle_store_cleanup(pdata)
             group_catalogue_read = res
             call progress_bar_close()
             return
          endif
       endif

       deallocate(nr, fofgrnr, subgrnr, ids_idx)
    
       ! Next particle type
    end do
    
    call progress_bar_update(1.0)

    deallocate(groupids_idx)

    call particle_store_verify(pdata)   
    group_catalogue_read%success = .true.

    call progress_bar_close()

    return
    
  contains

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
