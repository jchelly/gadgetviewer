module gadget4_groups

#include "../../config.h"
  use f90_util
  use data_types
  use return_status
  use gadget_path
  use particle_store
#ifdef HAVE_HDF5
  use read_hdf5
#endif
  implicit none
  private
  save

  public :: gadget4_groups_read

contains
  
  type(result_type) function gadget4_groups_read(isnap, path_data, icat, rinfo) result(res)
    !
    ! Read Gadget-4 fof_subhalo_tab and label particles with fof/subhalo index
    !
    use partial_read_info
    implicit none
    ! Input parameters
    integer,               intent(in) :: isnap
    type (path_data_type), intent(in) :: path_data
    integer,               intent(in) :: icat
    type (read_info) :: rinfo
#ifdef HAVE_HDF5
    ! Group catalogue info
    character(len=fname_maxlen) :: tab_file
    integer :: ifile, nfiles
    integer(kind=int8byte) :: ngroups_total, nsubhalos_total
    integer(kind=int8byte) :: ngroups_file, nsubhalos_file
    integer(kind=int4byte), dimension(:,:), allocatable :: grouplen_type
    integer(kind=int4byte), dimension(:,:), allocatable :: subhalolen_type
    integer(kind=int4byte), dimension(:),   allocatable :: groupnsubs
    integer :: rank, ntypes
    integer(kind=int8byte), dimension(7) :: dims
    integer(kind=int8byte) :: group_offset, subhalo_offset
    integer :: dummy
    integer(kind=index_kind) :: np(maxspecies), offset, i, j, k, sub_offset, sub_index
    integer :: nspecies, ispecies
    integer(kind=i_prop_kind), dimension(:), allocatable :: grnr, subnr
    character(len=maxlen), dimension(maxspecies) :: species_name
    character(len=maxlen) :: propname

    ! Check we read the full snapshot
    if(.not.have_full_snapshot(rinfo))then
       res%success = .false.
       res%string  = "Can only read Gadget-4 halo finder output if the full snapshot was read in"
       call cleanup()
       return
    endif

    ! Read the file headers to determine total groups and number of files
    ifile = 0
    nfiles = 1
    group_offset = 0
    subhalo_offset = 0
    do while(ifile.lt.nfiles)

       ! Generate the path to the tab file
       call gadget_path_generate(isnap, ifile, tab_file, path_data)

       ! Open the file
       if(hdf5_open_file(tab_file).ne.0)then
          res%success = .false.
          res%string  = "Unable to open file: "//trim(tab_file)
          call cleanup()
          return   
       endif

       if(ifile.eq.0)then
          ! Read header on first iteration
          if(hdf5_read_attribute("Header/NumFiles", nfiles).ne.0)then
             res%success = .false.
             res%string  = "Unable to read Header/NumFiles"
             call cleanup()
             return
          endif
          if(hdf5_read_attribute("Header/Ngroups_Total", ngroups_total).ne.0)then
             res%success = .false.
             res%string  = "Unable to read Header/Ngroups_Total"
             call cleanup()
             return
          endif
          if(hdf5_read_attribute("Header/Nsubhalos_Total", nsubhalos_total).ne.0)then
             res%success = .false.
             res%string  = "Unable to read Header/Nsubhalos_Total"
             call cleanup()
             return
          endif

          ! Determine number of particle types
          if(hdf5_dataset_size("Group/GroupLenType", rank, dims).ne.0)then
             res%success = .false.
             res%string  = "Unable to get size of GroupLenType dataset"
             call cleanup()
             return          
          endif
          ntypes = dims(1)

          ! Allocate arrays
          allocate(grouplen_type(ntypes, ngroups_total))
          allocate(subhalolen_type(ntypes, nsubhalos_total))
          allocate(groupnsubs(ngroups_total))

       endif ! if (file_nr.eq.0)

       ! Find number of halos in this file
       if(hdf5_read_attribute("Header/Ngroups_ThisFile", ngroups_file).ne.0)then
          res%success = .false.
          res%string  = "Unable to read Header/Ngroups_ThisFile"
          call cleanup()
          return
       endif
       if(hdf5_read_attribute("Header/Nsubhalos_ThisFile", nsubhalos_file).ne.0)then
          res%success = .false.
          res%string  = "Unable to read Header/Nsubhalos_ThisFile"
          call cleanup()
          return
       endif
         
       ! Read arrays
       if(hdf5_read_dataset("Group/GroupLenType", &
            grouplen_type(:, group_offset+1:group_offset+ngroups_file)).ne.0)then
          res%success = .false.
          res%string  = "Unable to read Group/GroupLenType"
          call cleanup()
          return
       endif
       if(hdf5_read_dataset("Subhalo/SubhaloLenType", &
            subhalolen_type(:, subhalo_offset+1:subhalo_offset+nsubhalos_file)).ne.0)then
          res%success = .false.
          res%string  = "Unable to read Subhalo/SubhaloLenType"
          call cleanup()
          return
       endif
       if(hdf5_read_dataset("Group/GroupNsubs", &
            groupnsubs(group_offset+1:group_offset+ngroups_file)).ne.0)then
          res%success = .false.
          res%string  = "Unable to read Group/GroupNsubs"
          call cleanup()
          return
       endif

       group_offset = group_offset + ngroups_file
       subhalo_offset = subhalo_offset + nsubhalos_file       
       dummy = hdf5_close_file()
       ifile = ifile + 1
    end do ! Next file

    ! Now create arrays with halo membership for particles
    call particle_store_contents(pdata, get_nspecies=nspecies, get_np=np, &
         get_species_names=species_name)
    do ispecies = 1, nspecies, 1
       if(np(ispecies).gt.0.and.ispecies.le.ntypes)then
          allocate(grnr(np(ispecies)))
          allocate(subnr(np(ispecies)))
          grnr = -1
          subnr = -1

          ! Compute group membership for the particles
          offset = 0
          sub_index = 0
          do i = 1, ngroups_total, 1
             ! Subhalos
             sub_offset = offset
             do j = 1, groupnsubs(i), 1
                sub_index = sub_index + 1
                do k = 1, subhalolen_type(ispecies, sub_index), 1
                   sub_offset = sub_offset + 1
                   ! TODO: detect if we didn't read the full snapshot and generate a suitable error message
                   if(sub_offset.le.np(ispecies))subnr(sub_offset) = sub_index - 1
                end do
             end do
             ! Groups
             do j = 1, grouplen_type(ispecies, i), 1
                offset = offset + 1
                ! TODO: detect if we didn't read the full snapshot and generate a suitable error message
                if(offset.le.np(ispecies))grnr(offset) = i - 1
             end do
          end do
          ! Store group membership
          if(icat.gt.1)then
             write(propname,'(1a,1i3.3)')"GroupNr",icat
          else
             propname = "GroupNr"
          endif
          res = particle_store_new_property(pdata, species_name(ispecies), &
               propname, "INTEGER")
          if(.not.res%success)then
             call cleanup()
             return
          endif
          res = particle_store_add_data(pdata, species_name(ispecies), &
               prop_name=propname, idata=grnr)
          if(.not.res%success)then
             call cleanup()
             return
          endif

          ! Store subhalo membership
          if(icat.gt.1)then
             write(propname,'(1a,1i3.3)')"SubhaloNr",icat
          else
             propname = "SubhaloNr"
          endif
          res = particle_store_new_property(pdata, species_name(ispecies), &
               propname, "INTEGER")
          if(.not.res%success)then
             call cleanup()
             return
          endif
          res = particle_store_add_data(pdata, species_name(ispecies), &
               prop_name=propname, idata=subnr)
          if(.not.res%success)then
             call cleanup()
             return
          endif

          deallocate(grnr)
          deallocate(subnr)
       endif
    end do

    deallocate(grouplen_type)
    deallocate(subhalolen_type)
    deallocate(groupnsubs)
#else
    res%string = "Code was compiled without HDF5 support"
    res%success = .false.
#endif
    return
    
  contains

#ifdef HAVE_HDF5

    subroutine cleanup()
      
      implicit none

      if(allocated(grouplen_type))deallocate(grouplen_type)
      if(allocated(subhalolen_type))deallocate(subhalolen_type)
      if(allocated(grnr))deallocate(grnr)
      if(allocated(subnr))deallocate(subnr)
      if(allocated(groupnsubs))deallocate(groupnsubs)
      dummy = hdf5_close_file()

      return
    end subroutine cleanup

#endif

  end function gadget4_groups_read

end module gadget4_groups
