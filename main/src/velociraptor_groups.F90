module velociraptor_groups
!
! Read group catalogues output by Velociraptor halo finder
!
#include "../../config.h"
  use data_types
  use return_status
  use gadget_path
  use f90_util
#ifdef HAVE_HDF5
  use read_hdf5
#endif
  use string_module
  use particle_store

  implicit none
  private
  save

  public :: velociraptor_groups_read
  public :: velociraptor_groups_add_properties

  character(len=25), dimension(9) :: vr_file_type = (/&
       "catalog_groups           ", &
       "catalog_particles        ", &
       "catalog_particles.unbound", &
       "catalog_partypes         ", &
       "catalog_partypes.unbound ", &
       "catalog_SOlist           ", &
       "hierarchy                ", &
       "profiles                 ", &
       "properties               "/)

contains

  integer function velociraptor_filename(path_data, isnap, ifile, filetype, fname) result(res)
!
! Generate the path to a Velociraptor file given the name of any one file from the set.
!
    implicit none
    type (path_data_type) :: path_data
    integer               :: isnap, ifile
    character(len=*)      :: filetype, fname
    character(len=500)    :: tmp
    integer :: i

    call gadget_path_generate(isnap, ifile, tmp, path_data)
    do i = 1, size(vr_file_type), 1
       if(replace_string(tmp, trim(vr_file_type(i)), trim(filetype), fname, back=.true.).eq.0)then
          res = 0
          return
       endif
    end do
    
    res = 1
    return
  end function velociraptor_filename


  type (result_type) function velociraptor_groups_read(isnap, &
       path_data, nfof, nsub, nids, foflen, foffset, &
       sublen, suboffset, groupids, ID, hostHaloID) result(res)
!
! Read the specified group files
!
    implicit none
    ! Parameters
    integer, intent(in) :: isnap
    integer, intent(out) :: nfof, nsub, nids
    integer, dimension(:), allocatable :: foflen, foffset, sublen, suboffset
    integer(kind=i_prop_kind), dimension(:), allocatable :: groupids
    type (path_data_type) :: path_data
    integer(kind=i_prop_kind), dimension(:), allocatable :: ID, hostHaloID
#ifdef HAVE_HDF5
    ! Internal
    integer(kind=int8byte) :: igroup
    integer :: ifile, nfiles, hdferr
    character(len=500) :: catalog_groups_file
    character(len=500) :: catalog_particles_file
    character(len=500) :: properties_file
    ! Velociraptor file contents
    integer(kind=int8byte) :: num_of_files(1)
    integer(kind=int8byte) :: num_of_groups(1), total_num_of_groups(1)
    integer(kind=int8byte) :: num_of_particles_in_groups(1)
    integer(kind=int8byte) :: total_num_of_particles_in_all_groups(1)
    ! Offset into length/offset arrays
    integer(kind=int8byte) :: group_offset, id_offset

    ! Loop over catalog files in this output
    ifile = 0
    nfiles = 1
    group_offset = 0
    id_offset = 0
    do while(ifile.lt.nfiles)

       ! Find the catalog_groups file
       if(velociraptor_filename(path_data, isnap, ifile, "catalog_groups", catalog_groups_file).ne.0)then
          res%success = .false.
          res%string  = "Unable to determine name of catalog_groups file"
       endif

       ! Open the file and read the header
       if(hdf5_open_file(catalog_groups_file).ne.0)then
          res%success = .false.
          res%string  = "Unable to open file: "//trim(catalog_groups_file)
          call cleanup()
          return          
       endif
       if(hdf5_read_dataset("Num_of_files",  num_of_files).ne.0)then
          res%success = .false.
          res%string  = "Unable to read Num_of_files dataset"
          call cleanup()
          return
       endif
       nfiles = num_of_files(1)
       if(hdf5_read_dataset("Num_of_groups", num_of_groups).ne.0)then
          res%success = .false.
          res%string  = "Unable to read Num_of_groups dataset"
          call cleanup()
          return
       endif
       if(hdf5_read_dataset("Total_num_of_groups", total_num_of_groups).ne.0)then
          res%success = .false.
          res%string  = "Unable to read Total_num_of_groups dataset"
          call cleanup()
          return
       endif

       ! Allocate storage for lengths and offsets on first file
       if(ifile.eq.0)then
          allocate(suboffset(total_num_of_groups(1)))
       endif
       
       ! Read group offsets
       if(hdf5_read_dataset("Offset", suboffset(group_offset+1:group_offset+num_of_groups(1))).ne.0)then
          res%success = .false.
          res%string  = "Unable to read Offset dataset"
          call cleanup()
          return
       endif
       
       ! Close catalog_groups file
       hdferr = hdf5_close_file()

       ! Find the catalog_particles file
       if(velociraptor_filename(path_data, isnap, ifile, "catalog_particles", catalog_particles_file).ne.0)then
          res%success = .false.
          res%string  = "Unable to determine name of catalog_particles file"
       endif

       ! Open the file
       if(hdf5_open_file(catalog_particles_file).ne.0)then
          res%success = .false.
          res%string  = "Unable to open file: "//trim(catalog_particles_file)
          call cleanup()
          return          
       endif
       
       ! Read header
       if(hdf5_read_dataset("Num_of_particles_in_groups",  num_of_particles_in_groups).ne.0)then
          res%success = .false.
          res%string  = "Unable to read Num_of_particles_in_groups dataset"
          call cleanup()
          return
       endif
       if(hdf5_read_dataset("Total_num_of_particles_in_all_groups",  total_num_of_particles_in_all_groups).ne.0)then
          res%success = .false.
          res%string  = "Unable to read Total_num_of_particles_in_all_groups dataset"
          call cleanup()
          return
       endif
       
       ! Allocate storage for IDs
       if(ifile.eq.0)then
          allocate(groupids(total_num_of_particles_in_all_groups(1)))
       endif

       ! Read the IDs from this file
       if(hdf5_read_dataset("Particle_IDs", groupids(id_offset+1:id_offset+num_of_particles_in_groups(1))).ne.0)then
          res%success = .false.
          res%string  = "Unable to read Particle_IDs dataset"
          call cleanup()
          return
       endif

       ! Make offsets relative to the start of the first file and start at 1 rather than 0
       suboffset(group_offset+1:group_offset+num_of_groups(1)) = &
            suboffset(group_offset+1:group_offset+num_of_groups(1)) + id_offset + 1

       ! Close catalog_particles file
       hdferr = hdf5_close_file()

       ! Next file
       group_offset = group_offset + num_of_groups(1)
       id_offset    = id_offset    + num_of_particles_in_groups(1)
       ifile = ifile + 1
    end do

    ! Compute group sizes from offsets
    allocate(sublen(total_num_of_groups(1)))
    do igroup = 1, total_num_of_groups(1)-1, 1
       sublen(igroup) = suboffset(igroup+1) - suboffset(igroup)
    end do
    if(total_num_of_groups(1).gt.0)then
       sublen(total_num_of_groups(1)) = id_offset - suboffset(total_num_of_groups(1)) + 1
    endif

    ! Now read halo ID and host ID from properties file(s)
    group_offset = 0
    allocate(ID(total_num_of_groups(1)))
    allocate(hostHaloID(total_num_of_groups(1)))
    do ifile = 0, nfiles-1, 1

       ! Guess the name of the .properties file
       if(velociraptor_filename(path_data, isnap, ifile, "properties", properties_file).ne.0)then
          res%success = .false.
          res%string  = "Unable to determine name of catalog_particles file"
       endif

       ! Open the file
       if(hdf5_open_file(properties_file).ne.0)then
          call cleanup()
          res%success = .false.
          res%string  = "Unable to open file: "//trim(properties_file)
          return          
       endif
       ! Get number of groups
       if(hdf5_read_dataset("Num_of_groups", num_of_groups).ne.0)then
          call cleanup()
          res%success = .false.
          res%string  = "Unable to read Num_of_groups dataset"
          return
       endif
       ! Read the data
       if(hdf5_read_dataset("ID", ID(group_offset+1:group_offset+num_of_groups(1))).ne.0)then
          res%success = .false.
          res%string  = "Unable to read ID dataset"
          call cleanup()
          return
       endif
       if(hdf5_read_dataset("hostHaloID", hostHaloID(group_offset+1:group_offset+num_of_groups(1))).ne.0)then
          res%success = .false.
          res%string  = "Unable to read hostHaloID dataset"
          call cleanup()
          return
       endif

       ! Next file
       hdferr = hdf5_close_file()
       group_offset = group_offset + num_of_groups(1)       
    end do
    
    ! Set hostHaloID=ID for field halos
    do igroup = 1, total_num_of_groups(1), 1
       if(hostHaloID(igroup).lt.0)hostHaloID(igroup) = ID(igroup)
    end do

    ! Return number of groups etc
    nfof = 0
    nsub = total_num_of_groups(1)
    nids = total_num_of_particles_in_all_groups(1)

    res%success = .true.
    res%string  = ""

#else
    nfof = 0
    nsub = 0
    nids = 0
    res%success = .false.
    res%string  = "HDF5 support is required to read Velociraptor output"
#endif
    
    return

  contains

    subroutine cleanup()

      implicit none

      if(allocated(foflen))    deallocate(foflen)
      if(allocated(foffset))   deallocate(foffset)
      if(allocated(sublen))    deallocate(sublen)
      if(allocated(suboffset)) deallocate(suboffset)
      if(allocated(groupids))  deallocate(groupids)
      if(allocated(ID))        deallocate(ID)
      if(allocated(hostHaloID))deallocate(hostHaloID)

      return
    end subroutine cleanup

  end function velociraptor_groups_read


  type (result_type) function velociraptor_groups_add_properties(path_data, &
       isnap, ispecies, icat, subgrnr, ID, hostHaloID) result(res)

    implicit none
    ! Parameters
    type (path_data_type), intent(in) :: path_data
    integer,               intent(in) :: isnap, ispecies, icat
    integer(kind=i_prop_kind), dimension(:), allocatable :: subgrnr
    integer(kind=i_prop_kind), dimension(:), allocatable :: ID, hostHaloID
    ! Internal
    integer :: nspecies
    character(len=maxlen), dimension(maxspecies) :: species_name
    character(len=maxlen) :: propname
    integer :: ifile, nfiles, hdferr
    ! New property data
    integer(kind=index_kind) :: np, ipart
    integer(kind=i_prop_kind), dimension(:), allocatable :: iprop

    call particle_store_contents(pdata, get_nspecies=nspecies, &
         get_species_names=species_name)
    
    call particle_store_species(pdata, ispecies, get_np=np)
    allocate(iprop(np))

    ! Bounds check - subgrnr should be either 0 (no group) or in range 1-ngroups
    do ipart = 1, np, 1
       if(subgrnr(ipart).gt.size(ID))call terminate("VR group index out of range (1)!")
       if(subgrnr(ipart).lt.0)       call terminate("VR group index out of range (2)!")
    end do
    
    ! Look up VR halo ID for each particle
    iprop(:) = -1
    do ipart = 1, np, 1
       if(subgrnr(ipart).gt.0)iprop(ipart) = ID(subgrnr(ipart))
    end do
    if(icat.gt.1)then
       write(propname,'(1a,1i3.3)')"VR_ID_bound",icat
    else
       propname = "VR_ID_bound"
    endif
    res =  particle_store_new_property(pdata,species_name(ispecies), &
         propname, "INTEGER")
    if(res%success)res = particle_store_add_data(pdata, species_name(ispecies), &
         prop_name=propname, idata=iprop)
    if(.not.res%success)then
       deallocate(iprop)
       return
    endif

    ! Look up VR host halo ID for each particle
    iprop(:) = -1
    do ipart = 1, np, 1
       if(subgrnr(ipart).gt.0)iprop(ipart) = hostHaloID(subgrnr(ipart))
    end do
    if(icat.gt.1)then
       write(propname,'(1a,1i3.3)')"VR_hostHaloID_bound",icat
    else
       propname = "VR_hostHaloID_bound"
    endif
    res =  particle_store_new_property(pdata,species_name(ispecies), &
         propname, "INTEGER")
    if(res%success)res = particle_store_add_data(pdata, species_name(ispecies), &
         prop_name=propname, idata=iprop)
    if(.not.res%success)then
       deallocate(iprop)
       return
    endif

    deallocate(iprop)
    res%success = .true.
    res%string  = ""

    return

  end function velociraptor_groups_add_properties
  
end module velociraptor_groups
