module velociraptor_groups
!
! Read group catalogues output by Velociraptor halo finder
!
  use data_types
  use return_status
  use gadget_path
  use f90_util
  use read_hdf5
  use string_module
  use particle_store

  implicit none
  private
  save

  public :: velociraptor_groups_read
  public :: velociraptor_groups_add_properties

contains

  type (result_type) function velociraptor_groups_read(isnap, &
       path_data, nfof, nsub, nids, foflen, foffset, &
       sublen, suboffset, groupids)
!
! Read the specified group files
!
    implicit none
    ! Parameters
    integer, intent(in) :: isnap
    integer, intent(out) :: nfof, nsub, nids
    integer, dimension(:), pointer :: foflen, foffset, sublen, suboffset
    integer(kind=i_prop_kind), dimension(:), pointer :: groupids
    type (path_data_type) :: path_data
    ! Internal
    integer(kind=int8byte) :: igroup
    integer :: ifile, nfiles, hdferr
    character(len=500) :: catalog_groups_file
    character(len=500) :: catalog_particles_file
    ! Velociraptor file contents
    integer(kind=int8byte) :: num_of_files(1)
    integer(kind=int8byte) :: num_of_groups(1), total_num_of_groups(1)
    integer(kind=int8byte) :: num_of_particles_in_groups(1)
    integer(kind=int8byte) :: total_num_of_particles_in_all_groups(1)
    ! Offset into length/offset arrays
    integer(kind=int8byte) :: group_offset, id_offset

    ! Nullify input pointers
    nullify(foflen, foffset, sublen, suboffset, groupids)

    ! Loop over catalog files in this output
    nfiles = 1
    group_offset = 0
    id_offset = 0
    do while(ifile.lt.nfiles)
       
       ! Read the .catalog_groups file header
       call gadget_path_generate(isnap, ifile, catalog_groups_file, path_data)
       if(hdf5_open_file(catalog_groups_file).ne.0)then
          velociraptor_groups_read%success = .false.
          velociraptor_groups_read%string  = "Unable to open file: "//trim(catalog_groups_file)
          call cleanup()
          return          
       endif
       if(hdf5_read_dataset("Num_of_files",  num_of_files).ne.0)then
          velociraptor_groups_read%success = .false.
          velociraptor_groups_read%string  = "Unable to read Num_of_files dataset"
          call cleanup()
          return
       endif
       nfiles = num_of_files(1)
       if(hdf5_read_dataset("Num_of_groups", num_of_groups).ne.0)then
          velociraptor_groups_read%success = .false.
          velociraptor_groups_read%string  = "Unable to read Num_of_groups dataset"
          call cleanup()
          return
       endif
       if(hdf5_read_dataset("Total_num_of_groups", total_num_of_groups).ne.0)then
          velociraptor_groups_read%success = .false.
          velociraptor_groups_read%string  = "Unable to read Total_num_of_groups dataset"
          call cleanup()
          return
       endif

       ! Allocate storage for lengths and offsets on first file
       if(ifile.eq.0)then
          allocate(sublen(total_num_of_groups(1)))
          allocate(suboffset(total_num_of_groups(1)))
       endif
       
       ! Read group offsets
       if(hdf5_read_dataset("Offset", suboffset(group_offset+1:group_offset+num_of_groups(1))).ne.0)then
          velociraptor_groups_read%success = .false.
          velociraptor_groups_read%string  = "Unable to read Offset dataset"
          call cleanup()
          return
       endif
       
       ! Close catalog_groups file
       hdferr = hdf5_close_file()

       ! Guess the name of the .catalog_particles file
       if(replace_string(catalog_groups_file, ".catalog_groups", ".catalog_particles", &
            catalog_particles_file, back=.true.).ne.0)then
          velociraptor_groups_read%success = .false.
          velociraptor_groups_read%string  = "Can't guess .catalog_particles file name from "//trim(catalog_groups_file)
          return
       endif

       ! Open the file
       if(hdf5_open_file(catalog_particles_file).ne.0)then
          velociraptor_groups_read%success = .false.
          velociraptor_groups_read%string  = "Unable to open file: "//trim(catalog_particles_file)
          call cleanup()
          return          
       endif
       
       ! Read header
       if(hdf5_read_dataset("Num_of_particles_in_groups",  num_of_particles_in_groups).ne.0)then
          velociraptor_groups_read%success = .false.
          velociraptor_groups_read%string  = "Unable to read Num_of_particles_in_groups dataset"
          call cleanup()
          return
       endif
       if(hdf5_read_dataset("Total_num_of_particles_in_all_groups",  total_num_of_particles_in_all_groups).ne.0)then
          velociraptor_groups_read%success = .false.
          velociraptor_groups_read%string  = "Unable to read Total_num_of_particles_in_all_groups dataset"
          call cleanup()
          return
       endif
       
       ! Allocate storage for IDs
       if(ifile.eq.0)then
          allocate(groupids(total_num_of_particles_in_all_groups(1)))
       endif

       ! Read the IDs from this file
       if(hdf5_read_dataset("Particle_IDs", groupids(id_offset+1:id_offset+num_of_particles_in_groups(1))).ne.0)then
          velociraptor_groups_read%success = .false.
          velociraptor_groups_read%string  = "Unable to read Particle_IDs dataset"
          call cleanup()
          return
       endif

       ! Make offsets relative to the start of the first file and start at 1 rather than 0
       suboffset(group_offset+1:group_offset+num_of_groups(1)) = &
            suboffset(group_offset+1:group_offset+num_of_groups(1)) + id_offset + 1

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

    ! Return number of groups etc
    nfof = 0
    nsub = total_num_of_groups(1)
    nids = total_num_of_particles_in_all_groups(1)

    velociraptor_groups_read%success = .true.
    velociraptor_groups_read%string  = ""
    
    return

  contains

    subroutine cleanup()

      implicit none

      if(associated(foflen))    deallocate(foflen)
      if(associated(foffset))   deallocate(foffset)
      if(associated(sublen))    deallocate(sublen)
      if(associated(suboffset)) deallocate(suboffset)
      if(associated(groupids))  deallocate(groupids)

      return
    end subroutine cleanup

  end function velociraptor_groups_read


  type (result_type) function velociraptor_groups_add_properties(path_data, &
       isnap, ispecies, icat, subgrnr) result(res)

    implicit none
    ! Parameters
    type (path_data_type), intent(in) :: path_data
    integer,               intent(in) :: isnap, ispecies, icat
    integer(kind=i_prop_kind), dimension(:), pointer :: subgrnr
    ! Internal
    integer :: nspecies
    character(len=maxlen), dimension(maxspecies) :: species_name
    character(len=maxlen) :: propname
    integer :: ifile, nfiles, hdferr
    ! ! Filenames
    ! character(len=500) :: catalog_groups_file
    ! character(len=500) :: properties_file
    ! ! Velociraptor file contents
    ! integer(kind=int8byte) :: num_of_files(1)
    ! integer(kind=int8byte) :: num_of_groups(1), total_num_of_groups(1)
    ! ! Offset into length/offset arrays
    ! integer(kind=int8byte) :: group_offset
    ! ! Properties to read
    ! integer(kind=i_prop_kind), dimension(:), allocatable :: ID, hostHaloID
    ! ! New particle property array
    ! integer(kind=i_prop_kind), dimension(:), allocatable :: part_prop

    call particle_store_contents(pdata, get_nspecies=nspecies, &
         get_species_names=species_name)

    ! Add the group index as a new particle property
    if(associated(subgrnr))then
       write(propname,'(1a,1i3.3)')"VR_HaloIndex",icat
       res =  particle_store_new_property(pdata,species_name(ispecies), &
            propname, "INTEGER")
       if(.not.res%success)return
       res = particle_store_add_data(pdata, species_name(ispecies), &
            prop_name=propname, idata=subgrnr)
       if(.not.res%success)return
    endif
    
    ! NOTE: this doesn't work because we don't want to read all files for each species

    ! ! Read the .properties file(s) and add some more particle properties
    ! nfiles = 1
    ! group_offset = 0
    ! do while(ifile.lt.nfiles)

    !    ! Guess the name of the .properties file
    !    call gadget_path_generate(isnap, ifile, catalog_groups_file, path_data)
    !    if(replace_string(catalog_groups_file, ".catalog_groups", ".properties", &
    !         properties_file, back=.true.).ne.0)then
    !       call cleanup()
    !       res%success = .false.
    !       res%string  = "Can't guess .properties file name from "//trim(catalog_groups_file)
    !       return
    !    endif

    !    ! Open the file
    !    if(hdf5_open_file(properties_file).ne.0)then
    !       call cleanup()
    !       res%success = .false.
    !       res%string  = "Unable to open file: "//trim(properties_file)
    !       return          
    !    endif

    !    ! Get number of files, groups etc
    !    if(hdf5_read_dataset("Num_of_files",  num_of_files).ne.0)then
    !       call cleanup()
    !       res%success = .false.
    !       res%string  = "Unable to read Num_of_files dataset"
    !       return
    !    endif
    !    nfiles = num_of_files(1)
    !    if(hdf5_read_dataset("Num_of_groups", num_of_groups).ne.0)then
    !       call cleanup()
    !       res%success = .false.
    !       res%string  = "Unable to read Num_of_groups dataset"
    !       return
    !    endif
    !    if(hdf5_read_dataset("Total_num_of_groups", total_num_of_groups).ne.0)then
    !       call cleanup()
    !       res%success = .false.
    !       res%string  = "Unable to read Total_num_of_groups dataset"
    !       return
    !    endif

    !    ! On reading first file, allocate storage
    !    if(ifile.eq.0)then
    !       allocate(ID(total_num_of_groups(1)))
    !       allocate(hostHaloID(total_num_of_groups(1)))
    !    endif

    !    ! Read the data
    !    if(hdf5_read_dataset("ID", ID(group_offset+1:group_offset+num_of_groups(1))).ne.0)then
    !       res%success = .false.
    !       res%string  = "Unable to read ID dataset"
    !       call cleanup()
    !       return
    !    endif
    !    if(hdf5_read_dataset("hostHaloID", hostHaloID(group_offset+1:group_offset+num_of_groups(1))).ne.0)then
    !       res%success = .false.
    !       res%string  = "Unable to read ID dataset"
    !       call cleanup()
    !       return
    !    endif

    !    ! Next file
    !    hdferr = hdf5_close_file()
    !    group_offset = group_offset + num_of_groups(1)
    !    ifile = ifile + 1
    ! end do



    ! deallocate(ID, hostHaloID)
    res%success = .true.
    res%string  = ""

    return

  ! contains

  !   subroutine cleanup()

  !     implicit none

  !     if(allocated(ID))deallocate(ID)
  !     if(allocated(hostHaloID))deallocate(hostHaloID)

  !     return
  !   end subroutine cleanup

  end function velociraptor_groups_add_properties
  
end module velociraptor_groups
