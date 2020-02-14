module gadget_hdf5_reader
!
! Module to read Gadget_hdf5 files. Should also read GIMIC outputs.
!
#include "../../config.h"
#ifdef HAVE_HDF5
  use read_hdf5
#endif
  use data_types
  use byteswapper
  use return_status
  use particle_store
  use file_units
  use progress_bar
  use string_module
  use gadget_path
  use summary
  use key_file
  use partial_read_info
  use array_shrinker
  use f90_util
  use sort

  implicit none
  private
  save

  public :: gadget_hdf5_open
  public :: gadget_hdf5_read
  public :: gadget_hdf5_read_conf

  ! Simulation details
  integer :: nfiles

  ! Extra properties defined in the configuration file
  integer :: nextra_config = 0
  integer, parameter :: max_extra = 100
  character(len=maxlen), dimension(max_extra) :: extra_prop_config

  ! Extra properties from config+command line
  integer :: nextra_all
  character(len=maxlen), dimension(max_extra) :: extra_prop_all

  ! Which properties exist in the current snapshot
  logical,           dimension(maxspecies,max_extra) :: read_extra
  character(len=20), dimension(maxspecies,max_extra) :: extra_type
  integer,           dimension(maxspecies,max_extra) :: extra_bytes

  ! Path information extracted from file name
  type (path_data_type) :: path_data

  ! Which particle types to read
  logical, dimension(maxspecies) :: read_type

  ! Possible names of the Mass and ID datasets
  character(len=maxlen), dimension(1) :: id_datasets   = (/"ParticleIDs"/)
  character(len=maxlen), dimension(3) :: mass_datasets = (/&
       "Mass           ",&
       "Masses         ",&
       "DynamicalMasses"/)

contains

  subroutine sanitize_property_list(nextra, extra_prop)
!
! Ensure the extra properties list contains only unique elements
!
    implicit none
    integer :: nextra
    character(len=*), dimension(:) :: extra_prop
    integer :: iprop, jprop
    integer, dimension(:), allocatable :: sortidx

    ! Remove any duplicate property names
    allocate(sortidx(nextra))
    call sort_index(extra_prop(1:nextra), sortidx)
    do iprop = 2, nextra, 1
       if(extra_prop(sortidx(iprop)).eq.extra_prop(sortidx(iprop-1)))then
          extra_prop(sortidx(iprop)) = ""
       endif
    end do
    deallocate(sortidx)

    ! Only keep unique entries
    jprop = 0
    do iprop = 1, nextra, 1
       ! Check if this is a duplicate
       if(len_trim(extra_prop(iprop)).eq.0)cycle
       ! Keep this one
       jprop = jprop + 1
       extra_prop(jprop) = extra_prop(iprop)   
    end do
    nextra = jprop

    return
  end subroutine sanitize_property_list


  subroutine gadget_hdf5_read_conf(dir)
!
! Read in the configuration file, or create a default one if it doesn't
! exist
!
    implicit none
    character(len=*)   :: dir
    character(len=500) :: fname
    integer            :: ios
    character(len=500) :: str
    integer            :: i
    logical            :: is_duplicate
    integer :: iprop

    nextra_config = 0
    fname = trim(dir)//"/"//"gadget_hdf5_extra_properties"
    call read_key_file(fname)
    if(.not.file_has_group("Gadget HDF5"))then
       ! Default settings
       ! Common names in Gadget snapshots
       call add_extra("Metallicity")
       call add_extra("StarFormationRate")
       call add_extra("Temperature")
       call add_extra("Density")
       call add_extra("InternalEnergy")
       ! Common names in Swift-EAGLE snapshots
       ! All
       call add_extra("FOFGroupIDs")
       ! Gas
       call add_extra("MetalMassFractions")
       call add_extra("StarFormationRates")
       call add_extra("Temperatures")
       call add_extra("Densities")
       call add_extra("InternalEnergies")
       ! Stars
       call add_extra("BirthScaleFactors")
       call add_extra("BirthDensities")
       ! Black holes
       call add_extra("SubgridMasses")
       ! Save the new list
       call set_key("Gadget HDF5","Extra Properties", extra_prop_config(1:nextra_config)) 
       call write_key_file(fname)
    endif
    if(file_has_group("Gadget HDF5"))then
       extra_prop_config(:) = ""
       call get_key("Gadget HDF5","Extra Properties", extra_prop_config)
       do i = 1, max_extra, 1
          if(len_trim(extra_prop_config(i)).gt.0)nextra_config = nextra_config + 1
       end do
    endif
    call close_key_file()
    call sanitize_property_list(nextra_config, extra_prop_config)
    
    return

  contains 
      
    subroutine add_extra(name)
        
      character(len=*), intent(in) :: name
      
      if(nextra_config.lt.max_extra)then
         nextra_config = nextra_config + 1
         extra_prop_config(nextra_config) = trim(name)
      endif
      
    end subroutine add_extra

  end subroutine gadget_hdf5_read_conf

!
! Scan a simulation directory and store information
! required for reading snapshots
!
  type(result_type) function gadget_hdf5_open(fname, isnap, rinfo)

    implicit none
    ! Parameters
    character(len=*) :: fname
    integer, intent(out) :: isnap
    type (read_info) :: rinfo
#ifdef HAVE_HDF5
    ! Internal
    integer :: jsnap, ios, ifile
    logical :: fexist
    integer :: n, i1, i2
    integer :: ispecies, iextra
    character(len=maxlen) :: str
    ! Checking for datasets
    integer(kind=int8byte), dimension(7) :: npfile
    integer(kind=int8byte), dimension(7) :: nptot, nptot_hw
    logical,                dimension(7) :: find_type
    ! HDF5 stuff
    integer           :: hdferr, err_array(10)
    type(result_type) :: res
    integer           :: dtype
    integer           :: rank
    integer(kind=int8byte), dimension(10) :: dims

    gadget_hdf5_open%success = .false.

    ! Check we haven't been asked to use spatial indexing
    if(rinfo%use_index)then
       gadget_hdf5_open%string  = &
            "No spatial indexing in Gadget HDF5 snapshot"
       return
    endif

    ! Check if the file exists
    inquire(file=fname,exist=fexist,iostat=ios)
    if(ios.eq.0)then
       if(.not.fexist)then
          gadget_hdf5_open%success = .false.
          gadget_hdf5_open%string  = "File does not exist"
          return
       endif
    else
       gadget_hdf5_open%success = .false.
       gadget_hdf5_open%string  = "Unable to access file" 
       return
    endif

    ! See if we can open it with HDF5
    hdferr = hdf5_open_file(fname)
    if(hdferr.ne.0)then
       gadget_hdf5_open%string="Unable to open HDF5 file: "//trim(fname)
       return
    endif

    ! Read the number of files
    hdferr = hdf5_read_attribute("/Header/NumFilesPerSnapshot", n)
    if(hdferr.ne.0)then
       gadget_hdf5_open%string= &
            "Unable to read NumFilesPerSnapshot from file: "//trim(fname)
       return
    endif
    
    ! Close the file
    hdferr = hdf5_close_file()

    ! Extract information from the path so we can try to infer where the
    ! other snapshots will be
    res = gadget_path_extract(fname, jsnap, path_data, eagle_snapshot=.true.)
    if(.not.res%success)then
       ! fname is somehow messed up enough that we can't interpret it,
       ! despite it being a valid filename
       gadget_hdf5_open = res
       isnap = -1
       return
    endif

    ! Make full list of properties to try to read
    ! Start with things we always read
    nextra_all = 0
    do i1 = 1, size(id_datasets), 1
       call add_extra(id_datasets(i1))
    end do
    do i1 = 1, size(mass_datasets), 1
       call add_extra(mass_datasets(i1))
    end do
    ! Add list from config file
    do i1 = 1, nextra_config, 1
       call add_extra(extra_prop_config(i1))
    end do

    ! Then add any specified on the command line
    i1 = 1
    do while(i1.le.len_trim(rinfo%extra_dataset_names))
       i2 = index(rinfo%extra_dataset_names(i1:), ",")
       if(i2.lt.1)then
          i2 = len_trim(rinfo%extra_dataset_names) + 1
       else
          i2 = i2 + i1 - 1
       endif
       if(i2.gt.i1)then
          call add_extra(trim(adjustl(rinfo%extra_dataset_names(i1:i2-1))))
       endif
       i1 = max(i1+1, i2+1)
    end do

    ! Remove any duplicate property names
    call sanitize_property_list(nextra_all, extra_prop_all)

    ! Check which of the extra properties exist in this snapshot
    !
    ! Strategy is to keep opening snapshot files until we find at
    ! least one particle of each type that is present in the full
    ! snapshot.
    !
    read_extra = .false.
    find_type  = .true.
    read_type  = .false.
    do ifile = 0, n-1, 1
       call gadget_path_generate(jsnap, ifile, fname, path_data)
       hdferr = hdf5_open_file(fname)
       if(hdferr.ne.0)then
          gadget_hdf5_open%string= &
               "Unable to open file: "//trim(fname)
          return
       endif

       ! Read number of particles in this file
       npfile = 0
       hdferr = hdf5_read_attribute("/Header/NumPart_ThisFile", npfile)
       if(hdferr.ne.0)then
          gadget_hdf5_open%string="Unable to read NumPart_ThisFile from file"
          hdferr = hdf5_close_file()
          return
       endif

       ! If this is a multi file snapshot, read total number of particles
       if(n.gt.1)then
          nptot = 0
          hdferr = hdf5_read_attribute("/Header/NumPart_Total", nptot)
          if(hdferr.ne.0)then
             gadget_hdf5_open%string="Unable to read NumPart_Total from file"
             hdferr = hdf5_close_file()
             return
          endif
          nptot_hw = 0
          hdferr = hdf5_read_attribute("/Header/NumPart_Total_HighWord", nptot_hw)
          if(hdferr.ne.0)then
             ! Not all Gadget files have this dataset
             nptot_hw = 0
          endif
          nptot = nptot + (2_int8byte**32)*nptot_hw
       else
          ! This is a single file snapshot
          nptot = npfile
       endif

       do ispecies = 1, 6, 1
          
          if(npfile(ispecies).gt.0.and.find_type(ispecies))then

             ! Check if this particle type has a coordinates dataset
             str = "/PartType"//trim(string(ispecies-1,fmt='(i1.1)'))//"/Coordinates"
             hdferr = hdf5_dataset_type(str, dtype)
             if(hdferr.eq.0)read_type(ispecies) = .true.

             ! Check for other quantities
             do iextra = 1, nextra_all, 1
                ! Assume we're not reading this until we know otherwise
                read_extra(ispecies,iextra) = .false.
                extra_type(ispecies,iextra) = "UNKNOWN"
                ! Construct name of this dataset
                str = "/PartType"//trim(string(ispecies-1,fmt='(i1.1)'))//"/"// &
                     trim(extra_prop_all(iextra))
                ! Get the type of the dataset
                hdferr = hdf5_dataset_type(str, dtype)
                if(hdferr.ne.0)cycle
                ! Get the size of the dataset
                hdferr = hdf5_dataset_size(str, rank, dims)
                if(hdferr.ne.0)cycle
                if(rank.ne.1.or.dims(1).ne.npfile(ispecies))cycle
                select case(dtype)
                case(HDF5_INTEGER4)
                   ! Integer dataset
                   read_extra (ispecies,iextra) = .true.
                   extra_type (ispecies,iextra) = "INTEGER"
                   extra_bytes(ispecies,iextra) = 4
                case(HDF5_INTEGER8)
                   ! Integer dataset
                   read_extra (ispecies,iextra) = .true.
                   extra_type (ispecies,iextra) = "INTEGER"
                   extra_bytes(ispecies,iextra) = 8
                case(HDF5_REAL4)
                   ! Real dataset
                   read_extra(ispecies,iextra) = .true.
                   extra_type(ispecies,iextra) = "REAL"
                   extra_bytes(ispecies,iextra) = 4
                case(HDF5_REAL8)
                   ! Real dataset
                   read_extra(ispecies,iextra) = .true.
                   extra_type(ispecies,iextra) = "REAL"
                   extra_bytes(ispecies,iextra) = 8
                case default
                   ! Don't read if it's not real/integer
                   cycle
                end select
             end do

          endif

          ! Determine if we need to keep looking for a file with
          ! particles of this type - can stop if a) there aren't
          ! any in the snapshot or b) there ARE some in this file.
          if(nptot(ispecies).eq.0)find_type(ispecies)=.false.
          if(npfile(ispecies).gt.0)find_type(ispecies)=.false.

       end do
       hdferr = hdf5_close_file()

       ! If we've found at least one file with particles of each type
       ! present in the snapshot, can stop looking
       if(all(.not.find_type))exit
    end do

    ! Looks ok - store the details and return snapshot number
    nfiles        = max(1,n)
    isnap         = jsnap
  
    ! If we get this far, everything's ok
    gadget_hdf5_open%success = .true.

#else
    ! Should never get here because menu option is greyed out if HDF5 not
    ! available
    call terminate('gadget_hdf5_reader - Code was compiled without HDF5 support')

    ! Stop compiler complaining about return value not being set
    gadget_hdf5_open%success = .false.
    gadget_hdf5_open%string  = "Compiled without HDF5 support"
    isnap = 0
#endif
    return

  contains 
      
    subroutine add_extra(name)
      
      character(len=*), intent(in) :: name
        
      if(nextra_all.lt.max_extra)then
         nextra_all = nextra_all + 1
         extra_prop_all(nextra_all) = trim(name)
      endif
      
    end subroutine add_extra
      
  end function gadget_hdf5_open

!
! Read the specified snapshot, if possible
!
  type (result_type) function gadget_hdf5_read(isnap, rinfo)

    implicit none
    ! Parameters
    integer :: isnap
    type (read_info) :: rinfo
#ifdef HAVE_HDF5
    ! Internal
    integer(kind=index_kind) :: ipart
    integer :: ifile, i, j
    integer(kind=int8byte), dimension(7) :: nptot, nptot_hw
    integer(kind=int8byte), dimension(7) :: npfile
    character(len=fname_maxlen) :: fname
    character(len=50), dimension(6) :: species_name
    real, dimension(7) :: massarr
    integer :: istat
    real(kind=real8byte) :: boxsize, boxsize_array(3)
    integer :: num_dimensions
    ! Temporary storage for particles
    real(kind=pos_kind),       dimension(:,:), allocatable :: pos
#ifdef READ_VEL
    real(kind=vel_kind),       dimension(:,:), allocatable :: vel
#endif
    real(kind=r_prop_kind),    dimension(:),   allocatable :: mass
    integer(kind=i_prop_kind), dimension(:),   allocatable :: id
    ! Result from function calls
    type (result_type) :: res
    ! HDF5 stuff
    integer :: hdferr, err_array(10)
    character(len=100) :: gname
    integer :: dtype
    ! Progress bar
    integer(kind=int8byte) :: prog_tot, prog_so_far
    integer :: ispecies, iextra
    ! Range of files to read
    integer :: firstfile, lastfile
    ! Which particles to read
    logical, dimension(:), allocatable :: mask
    integer(kind=index_kind) :: offset, nkeep
    real :: rnd
    ! Whether each type has a mass dataset
    logical, dimension(6) :: have_mass
    ! Temp. storage
    real(kind=r_prop_kind),    dimension(:), allocatable :: rdata
    integer(kind=i_prop_kind), dimension(:), allocatable :: idata
    character(len=500) :: str, time_unit
    real :: redshift, time, expansion
    logical :: is_id, is_mass, found_id, found_mass

    gadget_hdf5_read%success = .false.

    ! Check we haven't been asked to use spatial indexing
    if(rinfo%use_index)then
       gadget_hdf5_read%success = .false.
       gadget_hdf5_read%string  = &
            "No spatial indexing in Gadget HDF5 snapshot"
       return
    endif

    call progress_bar_update(0.0)

    ! Read one file to get the total particle number and redshift
    call gadget_path_generate(isnap, 0, fname, path_data)

    hdferr = hdf5_open_file(fname)
    if(hdferr.ne.0)then
       gadget_hdf5_read%string="Unable to open HDF5 file: "//trim(fname)
       return
    endif
    
    ! Read total particle number
    nptot = 0
    hdferr = hdf5_read_attribute("/Header/NumPart_Total", nptot)
    if(hdferr.ne.0)then
       gadget_hdf5_read%string="Unable to read NumPart_Total from file"
       return
    endif
    nptot_hw = 0
    hdferr = hdf5_read_attribute("/Header/NumPart_Total_HighWord", nptot_hw)
    if(hdferr.ne.0)then
       nptot_hw = 0
    endif
    nptot = nptot + (2_int8byte**32)*nptot_hw

    ! Read redshift
    hdferr = hdf5_read_attribute("/Header/Redshift", redshift)
    if(hdferr.ne.0)then
       redshift = -1.0
    endif

    ! Check number of dimensions
    hdferr = hdf5_read_attribute("/Header/Dimension", num_dimensions)
    if(hdferr.eq.0)then
       if(num_dimensions.ne.3)then
          gadget_hdf5_read%string="Can only handle three dimensional outputs!"
          return
       endif
    endif

    ! Read box size:
    ! this is a scalar in Gadget but a 3 element array in Swift
    hdferr = hdf5_read_attribute("/Header/BoxSize", boxsize_array)
    if(hdferr.ne.0)then
       ! Try reading it as a scalar
       hdferr = hdf5_read_attribute("/Header/BoxSize", boxsize)
       if(hdferr.ne.0)then
          ! No box size, so assume non-periodic simulation
          boxsize = 0.0
       endif
    else
       ! Have a 3D box size - just use the largest dimension
       boxsize = maxval(boxsize_array)
    endif
    write(0,*)"Boxsize = ", boxsize
    
    ! Read time - prefer Time_GYR if present
    hdferr = hdf5_read_attribute("/Header/Time_GYR", time)
    if(hdferr.ne.0)then
       hdferr = hdf5_read_attribute("/Header/Time", time)
       if(hdferr.ne.0)then
          time = -1.0
       endif
       time_unit = ""
    else
       time_unit = "Gyr"
    endif

    ! Read expansion factor
    hdferr = hdf5_read_attribute("/Header/ExpansionFactor", expansion)
    if(hdferr.ne.0)then
       expansion = -1.0
    endif

    hdferr = hdf5_close_file()

    ! Set number of particles to zero for types we're not reading
    do ispecies = 1, 6, 1
       if(.not.read_type(ispecies))then
          npfile(ispecies) = 0
          nptot(ispecies) = 0
       endif
    end do

    ! At this point we know the file is readable, so deallocate the old
    ! snapshot
    call particle_store_empty(pdata)
    call particle_store_empty(psample)

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
               int(nptot(i), index_kind))
       endif
       ! Abort if memory allocation fails
       if(.not.res%success)then
          gadget_hdf5_read = res ! Pass back return status
          call particle_store_empty(pdata) ! Clean up any memory allocated
          return
       endif
    end do

    ! Assume no masses initially
    have_mass = .false.

    ! Allocate storage for any extra properties
    do ispecies = 1, 6, 1
       found_id   = .false.
       found_mass = .false.
       do iextra = 1, nextra_all, 1
          if(read_extra(ispecies,iextra))then
             ! Check if this is the ID or Mass dataset
             is_id   = .false.
             is_mass = .false.
             do i = 1, size(id_datasets), 1
                if(.not.found_id)then
                   if(extra_prop_all(iextra).eq.id_datasets(i))then
                      is_id    = .true.
                      found_id = .true.
                   endif
                endif
             end do
             do i = 1, size(mass_datasets), 1
                if(.not.found_mass)then
                   if(extra_prop_all(iextra).eq.mass_datasets(i))then
                      have_mass(ispecies) = .true.
                      is_mass    = .true.
                      found_mass = .true.
                   endif
                endif
             end do
             ! Allocate
             res = particle_store_new_property(pdata,species_name(ispecies),&
                  extra_prop_all(iextra), extra_type(ispecies,iextra), &
                  is_mass=is_mass, is_id=is_id)
             if(.not.res%success)then
                gadget_hdf5_read = res
                call particle_store_empty(pdata)
                return
             endif
             if(is_id)then
                ! Store number of bytes per ID
                call particle_store_set_idsize(pdata, extra_bytes(ispecies,iextra))
             endif
          endif
       end do
    end do

    ! Work out values for progress bar
    prog_tot    = sum(nptot(1:6))*2
    prog_so_far = 0

    ! Loop over files and read them
    if(rinfo%just_this_file)then
       firstfile = path_data%fileno
       lastfile  = path_data%fileno
    else
       firstfile = 0
       lastfile = nfiles-1
    endif

    do ifile = firstfile, lastfile, 1

       ! Generate filename
       call gadget_path_generate(isnap, ifile, fname, &
            path_data)

       ! Open file
       hdferr = hdf5_open_file(fname)
       if(hdferr.ne.0)then
          gadget_hdf5_read%string="Unable to open file: "//trim(fname)
          call particle_store_empty(pdata)
          return
       endif
       
       ! Read header
       npfile = 0
       hdferr = hdf5_read_attribute("/Header/NumPart_ThisFile", npfile)
       if(hdferr.ne.0)then
          gadget_hdf5_read%string="Unable to read NumPart_Total from file"
          hdferr = hdf5_close_file()
          return
       endif
       hdferr = hdf5_read_attribute("/Header/MassTable", massarr)
       if(hdferr.ne.0)then
          massarr = 0.0
       endif

       ! Create mass datasets for particles with no individual masses
       if(ifile.eq.firstfile)then
          do ispecies = 1, 6, 1
             if(.not.have_mass(ispecies))then
                res = particle_store_new_property(pdata,species_name(ispecies),&
                     mass_datasets(1), "REAL", is_mass=.true.)
             endif
          end do
       endif

       ! Set number of particles to zero for types we're not reading
       do ispecies = 1, 6, 1
          if(.not.read_type(ispecies))then
             npfile(ispecies) = 0
          endif
       end do

       ! Set up mask array for sampling
       if(rinfo%do_sampling.or.rinfo%do_sphere)then
          allocate(mask(sum(npfile)))
          mask = .true.
          if(rinfo%do_sampling)then
             do ipart = 1, sum(npfile), 1
                call random_number(rnd)
                if(rnd.gt.rinfo%sample_rate)mask(ipart)=.false.
             end do
          endif
       endif

       ! Read positions
       do i = 1, 6, 1
          if(npfile(i).gt.0)then
             ! Allocate temporary buffer
             allocate(pos(3,npfile(i)),stat=istat)
             if(istat.ne.0)then
                call particle_store_empty(pdata)
                gadget_hdf5_read%string = "Unable to allocate pos buffer"
                hdferr = hdf5_close_file()
                return
             endif
             ! Read the data
             gname  = "PartType"//trim(string(i-1))
             hdferr = hdf5_read_dataset(trim(gname)//"/Coordinates", pos)
             if(hdferr.ne.0)then
                gadget_hdf5_read%string="Unable to read positions from "//&
                     trim(fname)
                gadget_hdf5_read%string="NumPart "//trim(string(npfile(i)))//&
                     " Error "//trim(string(hdferr))
                call particle_store_empty(pdata)
                hdferr = hdf5_close_file()
                return
             endif
             ! Set mask=false for particles outside the selected region
             if(rinfo%do_sphere)then
                offset = sum(npfile(1:i-1))
                do j = 1, npfile(i), 1
                   if(any(abs(pos(:,j)-rinfo%pos).gt.rinfo%radius)) &
                        mask(offset+j) = .false.
                end do
             endif
             ! Store the data we want to keep
             offset = sum(npfile(1:i-1))
             if(allocated(mask))then
                nkeep = shrink_array(pos(1,:), mask(offset+1:))
                nkeep = shrink_array(pos(2,:), mask(offset+1:))
                nkeep = shrink_array(pos(3,:), mask(offset+1:))
                res = particle_store_add_data(pdata, species_name(i), &
                     pos=real(pos(:,1:nkeep),pos_kind))
             else
                res = particle_store_add_data(pdata, species_name(i), &
                     pos=real(pos,pos_kind))
             endif
             deallocate(pos)
             if(.not.res%success)then
                gadget_hdf5_read%string="Unable to allocate memory"
                call particle_store_empty(pdata)
                hdferr = hdf5_close_file()
                return
             endif
          endif
       end do

       prog_so_far = prog_so_far + sum(npfile)
       call progress_bar_update(real(prog_so_far)/real(prog_tot))

       ! Read velocities
       do i = 1, 6, 1
          if(npfile(i).gt.0)then
             
             ! Allocate temporary buffer
#ifdef READ_VEL
             allocate(vel(3,npfile(i)),stat=istat)
             if(istat.gt.0)then
                call particle_store_empty(pdata)
                gadget_hdf5_read%string = "Unable to allocate vel buffer"
                hdferr = hdf5_close_file()
                return
             endif
             ! Read the data
             gname  = "PartType"//trim(string(i-1))
             hdferr = hdf5_read_dataset(trim(gname)//"/Velocity", vel)
             if(hdferr.ne.0) &
                  hdferr = hdf5_read_dataset(trim(gname)//"/Velocities", vel)
             if(hdferr.ne.0)then
                gadget_hdf5_read%string="Unable to read velocities from "//&
                     trim(fname)
                call particle_store_empty(pdata)
                hdferr = hdf5_close_file()
                return
             endif
             ! Store the data we want to keep
             offset = sum(npfile(1:i-1))
             if(allocated(mask))then
                nkeep = shrink_array(vel(1,:), mask(offset+1:))
                nkeep = shrink_array(vel(2,:), mask(offset+1:))
                nkeep = shrink_array(vel(3,:), mask(offset+1:))
                res = particle_store_add_data(pdata, species_name(i), &
                     vel=real(vel(:,1:nkeep),vel_kind))
             else
                res = particle_store_add_data(pdata, species_name(i), &
                     vel=real(vel,vel_kind))
             endif
             deallocate(vel)
             if(.not.res%success)then
                gadget_hdf5_read%string="Unable to allocate memory"
                call particle_store_empty(pdata)
                hdferr = hdf5_close_file()
                return
             endif
#endif
          endif
       end do

       prog_so_far = prog_so_far + sum(npfile)
       call progress_bar_update(real(prog_so_far)/real(prog_tot))

       ! Read any extra properties
       do ispecies = 1, 6, 1
          if(npfile(ispecies).gt.0)then
             !
             ! Assign masses, if not stored individually
             !
             if(.not.have_mass(ispecies))then
                if(.not.rinfo%ignore_missing_mass.and.massarr(ispecies).eq.0)then
                   ! We were asked to read masses but we don't have any for this particle type
                   gadget_hdf5_read%string="Unable to find particle mass dataset"
                   call particle_store_empty(pdata)
                   hdferr = hdf5_close_file()
                   return
                endif
                ! Otherwise, construct the mass array
                allocate(rdata(npfile(ispecies)))
                if(massarr(ispecies).eq.0)then
                   rdata(:) = 1.0
                else
                   rdata(:) = massarr(ispecies)
                endif
                ! Store the data we want to keep
                offset = sum(npfile(1:ispecies-1))
                if(allocated(mask))then
                   nkeep = shrink_array(rdata, mask(offset+1:))
                else
                   nkeep = npfile(ispecies)
                endif
                res = particle_store_add_data(pdata,&
                     species_name(ispecies), mass_datasets(1), &
                     rdata=real(rdata(1:nkeep),kind=r_prop_kind))
                deallocate(rdata)
                if(.not.res%success)then
                   gadget_hdf5_read%string="Unable to allocate memory"
                   call particle_store_empty(pdata)
                   hdferr = hdf5_close_file()
                   return
                endif
             endif
             !
             ! Other quantities
             !
             do iextra = 1, nextra_all, 1
                if(read_extra(ispecies,iextra))then
                   ! Get the dataset name
                   str = "/PartType"//&
                        trim(string(ispecies-1,fmt='(i1.1)'))//"/"// &
                        trim(extra_prop_all(iextra))
                   ! Read the data
                   select case(extra_type(ispecies,iextra))
                   case("INTEGER")
                      allocate(idata(npfile(ispecies)))
                      hdferr = hdf5_read_dataset(str, idata)
                      if(hdferr.ne.0)then
                         gadget_hdf5_read%string=&
                              "Unable to read dataset: "//trim(str)
                         call particle_store_empty(pdata)
                         hdferr = hdf5_close_file()
                         return
                      endif
                      ! Store the data we want to keep
                      offset = sum(npfile(1:ispecies-1))
                      if(allocated(mask))then
                         nkeep = shrink_array(idata, mask(offset+1:))
                      else
                         nkeep = npfile(ispecies)
                      endif
                      res = particle_store_add_data(pdata,&
                           species_name(ispecies),&
                           extra_prop_all(iextra), &
                           idata=int(idata(1:nkeep),kind=i_prop_kind))
                      deallocate(idata)
                      if(.not.res%success)then
                         gadget_hdf5_read%string="Unable to allocate memory"
                         call particle_store_empty(pdata)
                         hdferr = hdf5_close_file()
                         return
                      endif
                   case("REAL")
                      allocate(rdata(npfile(ispecies)))
                      hdferr = hdf5_read_dataset(str, rdata)
                      if(hdferr.ne.0)then
                         gadget_hdf5_read%string="Unable to read dataset: "&
                              //trim(str)
                         call particle_store_empty(pdata)
                         hdferr = hdf5_close_file()
                         return
                      endif
                      ! Store the data we want to keep
                      offset = sum(npfile(1:ispecies-1))
                      if(allocated(mask))then
                         nkeep = shrink_array(rdata, mask(offset+1:))
                      else
                         nkeep = npfile(ispecies)
                      endif
                      res = particle_store_add_data(pdata,&
                           species_name(ispecies),&
                           extra_prop_all(iextra), &
                           rdata=real(rdata(1:nkeep),kind=r_prop_kind))
                      deallocate(rdata)
                      if(.not.res%success)then
                         gadget_hdf5_read%string="Unable to allocate memory"
                         call particle_store_empty(pdata)
                         hdferr = hdf5_close_file()
                         return
                      endif
                   end select
                endif
             end do
          endif
       end do
       
       ! Close this file
       hdferr = hdf5_close_file()

       if(rinfo%do_sampling.or.rinfo%do_sphere)then
          deallocate(mask)
       endif

       ! Next file
    end do

    gadget_hdf5_read%success = .true.

    ! Set redshift
    call particle_store_set_time(pdata,time,redshift,expansion,time_unit)
    call particle_store_set_boxsize(pdata,real(boxsize))

    !call summary_add_line(s,&
    !     "Number of files = "//trim(adjustl(string(nfiles))))

#else
    ! Should never get here because menu option is greyed out if HDF5 not
    ! available
    call terminate('gadget_hdf5_reader - Code was compiled without HDF5 support')

    ! Stop compiler complaining about return value not being set
    gadget_hdf5_read%success = .false.
    gadget_hdf5_read%string  = "Compiled without HDF5 support"
#endif

    return
  end function gadget_hdf5_read

end module gadget_hdf5_reader


