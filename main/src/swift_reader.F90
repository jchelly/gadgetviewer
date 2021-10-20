module swift_reader
!
! Module to read SWIFT snapshot files
!
#include "../../config.h"
#ifdef HAVE_HDF5
  use read_hdf5
#endif
  use data_types
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

  public :: swift_open
  public :: swift_read
  public :: swift_read_conf

  ! Weighting for progress bar
  real, dimension(maxspecies) :: prog_weight

  ! Maximum number of extra quantities to read
  integer, parameter :: max_extra = 100

  ! Extra properties defined in the config file
  integer :: nextra_config = 0
  character(len=maxlen), dimension(max_extra) :: extra_prop_config

  ! Extra properties from config+command line
  integer :: nextra_all
  character(len=maxlen), dimension(max_extra) :: extra_prop_all

  ! Which properties exist in the current snapshot
  logical, dimension(maxspecies,max_extra)           :: read_extra
  character(len=20), dimension(maxspecies,max_extra) :: extra_type

  ! Path information extracted from file name
  type (path_data_type) :: path_data

  ! Size of a particle ID
  integer :: id_size

  ! Number of particle types
  integer :: nr_types, nr_types_read

  ! Particle type names
  ! TODO: read from snapshot file
  character(len=21) :: species_name(7) = (/ &
       "Type 0 - Gas         ", &
       "Type 1 - DM          ", &
       "Type 2 - DMBackground", &
       "Type 3 - Sink        ", &
       "Type 4 - Stars       ", &
       "Type 5 - BH          ", &
       "Type 6 - Neutrino    " &
       /)

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

  
  subroutine swift_read_conf(dir)
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

    fname = trim(dir)//"/"//"swift_extra_properties"
    call read_key_file(fname)
    nextra_config = 0

    ! Create the config file if it doesn't exist
    if(.not.file_has_group("SWIFT"))then
       ! Default settings
       call add_extra("MetalMassFractions")
       call add_extra("StarFormationRates")
       call add_extra("Temperatures")
       call add_extra("Densities")
       call add_extra("InternalEnergies")
       call add_extra("FOFGroupIDs")
       call set_key("SWIFT","Extra Properties", extra_prop_config(1:nextra_config)) 
       call write_key_file(fname)
    else
       extra_prop_config(:) = ""
       call get_key("SWIFT","Extra Properties", extra_prop_config)
       do i = 1, max_extra, 1
          if(len_trim(extra_prop_config(i)).gt.0)then
             nextra_config = i
          else
             exit
          endif
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

  end subroutine swift_read_conf

!
! Scan a simulation directory and store information
! required for reading snapshots
!
  type(result_type) function swift_open(fname, isnap, rinfo)

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
    integer :: nr_cells
    integer(kind=int8byte), dimension(:), allocatable :: npfile
    ! HDF5 stuff
    integer           :: hdferr, err_array(10)
    type(result_type) :: res
    integer           :: dtype

    swift_open%success = .false.

    ! Check we haven't been asked to read one file
    if(rinfo%just_this_file)then
       swift_open%string  = &
            "Unable to read single file from multi file snapshot with SWIFT reader"
       return
    endif

    ! Check if the file exists
    inquire(file=fname,exist=fexist,iostat=ios)
    if(ios.eq.0)then
       if(.not.fexist)then
          swift_open%success = .false.
          swift_open%string  = "File does not exist: "//trim(fname)
          return
       endif
    else
       swift_open%success = .false.
       swift_open%string  = "Unable to access file" 
       return
    endif

    ! See if we can open it with HDF5
    hdferr = hdf5_open_file(fname)
    if(hdferr.ne.0)then
       swift_open%string="Unable to open HDF5 file: "//trim(fname)
       return
    endif

    ! Check that this snapshot has the Cells group
    hdferr = hdf5_read_attribute("/Cells/Meta-data/nr_cells", nr_cells)
    if(hdferr.ne.0)then
       swift_open%string= &
            "Unable to read Cells/Meta-data/nr_cells attribute from file: "//trim(fname)
       return
    endif

    ! Read the number of files
    hdferr = hdf5_read_attribute("/Header/NumFilesPerSnapshot", n)
    if(hdferr.ne.0)then
       swift_open%string= &
            "Unable to read NumFilesPerSnapshot from file: "//trim(fname)
       return
    endif
    if(n.ne.1)then
       swift_open%string="Can only read single file SWIFT snapshots"
       return
    endif

    ! Read the number of particle types
    hdferr = hdf5_read_attribute("/Header/NumPartTypes", nr_types)
    if(hdferr.ne.0)then
       swift_open%string= &
            "Unable to read NumPartTypes from file: "//trim(fname)
       return
    endif
    nr_types_read = min(nr_types, maxspecies)

    ! Close the file
    hdferr = hdf5_close_file()

    ! Don't know size of IDs initially
    id_size = -1

    ! Extract information from the path so we can try to infer where the
    ! other snapshots will be
    res = gadget_path_extract(fname, jsnap, path_data)
    if(.not.res%success)then
       ! fname is somehow messed up enough that we can't interpret it,
       ! despite it being a valid filename
       swift_open = res
       isnap = -1
       return
    endif

    ! Make full list of properties to try to read
    ! Add list from config file
    nextra_all = 0
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
    read_extra = .false.
    ifile = 0
    call gadget_path_generate(jsnap, ifile, fname, path_data)
    hdferr = hdf5_open_file(fname)
    if(hdferr.ne.0)then
       swift_open%string= &
            "Unable to open file: "//trim(fname)
       return
    endif

    ! Read particle number in this file
    allocate(npfile(nr_types))
    hdferr = hdf5_read_attribute("/Header/NumPart_ThisFile", npfile)
    if(hdferr.ne.0)then
       swift_open%string="Unable to read NumPart_ThisFile from file"
       hdferr = hdf5_close_file()
       deallocate(npfile)
       return
    endif

    do ispecies = 1, nr_types_read, 1
          
       ! Check ID size if we don't already have it
       if(id_size.lt.0)then
          str = "/PartType"//trim(string(ispecies-1,fmt='(i1.1)'))//"/"// &
               "ParticleIDs"
          hdferr = hdf5_dataset_type(str, dtype)
          if(hdferr.eq.0)then
             if(dtype.eq.HDF5_INTEGER4)then
                id_size = 4
             else
                id_size = 8
             endif
          endif
       endif

       do iextra = 1, nextra_all, 1
          str = "/PartType"//trim(string(ispecies-1,fmt='(i1.1)'))//"/"// &
               trim(extra_prop_all(iextra))
          hdferr = hdf5_dataset_type(str, dtype)
          if(hdferr.eq.0)then
             select case(dtype)
             case(HDF5_INTEGER4, HDF5_INTEGER8)
                ! Integer dataset
                read_extra(ispecies,iextra) = .true.
                extra_type(ispecies,iextra) = "INTEGER"
             case(HDF5_REAL4, HDF5_REAL8)
                ! Real dataset
                read_extra(ispecies,iextra) = .true.
                extra_type(ispecies,iextra) = "REAL"
             case default
                ! Don't read if its not real/integer
                read_extra(ispecies,iextra) = .false.
                extra_type(ispecies,iextra) = "UNKNOWN"
             end select
          endif
       end do

    end do
    hdferr = hdf5_close_file()

    ! Looks ok - store the details and return snapshot number
    isnap = jsnap
  
    ! Figure out weighting for progress bar
    prog_weight(1:nr_types_read) = real(npfile(1:nr_types_read))/sum(real(npfile(1:nr_types_read)))
    
    ! If we get this far, everything's ok
    swift_open%success = .true.
    deallocate(npfile)

#else
    ! Should never get here because menu option is greyed out if HDF5 not
    ! available
    call terminate('swift_reader - Code was compiled without HDF5 support')

    ! Stop compiler complaining about return value not being set
    swift_open%success = .false.
    swift_open%string  = "Compiled without HDF5 support"
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

  end function swift_open

!
! Read the specified snapshot, if possible
!
  type (result_type) function swift_read(isnap, rinfo)

    implicit none
    ! Parameters
    integer :: isnap
    type (read_info) :: rinfo
    ! HDF5
    integer :: hdferr
    ! Snapshot file name
    character(len=500) :: fname
    logical :: file_open
    ! Redshift, time etc
    real :: redshift, time, expansion
    integer :: nfiles
    integer :: ispecies
    integer(kind=int8byte), dimension(:), allocatable :: npfile
    ! Loops etc
    integer :: i, j, iextra, ios, iprop
    character(len=500) :: str
    ! Cell info
    integer :: nr_cells, cell_nr, nr_cells_read, nkeep
    real(kind=real8byte), dimension(:,:), allocatable :: centres
    real(kind=real8byte), dimension(3)   :: cell_size
    logical, dimension(:), allocatable :: read_cell
    integer(kind=int8byte), dimension(:), allocatable :: cell_offset, cell_length, cell_order
    real(kind=real8byte), dimension(3) :: wrapped_centre
    real(kind=real8byte), dimension(3) :: boxsize
    ! Range of particles in cell
    integer(kind=int8byte) :: first_particle, num_particles
    integer(kind=int8byte), parameter :: max_particles = 1000000
    integer :: first_cell, last_cell
    ! Mask for random sampling
    logical, dimension(:), allocatable :: mask
    ! Result from function calls
    type (result_type) :: res
    ! Random number
    real    :: rnd
    ! Read buffers
    real(kind=pos_kind),       dimension(:,:), allocatable :: pos
    integer(kind=i_prop_kind), dimension(:),   allocatable :: idata
    real(kind=r_prop_kind),    dimension(:),   allocatable :: rdata
    ! Progress bar
    real :: prog_so_far
    ! Name of the masses dataset
    character(len=100) :: mass_name

#ifdef HAVE_HDF5
    
    file_open = .false.
    swift_read%success = .false.

    call progress_bar_update(0.0)

    ! Generate the file name
    call gadget_path_generate(isnap, 0, fname, path_data)

    ! Check we haven't been asked to read one file
    if(rinfo%just_this_file)then
       swift_read%string  = &
            "Unable to read single files from multi file snapshots with SWIFT reader"
       call cleanup()
       return
    endif

    ! See if we can open it with HDF5
    hdferr = hdf5_open_file(fname)
    if(hdferr.ne.0)then
       swift_read%string="Unable to open HDF5 file: "//trim(fname)
       call cleanup()
       return
    else
       file_open = .true.
    endif

    ! Read redshift
    hdferr = hdf5_read_attribute("/Cosmology/Redshift", redshift)
    if(hdferr.ne.0)then
       redshift = -1.0
    endif
    
    ! Read time
    hdferr = hdf5_read_attribute("/Header/Time", time)
    if(hdferr.ne.0)then
       time = -1.0
    endif

    ! Read expansion factor
    hdferr = hdf5_read_attribute("/Cosomology/Scale-factor", expansion)
    if(hdferr.ne.0)then
       expansion = -1.0
    endif

    ! Check number of files
    hdferr = hdf5_read_attribute("/Header/NumFilesPerSnapshot", nfiles)
    if(hdferr.ne.0)then
       swift_read%string="Unable to read Header/NumFilesPerSnapshot"
       call cleanup()
       return
    endif
    if(nfiles.ne.1)then
       swift_read%string="Can only read single file SWIFT snapshots"
       call cleanup()
       return
    endif

    ! Read particle number in this file
    allocate(npfile(nr_types))
    hdferr = hdf5_read_attribute("/Header/NumPart_ThisFile", npfile)
    if(hdferr.ne.0)then
       swift_read%string="Unable to read NumPart_ThisFile from file"
       call cleanup()
       return
    endif

    ! At this point we know the file is readable, so deallocate the old
    ! snapshot
    call particle_store_empty(pdata)
    call particle_store_empty(psample)
   
    !
    ! Determine which cells we need to read
    !
    ! Find number of cells
    hdferr = hdf5_read_attribute("/Cells/Meta-data/nr_cells", nr_cells)
    if(hdferr.ne.0)then
       swift_read%string= &
            "Unable to read Cells/Meta-data/nr_cells attribute from file: "//trim(fname)
       call cleanup()
       return
    endif

    ! Read cell centres
    allocate(centres(3, nr_cells))
    hdferr = hdf5_read_dataset("Cells/Centres", centres)
    if(hdferr.ne.0)then
       swift_read%string= &
            "Unable to read Cells/Centres dataset from file: "//trim(fname)
       call cleanup()
       return
    endif

    ! Get cell size
    hdferr = hdf5_read_attribute("/Cells/Meta-data/size", cell_size)
    if(hdferr.ne.0)then
       swift_read%string= &
            "Unable to read Cells/Meta-data/size dataset from file: "//trim(fname)
       call cleanup()
       return
    endif

    ! Get box size
    hdferr = hdf5_read_attribute("/Header/BoxSize", boxsize)
    if(hdferr.ne.0)then
       swift_read%string= &
            "Unable to read Header/BoxSize from file: "//trim(fname)
       call cleanup()
       return
    endif

    ! Flag cells to read
    allocate(read_cell(nr_cells))
    if(rinfo%do_sphere)then
       !
       ! Reading a region
       !
       do cell_nr = 1, nr_cells, 1
          wrapped_centre = centres(:,cell_nr)

          ! Wrap this cell to copy nearest the requested point
          do i = 1, 3, 1
             do while(wrapped_centre(i).gt.rinfo%pos(i)+0.5*boxsize(i))
                wrapped_centre(i) = wrapped_centre(i) - boxsize(i)
             end do
             do while(wrapped_centre(i).lt.rinfo%pos(i)-0.5*boxsize(i))
                wrapped_centre(i) = wrapped_centre(i) + boxsize(i)
             end do
          end do

          ! Check if this cell overlaps the requested region
          read_cell(cell_nr) = .true.
          do i = 1, 3, 1
             if(wrapped_centre(i)+0.5*cell_size(i).lt.rinfo%pos(i)-rinfo%radius)read_cell(cell_nr) = .false.
             if(wrapped_centre(i)-0.5*cell_size(i).gt.rinfo%pos(i)+rinfo%radius)read_cell(cell_nr) = .false.
          end do

          ! Check next cell
       end do
    else
       !
       ! Reading everything
       !
       read_cell = .true.
    endif
    
    !
    ! Loop over particle types to read
    !
    do ispecies = 1, nr_types_read, 1

       ! Allocate storage for a new particle type
       res = particle_store_new_species(pdata,species_name(ispecies))
       if(.not.res%success)then
          swift_read = res
          call particle_store_empty(pdata)
          call cleanup()
          return
       endif

       ! Skip types with no particles
       if(npfile(ispecies).gt.0)then

          ! Read cell lengths for this type
          write(str, "('Cells/Counts/PartType',1i1)")ispecies-1
          allocate(cell_length(nr_cells))
          hdferr = hdf5_read_dataset(str, cell_length)
          if(hdferr.ne.0)then
             swift_read%string= &
                  "Unable to read "//trim(str)//" dataset from file: "//trim(fname)
             call cleanup()
             return
          endif

          ! Read cell offsets for this type
          write(str, "('Cells/OffsetsInFile/PartType',1i1)")ispecies-1
          allocate(cell_offset(nr_cells))
          hdferr = hdf5_read_dataset(str, cell_offset)
          if(hdferr.ne.0)then
             swift_read%string= &
                  "Unable to read "//trim(str)//" dataset from file: "//trim(fname)
             call cleanup()
             return
          endif

          ! Discard offsets and lengths of cells we're not reading
          nr_cells_read = 0
          do cell_nr = 1, nr_cells, 1
             if(read_cell(cell_nr))then
                nr_cells_read = nr_cells_read + 1
                cell_length(nr_cells_read) = cell_length(cell_nr)
                cell_offset(nr_cells_read) = cell_offset(cell_nr)
             endif
          end do

          ! Decide on name of the mass dataset
          if(ispecies.eq.6)then
             mass_name = "DynamicalMasses"
          else if(ispecies.eq.7)then
             mass_name = "FakeUnitMass"
          else
             mass_name = "Masses"
          endif

          ! Allocate storage for masses and IDs
          res = particle_store_new_property(pdata,species_name(ispecies), mass_name, &
               "REAL", is_mass=.true.)
          if(.not.res%success)then
             swift_read = res
             call particle_store_empty(pdata)
             call cleanup()
             return
          endif
          res = particle_store_new_property(pdata,species_name(ispecies),"ParticleIDs", &
               "INTEGER", is_id=.true.)
          if(.not.res%success)then
             swift_read = res
             call particle_store_empty(pdata)
             return
          endif

          ! Allocate storage for any extra properties
          do iextra = 1, nextra_all, 1
             if(read_extra(ispecies,iextra))then
                res = particle_store_new_property(pdata,species_name(ispecies),&
                     extra_prop_all(iextra), extra_type(ispecies,iextra))
                if(.not.res%success)then
                   swift_read = res
                   call particle_store_empty(pdata)
                   call cleanup()
                   return
                endif
             endif
          end do

          if(nr_cells_read.gt.0)then

             ! Get sorting index for cell offsets
             allocate(cell_order(nr_cells_read))
             call sort_index(cell_offset(1:nr_cells_read), cell_order)

             ! Get name of the group for this type
             write(str, "('/PartType',i1.1)")ispecies-1
             
             ! Loop over cells in order of offset
             first_cell = 1
             do while(first_cell.le.nr_cells_read)
                
                ! Find range of cells to read in on this iteration
                last_cell = first_cell
                num_particles = 0
                do while(.true.)
                   num_particles = num_particles + cell_length(cell_order(last_cell))
                   ! Check for end of cell array
                   if(last_cell.eq.nr_cells_read)exit
                   ! Check if we can merge the next cell with this one
                   ! Don't merge if there would be too many particles
                   if(num_particles+cell_length(cell_order(last_cell+1)).gt.max_particles)exit
                   ! Don't merge if cells are not contiguous
                   if(cell_offset(cell_order(last_cell))+cell_length(cell_order(last_cell)).eq.&
                        cell_offset(cell_order(last_cell+1)))then
                      last_cell = last_cell + 1
                   else
                      exit
                   endif
                end do
                first_particle = cell_offset(cell_order(first_cell))

                prog_so_far = sum(prog_weight(1:ispecies-1)) + &
                     (real(first_cell)/real(nr_cells_read))*prog_weight(ispecies)
                call progress_bar_update(prog_so_far/sum(prog_weight))

                if(num_particles.gt.0)then

                   ! Generate mask for random sampling
                   if(rinfo%do_sampling)then
                      allocate(mask(num_particles))
                      mask = .true.
                      if(rinfo%do_sampling)then
                         do j = 1, num_particles, 1
                            call random_number(rnd)
                            if(rnd.gt.rinfo%sample_rate)mask(j)=.false.
                         end do
                      endif
                   endif

                   ! Read positions for this cell
                   allocate(pos(3,num_particles))
                   if(hdf5_read_dataset(trim(str)//"/Coordinates", pos, &
                        start=(/0_int8byte, first_particle/), &
                        count=(/3_int8byte, num_particles/)).ne.0)then
                      swift_read%string = "Unable to read Coordinates dataset"
                      call particle_store_empty(pdata)
                      call cleanup()
                      return
                   endif
                   if(rinfo%do_sampling)then
                      nkeep = shrink_array(pos(1,:), mask(:))
                      nkeep = shrink_array(pos(2,:), mask(:))
                      nkeep = shrink_array(pos(3,:), mask(:))
                   else
                      nkeep = num_particles
                   endif
                   res = particle_store_add_data(pdata, species_name(ispecies), &
                        pos=pos(1:3,1:nkeep))
                   deallocate(pos)
                   if(.not.res%success)then
                      swift_read%string = "Failed to allocate memory"
                      call particle_store_empty(pdata)
                      call cleanup()
                      return                      
                   endif

                   ! Read IDs
                   allocate(idata(num_particles))
                   if(hdf5_read_dataset(trim(str)//"/ParticleIDs", idata, &
                        start=(/first_particle/), &
                        count=(/num_particles/)).ne.0)then
                      swift_read%string = "Unable to read ParticleIDs dataset"
                      call particle_store_empty(pdata)
                      call cleanup()
                      return
                   endif
                   if(rinfo%do_sampling)then
                      nkeep = shrink_array(idata(:), mask(:))
                   else
                      nkeep = num_particles
                   endif
                   res = particle_store_add_data(pdata, species_name(ispecies), &
                        prop_name="ParticleIDs", idata=idata(1:nkeep))
                   deallocate(idata)
                   if(.not.res%success)then
                      swift_read%string = "Failed to allocate memory"
                      call particle_store_empty(pdata)
                      call cleanup()
                      return                      
                   endif

                   ! Read masses
                   allocate(rdata(num_particles))
                   if(ispecies.ne.7)then
                      if(hdf5_read_dataset(trim(str)//"/"//trim(mass_name), rdata, &
                           start=(/first_particle/), &
                           count=(/num_particles/)).ne.0)then
                         swift_read%string = "Unable to read Masses dataset"
                         call particle_store_empty(pdata)
                         call cleanup()
                         return
                      endif
                   else
                      rdata = 1.0
                   endif
                   if(rinfo%do_sampling)then
                      nkeep = shrink_array(rdata(:), mask(:))
                   else
                      nkeep = num_particles
                   endif
                   res = particle_store_add_data(pdata, species_name(ispecies), &
                        prop_name=mass_name, rdata=rdata(1:nkeep))
                   deallocate(rdata)
                   if(.not.res%success)then
                      swift_read%string = "Failed to allocate memory"
                      call particle_store_empty(pdata)
                      call cleanup()
                      return                      
                   endif
                   
                   ! Read other properties
                   do iprop = 1, nextra_all, 1
                      if(read_extra(ispecies, iprop))then
                         ! Read the dataset
                         select case(extra_type(ispecies, iprop))
                         case("INTEGER")
                            allocate(idata(num_particles), stat=ios)
                            if(ios.eq.0)then
                               ios = hdf5_read_dataset(trim(str)//"/"//trim(extra_prop_all(iprop)),&
                                    idata, start=(/first_particle/), count=(/num_particles/))
                               if(ios.eq.0)then
                                  ! Sample particles
                                  if(rinfo%do_sampling)then
                                     nkeep = shrink_array(idata(:), mask(:))
                                  else
                                     nkeep = num_particles
                                  endif
                                  res = particle_store_add_data(pdata, &
                                       species_name(ispecies), &
                                       prop_name=extra_prop_all(iprop), &
                                       idata=idata(1:nkeep))
                                  if(.not.res%success)then
                                     swift_read%success = .false.
                                     swift_read%string = "Unable to allocate memory"
                                     call cleanup()
                                     call particle_store_empty(pdata)
                                     return
                                  endif
                               endif
                            endif
                            deallocate(idata)
                         case("REAL")
                            allocate(rdata(num_particles), stat=ios)
                            if(ios.eq.0)then
                               ios = hdf5_read_dataset(trim(str)//"/"//trim(extra_prop_all(iprop)),&
                                    rdata, start=(/first_particle/), count=(/num_particles/))
                               if(ios.eq.0)then
                                  ! Sample particles
                                  if(rinfo%do_sampling)then
                                     nkeep = shrink_array(rdata(:), mask(:))
                                  else
                                     nkeep = num_particles
                                  endif
                                  res = particle_store_add_data(pdata, &
                                       species_name(ispecies), &
                                       prop_name=extra_prop_all(iprop), &
                                       rdata=rdata(1:nkeep))
                                  if(.not.res%success)then
                                     swift_read%success = .false.
                                     swift_read%string = "Unable to allocate memory"
                                     call cleanup()
                                     call particle_store_empty(pdata)
                                     return
                                  endif
                               endif
                            endif
                            deallocate(rdata)
                         case default
                            swift_read%success = .false.
                            swift_read%string  = "Dataset has unknown type!"
                            call cleanup()
                            call particle_store_empty(pdata)
                            return 
                         end select
                         if(ios.ne.0)then
                            swift_read%success = .false.
                            swift_read%string  = "Unable to read "//&
                                 trim(extra_prop_all(iprop))//" dataset"
                            call cleanup()
                            call particle_store_empty(pdata)
                            return
                         endif
                      endif
                      ! Next extra property
                   end do
                   
                   if(allocated(mask))deallocate(mask)

                endif

                ! Next cell to read
                first_cell = last_cell + 1
             end do
             
             deallocate(cell_order)
          endif
          deallocate(cell_length, cell_offset)
          
       endif
       ! Next particle type
    end do

    ! Close the file
    hdferr = hdf5_close_file()
    file_open = .false.

    ! Store box size
    call particle_store_set_boxsize(pdata, real(boxsize(1)))

    ! Set redshift
    call particle_store_set_time(pdata,time,redshift,expansion,"")

    ! Store ID size for this snapshot
    call particle_store_set_idsize(pdata, id_size)

    call progress_bar_update(1.0)

    call cleanup()

    swift_read%success = .true.

#else
    ! Should never get here because menu option is greyed out if HDF5 not
    ! available
    call terminate('swift_reader - Code was compiled without HDF5 support')

    ! Stop compiler complaining about return value not being set
    swift_read%success = .false.
    swift_read%string  = "Compiled without HDF5 support"
#endif

    return

  contains 

    subroutine cleanup()
      
      if(allocated(read_cell))   deallocate(read_cell)
      if(allocated(centres))     deallocate(centres)
      if(allocated(cell_length)) deallocate(cell_length)
      if(allocated(cell_offset)) deallocate(cell_offset)
      if(allocated(cell_order))  deallocate(cell_order)
      if(allocated(npfile))      deallocate(npfile)
      if(allocated(pos))         deallocate(pos)
      if(allocated(idata))       deallocate(idata)
      if(allocated(rdata))       deallocate(rdata)

      if(file_open)then
         hdferr = hdf5_close_file()
         file_open = .false.
      endif

      return
    end subroutine cleanup

  end function swift_read

end module swift_reader


