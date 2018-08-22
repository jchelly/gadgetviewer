module gadget_eagle_reader
!
! Module to read EAGLE snapshot files
!
#include "../../config.h"
#ifdef HAVE_HDF5
  use read_hdf5
  use read_eagle
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

  implicit none
  private
  save

  public :: gadget_eagle_open
  public :: gadget_eagle_read
  public :: gadget_eagle_read_conf

  ! Simulation details
  integer :: nfiles

  ! Particle masses
  real, dimension(6) :: mpart

  ! Weighting for progress bar
  real, dimension(6) :: prog_weight

  ! Extra properties to read
  integer :: nextra = 0
  integer, parameter :: max_extra = 100
  character(len=maxlen), dimension(max_extra) :: extra_prop

  ! Which properties exist in the current snapshot
  logical, dimension(maxspecies,max_extra)           :: read_extra
  character(len=20), dimension(maxspecies,max_extra) :: extra_type

  ! Path information extracted from file name
  type (path_data_type) :: path_data

  ! Size of a particle ID
  integer :: id_size

contains

  subroutine gadget_eagle_read_conf(dir)
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
    character(len=maxlen), dimension(max_extra) :: props
    integer :: iextra

    fname = trim(dir)//"/"//"gadget_eagle_extra_properties"
    props = ""
    call read_key_file(fname)
    if(file_has_group("Eagle"))then
       call get_key("Eagle","Extra Properties", props)
    else
       ! Default settings
       nextra = 5
       extra_prop(1) = "Metallicity"
       extra_prop(2) = "StarFormationRate"
       extra_prop(3) = "Temperature"
       extra_prop(4) = "Density"
       extra_prop(5) = "InternalEnergy"
       call set_key("Eagle","Extra Properties", extra_prop(1:nextra)) 
       call write_key_file(fname)
    endif
    call close_key_file()

    nextra = 0
    do iextra = 1, max_extra, 1
       str = trim(props(iextra))
       if(len_trim(str).gt.0)then
          ! Ignore names that conflict with things we always read
          if(trim(adjustl(str)).eq."Coordinates")cycle
          if(trim(adjustl(str)).eq."Velocity")cycle
          if(trim(adjustl(str)).eq."Velocities")cycle
          if(trim(adjustl(str)).eq."Mass")cycle
          if(trim(adjustl(str)).eq."ParticleIDs")cycle
          if(trim(adjustl(str)).eq."ID")cycle
          ! Check for duplicates
          is_duplicate = .false.
          do i = 1, nextra, 1
             if(trim(adjustl(str)).eq.trim(extra_prop(i))) &
                  is_duplicate = .true.
          end do
          if(.not.is_duplicate)then
             nextra = nextra + 1
             extra_prop(nextra) = trim(adjustl(str))
          endif
       endif
    end do
    
    return

  end subroutine gadget_eagle_read_conf

!
! Scan a simulation directory and store information
! required for reading snapshots
!
  type(result_type) function gadget_eagle_open(fname, isnap, rinfo)

    implicit none
    ! Parameters
    character(len=*) :: fname
    integer, intent(out) :: isnap
    type (read_info) :: rinfo
#ifdef HAVE_HDF5
    ! Internal
    integer :: jsnap, ios, ifile
    logical :: fexist
    integer :: n
    integer :: ispecies, iextra
    character(len=maxlen) :: str
    ! Checking for datasets
    integer(kind=int4byte), dimension(6) :: npfile
    integer(kind=int8byte), dimension(6) :: nptot, nptot_hw
    logical,                dimension(6) :: find_type
    ! HDF5 stuff
    integer           :: hdferr, err_array(10)
    type(result_type) :: res
    integer           :: dtype

    gadget_eagle_open%success = .false.

    ! Check we haven't been asked to read one file
    if(rinfo%just_this_file)then
       gadget_eagle_open%string  = &
            "Unable to read single files with Eagle reader"
       return
    endif

    ! Check we HAVE been asked to use spatial indexing
    if(.not.rinfo%use_index)then
       gadget_eagle_open%string  = &
            "Must use spatial index with Eagle reader"
       return
    endif

    ! Check if the file exists
    inquire(file=fname,exist=fexist,iostat=ios)
    if(ios.eq.0)then
       if(.not.fexist)then
          gadget_eagle_open%success = .false.
          gadget_eagle_open%string  = "File does not exist: "//trim(fname)
          return
       endif
    else
       gadget_eagle_open%success = .false.
       gadget_eagle_open%string  = "Unable to access file" 
       return
    endif

    ! See if we can open it with HDF5
    hdferr = hdf5_open_file(fname)
    if(hdferr.ne.0)then
       gadget_eagle_open%string="Unable to open HDF5 file: "//trim(fname)
       return
    endif

    ! Check if we have the HashTable group
    hdferr = hdf5_read_attribute("/HashTable/HashBits", n)
    if(hdferr.ne.0)then
       gadget_eagle_open%string= &
            "Unable to read hash table size from file: "//trim(fname)
       return
    endif

    ! Read the number of files
    hdferr = hdf5_read_attribute("/Header/NumFilesPerSnapshot", n)
    if(hdferr.ne.0)then
       gadget_eagle_open%string= &
            "Unable to read NumFilesPerSnapshot from file: "//trim(fname)
       return
    endif
    
    ! Read particle masses
    hdferr = hdf5_read_attribute("/Header/MassTable", mpart)
    if(hdferr.ne.0)then
       gadget_eagle_open%string= &
            "Unable to read MassTable from file: "//trim(fname)
       return
    endif

    ! Close the file
    hdferr = hdf5_close_file()

    ! Don't know size of IDs initially
    id_size = -1

    ! Extract information from the path so we can try to infer where the
    ! other snapshots will be
    res = gadget_path_extract(fname, jsnap, path_data, eagle_snapshot=.true.)
    if(.not.res%success)then
       ! fname is somehow messed up enough that we can't interpret it,
       ! despite it being a valid filename
       gadget_eagle_open = res
       isnap = -1
       return
    endif

    ! Check which of the extra properties exist in this snapshot
    !
    ! Strategy is to keep opening snapshot files until we find at
    ! least one particle of each type that is present in the full
    ! snapshot.
    !
    read_extra = .false.
    find_type  = .true.
    do ifile = 0, n-1, 1
       call gadget_path_generate(jsnap, ifile, fname, path_data)
       hdferr = hdf5_open_file(fname)
       if(hdferr.ne.0)then
          gadget_eagle_open%string= &
               "Unable to open file: "//trim(fname)
          return
       endif

       ! Read total particle number and number in this file
       hdferr = hdf5_read_attribute("/Header/NumPart_Total", nptot)
       if(hdferr.ne.0)then
          gadget_eagle_open%string="Unable to read NumPart_Total from file"
          hdferr = hdf5_close_file()
          return
       endif
       hdferr = hdf5_read_attribute("/Header/NumPart_Total_HighWord", nptot_hw)
       if(hdferr.ne.0)then
          ! Not all Gadget files have this dataset
          nptot_hw = 0
       endif
       hdferr = hdf5_read_attribute("/Header/NumPart_ThisFile", npfile)
       if(hdferr.ne.0)then
          gadget_eagle_open%string="Unable to read NumPart_ThisFile from file"
          hdferr = hdf5_close_file()
          return
       endif
       nptot = nptot + (2_int8byte**32)*nptot_hw

       do ispecies = 1, 6, 1
          
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

          ! Determine if we need to keep looking for a file with
          ! particles of this type - can stop if a) there aren't
          ! any in the snapshot or b) there ARE some in this file.
          if(nptot(ispecies).eq.0)find_type(ispecies)=.false.
          if(npfile(ispecies).gt.0)find_type(ispecies)=.false.

          do iextra = 1, nextra, 1
             str = "/PartType"//trim(string(ispecies-1,fmt='(i1.1)'))//"/"// &
                  trim(extra_prop(iextra))
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

       ! If we've found at least one file with particles of each type
       ! present in the snapshot, can stop looking
       if(all(.not.find_type))exit
    end do

    ! Looks ok - store the details and return snapshot number
    nfiles        = max(1,n)
    isnap         = jsnap
  
    ! Figure out weighting for progress bar
    prog_weight = real(nptot)/ sum(real(nptot))
    
    ! If we get this far, everything's ok
    gadget_eagle_open%success = .true.

#else
    ! Should never get here because menu option is greyed out if HDF5 not
    ! available
    call terminate('gadget_eagle_reader - Code was compiled without HDF5 support')

    ! Stop compiler complaining about return value not being set
    gadget_eagle_open%success = .false.
    gadget_eagle_open%string  = "Compiled without HDF5 support"
    isnap = 0
#endif

    return
  end function gadget_eagle_open

!
! Read the specified snapshot, if possible
!
  type (result_type) function gadget_eagle_read(isnap, rinfo)

    implicit none
    ! Parameters
    integer :: isnap
    type (read_info) :: rinfo
    ! Snapshot reader info
#ifdef HAVE_HDF5
    type(EagleSnapshot) :: snap
#endif
    integer(kind=index_kind) :: np, n
    ! I/O error code
    integer :: ios
    ! Particle type names
    character(len=50), dimension(6) :: species_name
    integer :: ispecies
    ! Result from function calls
    type (result_type) :: res
    ! Snapshot file name
    character(len=500) :: fname
    ! Data buffers
    real(kind=pos_kind), dimension(:,:), pointer :: pos
#ifdef READ_VEL
    real(kind=vel_kind), dimension(:,:), pointer :: vel
#endif
    integer(kind=i_prop_kind), dimension(:), pointer :: idata
    real(kind=r_prop_kind), dimension(:), pointer :: rdata
    ! Sampling
    logical, dimension(:), allocatable :: mask
    integer(kind=index_kind) :: nkeep
    real    :: rnd
    ! Loops etc
    integer(kind=index_kind) :: i, j
    integer :: iprop, iextra, hdferr
    logical :: fexist
    ! redshift, time etc
    character(len=500) :: str, time_unit
    real :: redshift, time, expansion
    real :: fac

#ifdef HAVE_HDF5
    
    call progress_bar_update(0.0)

    ! Generate the file name
    call gadget_path_generate(isnap, 0, fname, path_data)

    ! Check we haven't been asked to read one file
    if(rinfo%just_this_file)then
       gadget_eagle_read%success = .false.
       gadget_eagle_read%string  = &
            "Unable to read single files with Eagle reader"
       return
    endif

    ! Check we HAVE been asked to use spatial indexing
    if(.not.rinfo%use_index)then
       gadget_eagle_read%success = .false.
       gadget_eagle_read%string  = &
            "Unable to read all files with Eagle reader"
       return
    endif

    ! Check if the file exists
    inquire(file=fname,exist=fexist,iostat=ios)
    if(ios.eq.0)then
       if(.not.fexist)then
          gadget_eagle_read%success = .false.
          gadget_eagle_read%string  = "File does not exist: "//trim(fname)
          return
       endif
    else
       gadget_eagle_read%success = .false.
       gadget_eagle_read%string  = "Unable to access file" 
       return
    endif

    ! See if we can open it with HDF5
    hdferr = hdf5_open_file(fname)
    if(hdferr.ne.0)then
       gadget_eagle_read%string="Unable to open HDF5 file: "//trim(fname)
       return
    endif

    ! Read redshift
    hdferr = hdf5_read_attribute("/Header/Redshift", redshift)
    if(hdferr.ne.0)then
       redshift = -1.0
    endif
    
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

    ! Close the file
    hdferr = hdf5_close_file()

    ! At this point we know the file is readable, so deallocate the old
    ! snapshot
    call particle_store_empty(pdata)
    call particle_store_empty(psample)

    ! Open the snapshot
    snap = open_snapshot(fname, iostat=ios)
    if(ios.ne.0)then
       gadget_eagle_read%success = .false.
       gadget_eagle_read%string  = "Unable to open Eagle snapshot file"
       return
    endif

    ! Select region to read
    if(rinfo%do_sphere)then
       call select_region(snap, &
            rinfo%pos(1)-rinfo%radius, rinfo%pos(1)+rinfo%radius, &
            rinfo%pos(2)-rinfo%radius, rinfo%pos(2)+rinfo%radius, &
            rinfo%pos(3)-rinfo%radius, rinfo%pos(3)+rinfo%radius)
    else
       call select_region(snap, 0.0, 1.0e20, 0.0, 1.0e20, 0.0, 1.0e20)
    endif

    ! Loop over particle types
    do ispecies = 1, 6, 1

       ! Find number of particles of this type
       np = count_particles(snap, ispecies-1, iostat=ios)
       if(ios.ne.0)then
          gadget_eagle_read%success = .false.
          gadget_eagle_read%string  = &
               "Unable to get particle count in Eagle snapshot"
          return
       endif

       ! Get name for this type
       select case(ispecies)
       case(1)
          species_name(ispecies)="Type 0 - Gas"
       case(2)
          species_name(ispecies)="Type 1 - Dark matter"
       case(3)
          species_name(ispecies)="Type 2 - Boundary"
       case(4)
          species_name(ispecies)="Type 3 - Boundary"
       case(5)
          species_name(ispecies)="Type 4 - Boundary/stars"
       case(6)
          species_name(ispecies)="Type 5 - Other"
       end select

       ! Allocate storage for a new particle type
       if(rinfo%do_sampling.or.rinfo%do_sphere.or.rinfo%just_this_file)then
          res = particle_store_new_species(pdata,species_name(ispecies))
       else
          res = particle_store_new_species(pdata,species_name(ispecies), np)
       endif
       if(.not.res%success)then
          gadget_eagle_read = res
          call particle_store_empty(pdata)
          return
       endif

       ! Allocate storage for masses and IDs
       i = ispecies
       res = particle_store_new_property(pdata,species_name(i),"Mass", &
            "REAL")
       if(.not.res%success)then
          gadget_eagle_read = res
          call particle_store_empty(pdata)
          return
       endif
       res = particle_store_new_property(pdata,species_name(i),"ID", &
            "INTEGER")
       if(.not.res%success)then
          gadget_eagle_read = res
          call particle_store_empty(pdata)
          return
       endif

       ! Allocate storage for any extra properties
       do iextra = 1, nextra, 1
          if(read_extra(ispecies,iextra))then
             res = particle_store_new_property(pdata,species_name(ispecies),&
                  extra_prop(iextra), extra_type(ispecies,iextra))
             if(.not.res%success)then
                gadget_eagle_read = res
                call particle_store_empty(pdata)
                return
             endif
          endif
       end do

       if(np.gt.0)then
          
          ! Generate mask for random sampling
          if(rinfo%do_sampling)then
             allocate(mask(np))
             mask = .true.
             if(rinfo%do_sampling)then
                do j = 1, np, 1
                   call random_number(rnd)
                   if(rnd.gt.rinfo%sample_rate)mask(j)=.false.
                end do
             endif
          endif

          ! Read positions
          nullify(pos)
          allocate(pos(3,np), stat=ios)
          if(ios.eq.0)then
             n = read_dataset(snap, ispecies-1, "Coordinates", pos, iostat=ios)
          endif
          if(ios.ne.0)then
             gadget_eagle_read%success = .false.
             gadget_eagle_read%string  = "Unable to read Coordinates dataset"
             call close_snapshot(snap)
             call particle_store_empty(pdata)
             if(associated(pos))deallocate(pos)
             return
          endif
          ! Sample particles
          if(rinfo%do_sampling)then
             nkeep = shrink_array(pos(1,:), mask(:))
             nkeep = shrink_array(pos(2,:), mask(:))
             nkeep = shrink_array(pos(3,:), mask(:))
          else
             nkeep = np
          endif
          res = particle_store_add_data(pdata, species_name(ispecies), &
               pos=pos(1:3,1:nkeep))
          deallocate(pos)
          if(.not.res%success)then
             gadget_eagle_read%success = .false.
             gadget_eagle_read%string  = "Unable to allocate memory"
             call close_snapshot(snap)
             call particle_store_empty(pdata)
             return
          endif

          ! Update progress bar
          call progress_bar_update(sum(prog_weight(1:ispecies-1)) &
               + 0.2*prog_weight(ispecies))

          ! Read velocities
#ifdef READ_VEL
          nullify(vel)
          allocate(vel(3,np), stat=ios)
          if(ios.eq.0)then
             n = read_dataset(snap, ispecies-1, "Velocity", vel, iostat=ios)
             if(ios.ne.0)then
                ! Try alternate spelling
                n = read_dataset(snap, ispecies-1, "Velocities", vel, iostat=ios)
             endif
          endif
          if(ios.ne.0)then
             gadget_eagle_read%success = .false.
             gadget_eagle_read%string  = "Unable to read Velocity dataset"
             call close_snapshot(snap)
             call particle_store_empty(pdata)
             if(associated(vel))deallocate(vel)
             return
          endif
          ! Sample particles
          if(rinfo%do_sampling)then
             nkeep = shrink_array(vel(1,:), mask(:))
             nkeep = shrink_array(vel(2,:), mask(:))
             nkeep = shrink_array(vel(3,:), mask(:))
          else
             nkeep = np
          endif
          res = particle_store_add_data(pdata, species_name(ispecies), &
               vel=vel(1:3,1:nkeep))
          deallocate(vel)
          if(.not.res%success)then
             gadget_eagle_read%success = .false.
             gadget_eagle_read%string  = "Unable to allocate memory"
             call close_snapshot(snap)
             call particle_store_empty(pdata)
             return
          endif
#endif

          ! Update progress bar
          call progress_bar_update(sum(prog_weight(1:ispecies-1)) &
               + 0.4*prog_weight(ispecies))

          ! Read IDs
          nullify(idata)
          allocate(idata(np), stat=ios)
          if(ios.eq.0)then
             n = read_dataset(snap, ispecies-1, "ParticleIDs", idata, &
                  iostat=ios)
          endif
          if(ios.ne.0)then
             gadget_eagle_read%success = .false.
             gadget_eagle_read%string  = "Unable to read ParticleIDs dataset"
             call close_snapshot(snap)
             call particle_store_empty(pdata)
             if(associated(idata))deallocate(idata)
             return
          endif
          ! Sample particles
          if(rinfo%do_sampling)then
             nkeep = shrink_array(idata(:), mask(:))
          else
             nkeep = np
          endif
          res = particle_store_add_data(pdata, species_name(ispecies), &
               prop_name="ID", idata=idata(1:nkeep))
          deallocate(idata)
          if(.not.res%success)then
             gadget_eagle_read%success = .false.
             gadget_eagle_read%string  = "Unable to allocate memory"
             call close_snapshot(snap)
             call particle_store_empty(pdata)
             return
          endif

          ! Read masses, if necessary
          nullify(rdata)
          allocate(rdata(np), stat=ios)
          if(ios.eq.0)then
             if(mpart(ispecies).eq.0.0)then
                n = read_dataset(snap, ispecies-1, "Mass", rdata, iostat=ios)
                if(ios.ne.0)then
                   ! Try alternate spelling
                   n = read_dataset(snap, ispecies-1, "Masses", rdata, iostat=ios)
                endif
             else
                rdata(:) = mpart(ispecies)
             endif
          endif
          if(ios.ne.0)then
             gadget_eagle_read%success = .false.
             gadget_eagle_read%string  = "Unable to read Mass dataset"
             call close_snapshot(snap)
             call particle_store_empty(pdata)
             if(associated(rdata))deallocate(rdata)
             return
          endif
          ! Sample particles
          if(rinfo%do_sampling)then
             nkeep = shrink_array(rdata(:), mask(:))
          else
             nkeep = np
          endif
          res = particle_store_add_data(pdata, species_name(ispecies), &
               prop_name="Mass", rdata=rdata(1:nkeep))
          deallocate(rdata)
          if(.not.res%success)then
             gadget_eagle_read%success = .false.
             gadget_eagle_read%string  = "Unable to allocate memory"
             call close_snapshot(snap)
             call particle_store_empty(pdata)
             return
          endif

          ! Update progress bar
          call progress_bar_update(sum(prog_weight(1:ispecies-1)) &
               + 0.6*prog_weight(ispecies))
 
          ! Read other properties
          do iprop = 1, nextra, 1
             if(read_extra(ispecies, iprop))then
                ! Read the dataset
                select case(extra_type(ispecies, iprop))
                case("INTEGER")
                   allocate(idata(np), stat=ios)
                   if(ios.eq.0)then
                      n = read_dataset(snap, ispecies-1, extra_prop(iprop), &
                           idata, iostat=ios)
                      if(ios.eq.0)then
                         ! Sample particles
                         if(rinfo%do_sampling)then
                            nkeep = shrink_array(idata(:), mask(:))
                         else
                            nkeep = np
                         endif
                         res = particle_store_add_data(pdata, &
                              species_name(ispecies), &
                              prop_name=extra_prop(iprop), &
                              idata=idata(1:nkeep))
                         if(.not.res%success)then
                            gadget_eagle_read%success = .false.
                            gadget_eagle_read%string  = &
                                 "Unable to allocate memory"
                            call close_snapshot(snap)
                            call particle_store_empty(pdata)
                            deallocate(idata)
                            return
                         endif
                      endif
                   endif
                   deallocate(idata)
                case("REAL")
                   allocate(rdata(np), stat=ios)
                   if(ios.eq.0)then
                      n = read_dataset(snap, ispecies-1, extra_prop(iprop), &
                           rdata, iostat=ios)
                      if(ios.eq.0)then
                         ! Sample particles
                         if(rinfo%do_sampling)then
                            nkeep = shrink_array(rdata(:), mask(:))
                         else
                            nkeep = np
                         endif
                         res = particle_store_add_data(pdata, &
                              species_name(ispecies), &
                              prop_name=extra_prop(iprop), &
                              rdata=rdata(1:nkeep))
                         if(.not.res%success)then
                            gadget_eagle_read%success = .false.
                            gadget_eagle_read%string  = &
                                 "Unable to allocate memory"
                            call close_snapshot(snap)
                            call particle_store_empty(pdata)
                            deallocate(rdata)
                            return
                         endif
                      endif
                   endif
                   deallocate(rdata)
                case default
                   gadget_eagle_read%success = .false.
                   gadget_eagle_read%string  = "Dataset has unknown type!"
                   call close_snapshot(snap)
                   call particle_store_empty(pdata)
                   return  
                end select
                if(ios.ne.0)then
                   gadget_eagle_read%success = .false.
                   gadget_eagle_read%string  = "Unable to read "//&
                        trim(extra_prop(iprop))//" dataset"
                   call close_snapshot(snap)
                   call particle_store_empty(pdata)
                   return
                endif
             endif

             ! Update progress bar
             fac = float(iprop)/float(nextra) * 0.4 + 0.6
             call progress_bar_update(sum(prog_weight(1:ispecies-1)) &
                  + fac*prog_weight(ispecies))

             ! Next extra property
          end do

          ! Deallocate sampling mask if used
          if(allocated(mask))deallocate(mask)

       endif
       
       ! Update progress bar
       call progress_bar_update(sum(prog_weight(1:ispecies)))

       ! Next particle type
    end do

    ! Store box size
    call particle_store_set_boxsize(pdata,real(snap%boxsize))

    ! Finished!
    call close_snapshot(snap)
    gadget_eagle_read%success = .true.

    ! Set redshift
    call particle_store_set_time(pdata,time,redshift,expansion,time_unit)

    ! Store ID size for this snapshot
    call particle_store_set_idsize(pdata, id_size)

    call progress_bar_update(1.0)

#else
    ! Should never get here because menu option is greyed out if HDF5 not
    ! available
    call terminate('gadget_eagle_reader - Code was compiled without HDF5 support')

    ! Stop compiler complaining about return value not being set
    gadget_eagle_read%success = .false.
    gadget_eagle_read%string  = "Compiled without HDF5 support"
#endif

    return
  end function gadget_eagle_read

end module gadget_eagle_reader


