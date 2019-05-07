module additional_data
!
! Module to read extra data from a set of ascii / binary / HDF5 files
!

#include "../../config.h"

  use f90_gui
  use gadget_path
  use particle_store
  use string_module
  use return_status
  use sort
  use auxiliary_file_list
  use data_types

  implicit none
  private
  save

  public :: additional_data_open
  public :: additional_data_close
  public :: additional_data_process_events
  public :: additional_data_init
  public :: additional_data_read

  ! Window for adding new files
  type(gui_window) :: window
  logical          :: window_open = .false.

  ! Widgets
  type(gui_entrybox)     :: propname_entry
  type(gui_radio_button) :: integer_button, real_button
  type(gui_entrybox)     :: fname_entry
  type(gui_button)       :: browse_button
  type(gui_radio_button) :: ascii_button, hdf5_button, binary_button
  type(gui_entrybox)     :: nfiles_entry

  ! Ascii file info widgets
  type(gui_entrybox)     :: hlines_entry
  type(gui_entrybox)     :: colno_entry

  ! HDF5 file info widgets
  type(gui_entrybox)     :: dset_entry

  ! Binary file info widgets
  type(gui_entrybox)     :: recno_entry 
  type(gui_radio_button) :: bytes_4_button, bytes_8_button

  ! Buttons for particle types
  type(gui_radio_button), dimension(maxspecies) :: applies_box

  ! Ok button
  type(gui_button) :: ok_button

  ! State of format buttons
  logical :: state_ascii, state_hdf5, state_binary

contains

  subroutine additional_data_init()
!
! Module should be re-initialised when a new simulation is loaded
! so we don't try to read an irrelevant set of files on snapshot change.
!
    implicit none

    naux = 0

    return
  end subroutine additional_data_init


  subroutine additional_data_open(mainwin)
!
! Open the window used to specify the data to load
!
    implicit none
    ! Reference to main window
    type(gui_window) :: mainwin
    ! Widgets
    type(gui_box)    :: hbox, vbox
    type(gui_box)    :: outer_vbox
    type(gui_label)  :: label
    type(gui_box)    :: ascii_box, hdf5_box, binary_box, tmp_box
    ! Internal
    integer          :: i, j
    
    if(window_open)return

    call gui_create_window(window, "Read extra particle properties", &
         parent=mainwin, resize=.false.)
    call gui_packing_mode(expand=.false., fill=.false., spacing=3, &
         position=gui_start)
    call gui_create_box(hbox, window, gui_horizontal)
    call gui_create_box(outer_vbox, hbox, gui_vertical)

    ! Dataset to read
    call gui_create_box(vbox, outer_vbox, gui_vertical, frame=.true., &
         label="Dataset to read")

    ! Name and data type
    call gui_create_box(hbox, vbox, gui_horizontal)
    call gui_create_label(label, hbox, "Name of property to load: ")
    call gui_packing_mode(position=gui_end)
    call gui_create_entrybox(propname_entry, hbox, 20)
    call gui_packing_mode(position=gui_start)
    call gui_create_box(hbox, vbox, gui_horizontal)
    call gui_create_label(label, hbox, "Type of data: ")
    call gui_packing_mode(position=gui_end)
    call gui_create_radio_button(real_button,    hbox, "Real")
    call gui_create_radio_button(integer_button, hbox, "Integer", &
         previous=real_button)
    call gui_packing_mode(position=gui_start)

    ! Boxes for particle types
    call gui_create_box(hbox, vbox, gui_horizontal)
    call gui_create_label(label, hbox, "Applies to particle type: ")
    call gui_packing_mode(position=gui_end)
    j = -1
    do i = maxspecies, 1, -1
       if(j.lt.1)then
          call gui_create_radio_button(applies_box(i), hbox, &
               trim(adjustl(string(i-1))))
       else
          call gui_create_radio_button(applies_box(i), hbox, &
               trim(adjustl(string(i-1))), previous=applies_box(j))  
       endif
       j = i
    end do
    call gui_packing_mode(position=gui_start)
    call gui_radio_button_set_state(applies_box(1), .true.)

    ! File locations
    call gui_create_box(vbox, outer_vbox, gui_vertical, frame=.true., &
         label="Location of file(s)")

    ! Number of files
    call gui_create_box(hbox, vbox, gui_horizontal)
    call gui_create_label(label, hbox, "Number of files: ")
    call gui_packing_mode(position=gui_end)
    call gui_create_entrybox(nfiles_entry, hbox, length=5)
    call gui_packing_mode(position=gui_start)
    
    ! Filename
    call gui_create_box(hbox, vbox, gui_horizontal)
    call gui_create_label(label, hbox, "Name of one file: ")
    call gui_packing_mode(expand=.true.,fill=.true.)
    call gui_create_entrybox(fname_entry, hbox)
    call gui_packing_mode(expand=.false.,fill=.false.)
    call gui_create_button(browse_button,hbox,"Browse")

    ! Label
    call gui_create_box(hbox, vbox, gui_horizontal)
    call gui_create_label(label, hbox, &
         "(assumes Gadget naming convention)")

    ! Box with file format options
    call gui_create_box(vbox, outer_vbox, gui_vertical, frame=.true., &
         label="File format")

    ! Ascii
    call gui_create_box(hbox, vbox, gui_horizontal)
    call gui_create_box(tmp_box, hbox, gui_vertical)
    call gui_create_radio_button(ascii_button, tmp_box, "Plain text")
    call gui_packing_mode(position=gui_end)
    call gui_create_box(ascii_box, hbox, gui_vertical)
    call gui_packing_mode(position=gui_start)
    call gui_create_box(hbox, ascii_box, gui_horizontal)
    call gui_packing_mode(position=gui_end)
    call gui_create_entrybox(hlines_entry, hbox, length=5)
    call gui_create_label(label, hbox, "Header lines: ")
    call gui_packing_mode(position=gui_start)
    call gui_create_box(hbox, ascii_box, gui_horizontal)
    call gui_packing_mode(position=gui_end)
    call gui_create_entrybox(colno_entry, hbox, length=5)
    call gui_create_label(label, hbox, "Column to read: ")
    call gui_packing_mode(position=gui_start)

    ! HDF5
    call gui_create_box(hbox, vbox, gui_horizontal)
    call gui_create_box(tmp_box, hbox, gui_vertical)
    call gui_create_radio_button(hdf5_button, tmp_box, "HDF5", &
         previous=ascii_button)
    call gui_packing_mode(position=gui_end)
    call gui_create_box(hdf5_box, hbox, gui_vertical)
    call gui_packing_mode(position=gui_start)
    call gui_create_box(hbox, hdf5_box, gui_horizontal)
    call gui_packing_mode(position=gui_end)
    call gui_packing_mode(expand=.true.,fill=.true.)
    call gui_create_entrybox(dset_entry, hbox)
    call gui_packing_mode(expand=.false.,fill=.false.)
    call gui_create_label(label, hbox, "Dataset: ")
    call gui_packing_mode(position=gui_start)

    ! Binary
    call gui_create_box(hbox, vbox, gui_horizontal)
    call gui_create_box(tmp_box, hbox, gui_vertical)
    call gui_create_radio_button(binary_button, tmp_box, &
         "Fortran unformatted", previous=hdf5_button)
    call gui_packing_mode(position=gui_end)
    call gui_create_box(binary_box, hbox, gui_vertical)
    call gui_packing_mode(position=gui_start)
    call gui_create_box(hbox, binary_box, gui_horizontal)
    call gui_packing_mode(position=gui_end)
    call gui_create_entrybox(recno_entry, hbox, length=5)
    call gui_create_label(label, hbox, "Record number: ")
    call gui_packing_mode(position=gui_start)
    call gui_create_box(hbox, binary_box, gui_horizontal)
    call gui_packing_mode(position=gui_end)
    call gui_create_radio_button(bytes_8_button, hbox, "8")
    call gui_create_radio_button(bytes_4_button, hbox, "4", &
         previous=bytes_8_button)
    call gui_create_label(label, hbox, "Bytes per particle: ")
    call gui_packing_mode(position=gui_start)
    call gui_radio_button_set_state(bytes_4_button, .true.)

#ifndef HAVE_HDF5
    ! Grey out HDF5 format button if library isn't available
    call gui_set_sensitive(hdf5_button, .false.)
#endif

    ! Read button
    call gui_create_box(hbox, outer_vbox, gui_horizontal)
    call gui_packing_mode(position=gui_end)
    call gui_create_button(ok_button, hbox, " Read data ")
    call gui_packing_mode(position=gui_start)

    ! Display the window and record status
    call gui_show_window(window)
    window_open = .true.
    call update_window()

    return
  end subroutine additional_data_open


  logical function additional_data_process_events(mainwin)
!
! Process events in the window and return true if we need to
! resample the particles.
!
    implicit none
    type(gui_window)  :: mainwin
    logical :: ok
    character(len=500) :: fname

    additional_data_process_events = .false.

    if(.not.window_open)return

    if(gui_window_closed(window))then
       call additional_data_close()
       return
    endif

    if(gui_radio_button_changed(hdf5_button).or. &
         gui_radio_button_changed(ascii_button).or. &
         gui_radio_button_changed(binary_button))then
       call update_window()
    end if

    ! Pop up file selector if browse button is clicked
    if(gui_button_clicked(browse_button))then
       call gui_select_file(mainwin, &
            "Select file to read", &
            gui_file_open, ok, fname)
       if(ok)then
          call gui_entrybox_set_text(fname_entry, trim(fname))
       endif
    endif

    ! Add the new file(s) if ok button is pressed
    if(gui_button_clicked(ok_button))then
       call additional_data_add_file()
       ! Will need to resample if we read new data - signal by
       ! returning true.
       additional_data_process_events = .true.
    endif

    return
  end function additional_data_process_events


  subroutine additional_data_close()
!
! Close the window
!
    implicit none

    if(.not.window_open)return

    call gui_destroy_window(window)
    window_open = .false.

    return
  end subroutine additional_data_close


  subroutine update_window()
!
! Update the state of the widgets in the window
!
    implicit none
    integer(kind=index_kind), dimension(maxspecies) :: np
    integer :: nspecies
    integer :: i

    ! Get button states
    call gui_radio_button_get_state(ascii_button,state_ascii)
    call gui_radio_button_get_state(hdf5_button,state_hdf5)
    call gui_radio_button_get_state(binary_button,state_binary)

    ! Set sensitivity of widgets
    call gui_set_sensitive(colno_entry, state_ascii)
    call gui_set_sensitive(hlines_entry, state_ascii)
    call gui_set_sensitive(dset_entry, state_hdf5)
    call gui_set_sensitive(recno_entry, state_binary)
    call gui_set_sensitive(bytes_4_button, state_binary)
    call gui_set_sensitive(bytes_8_button, state_binary)

    call particle_store_contents(pdata, get_np=np, get_nspecies=nspecies)
    do i = 1, maxspecies, 1
       if(i.le.nspecies)then
          call gui_set_sensitive(applies_box(i), (np(i).gt.0))
       else
          call gui_set_sensitive(applies_box(i), .false.)
       endif
    end do

    return
  end subroutine update_window


  subroutine additional_data_add_file()
!
! Extract values from the widgets and, if all goes well, add a new file
! to be read
!
    implicit none
    type (auxiliary_file) :: aux
    character(len=100)    :: str
    integer               :: ios
    character(len=20)     :: bt
    character(len=500)    :: fname
    type(result_type)     :: res
    integer               :: isnap, nspecies
    integer               :: i
    logical               :: state

    ! Check which particles this property applies to
    call particle_store_contents(psample,get_nspecies=nspecies)
    do i = 1, nspecies, 1
       call gui_radio_button_get_state(applies_box(i), state)
       if(state)aux%ispecies = i
    end do

    ! Check we have a name for this property
    call gui_entrybox_get_text(propname_entry, aux%prop_name)
    if(len_trim(aux%prop_name).eq.0)then
       bt=gui_display_dialog(window,"error", &
            "The name of the new particle property must be specified")
       return
    endif

    ! Get number of files
    call gui_entrybox_get_value(nfiles_entry, aux%nfiles, ios)
    if(ios.ne.0)then
       bt=gui_display_dialog(window,"error", &
            "Unable to interpret number of files")
       return  
    endif
    if(aux%nfiles.lt.1)then
       bt=gui_display_dialog(window,"error", &
            "Number of files must be greater than zero")
       return
    endif

    ! Determine selected file format and extract relevant values
    ! Ascii
    if(state_ascii)then
       call gui_entrybox_get_value(hlines_entry,aux%header_lines, ios)
       if(ios.ne.0)then
          bt=gui_display_dialog(window,"error", &
               "Unable to interpret number of header lines")
          return
       endif
       call gui_entrybox_get_value(colno_entry,aux%column_no, ios)
       if(ios.ne.0)then
          bt=gui_display_dialog(window,"error", &
               "Unable to interpret column number")
          return
       endif
       if(aux%column_no.le.0)then
          bt=gui_display_dialog(window,"error", &
               "Column number must be greater than zero")
          return
       endif
       if(aux%header_lines.lt.0)then
          bt=gui_display_dialog(window,"error", &
               "Column number must not be negative")
          return
       endif
       aux%format="ASCII"
    endif
    ! HDF5
    if(state_hdf5)then
       call gui_entrybox_get_text(dset_entry,str)
       if(len_trim(str).eq.0)then
          bt=gui_display_dialog(window,"error", &
               "Dataset name must be specified to read HDF5 files")
          return
       else
          aux%dataset_name = trim(str)
       endif
       aux%format="HDF5"
    endif
    ! Binary
    if(state_binary)then
       call gui_entrybox_get_value(recno_entry, aux%record_no, ios)
       if(ios.ne.0)then
          bt=gui_display_dialog(window,"error", &
               "Unable to interpret record number")
          return
       endif
       if(aux%record_no.le.0)then
          bt=gui_display_dialog(window,"error", &
               "Record number must be greater than zero")
          return
       endif
       aux%format="BINARY"
    endif

    ! Extract the filename and make sure we understand it
    call gui_entrybox_get_text(fname_entry,fname)
    res = gadget_path_extract(fname, isnap, aux%path_data)
    if(.not.res%success)then
       bt=gui_display_dialog(window,"error", &
            "Invalid filename")
       return
    endif

    ! Determine data type
    call gui_radio_button_get_state(integer_button,state)
    if(state)then
       aux%type = "INTEGER"
    else
       aux%type = "REAL"
    end if

    ! If we get this far everything looks ok, so add the file set to the list
    ! and attemp to read it
    naux = naux + 1
    if(naux.gt.nauxmax)then
       bt=gui_display_dialog(window,"error", &
            "Reached maximum number of additional files")
       naux = naux - 1
       return
    endif
    aux_file(naux) = aux

    ! Attempt to read the data
    res = additional_data_read(naux)
    if(.not.res%success)then
       ! Failed - don't try to read this again (e.g. on snapshot change)
       naux = naux - 1
       ! Pass error message from read routine back to user
       bt=gui_display_dialog(window,"error", res%string)
    else
       ! Success message
       bt=gui_display_dialog(window,"info", "Finished reading data")
    endif

    return
  end subroutine additional_data_add_file


  type (result_type) function additional_data_read(iaux)
!
! Read one or all of the entries in the list of auxiliary data files
!
    implicit none
    integer, optional :: iaux
    integer,              dimension(nauxmax) :: idx
    type(auxiliary_file), dimension(nauxmax) :: tmp_aux
    character(len=500),   dimension(nauxmax) :: fname
    integer :: i, j

    ! If we're reading all the files, sort by filename so that datasets
    ! read from the same file can be read at the same time. This should
    ! be more efficient, especially in the case of reading multiple
    ! columns from an ascii file.
    if(.not.present(iaux))then
       ! Sort on path to snapshot 0, file 0 so that the sorting order
       ! doesn't depend on which file in the set the user selected.
       do i = 1, naux, 1
          call gadget_path_generate(0, 0, &
               fname(i), &
               aux_file(i)%path_data)
          ! Add on format in case the same file has been specified with
          ! different formats (can't see how this could be valid though!)
          fname(i) = trim(fname(i))//trim(aux_file(i)%format)
       end do
       call sort_index(fname(1:naux), idx(1:naux))
       do i = 1, naux, 1
          tmp_aux(i) = aux_file(idx(i))
       end do
       aux_file = tmp_aux
    endif

    if(present(iaux))then

       ! Read file iaux
       call read_aux(iaux,iaux)

    else
       i = 1
       do while(i.le.naux)
          j = i
          if(j+1.le.naux)then
             do while(fname(i).eq.fname(j+1))
                j = j + 1
                if(j.eq.naux)exit
             end do
          endif

          ! Read i to j, which are in the same file
          call read_aux(i,j)

          i = j + 1
       end do
    endif
    
    ! Deallocate any memory allocated for properties that turned out
    ! to be unreadable
    call particle_store_cleanup(pdata)

    ! Check we left the particle store in a consistent state
    call particle_store_verify(pdata)

    ! Report any problems
    if(all(aux_file(1:naux)%ok))then
       additional_data_read%success = .true.
    else
       additional_data_read%success = .false.
       additional_data_read%string = "Failed to read file"
       if(present(iaux))then
          additional_data_read%string = trim(additional_data_read%string) // &
               ": "//trim(aux_file(iaux)%error)
       endif
    endif

    return

  contains

    subroutine read_aux(i,j)
!
! Read the specified range of files
!
      implicit none
      integer :: i, j

      aux_file(i:j)%ok        = .true.
      aux_file(i:j)%allocated = .false.
      aux_file(i:j)%nread     = 0

      select case(aux_file(i)%format)
      case("HDF5")
#ifdef HAVE_HDF5
         call additional_data_hdf5(i,j)
#else
         call terminate('Code was compiled without HDF5 support')
#endif
      case("BINARY")
         aux_file(i:j)%ok        = .false.
         aux_file(i:j)%allocated = .false.
         aux_file(i:j)%nread     = 0
         aux_file(i:j)%error     = "Binary reader not implemented yet"
         return
      case("ASCII")
         call additional_data_ascii(i,j)
      end select

      return
    end subroutine read_aux

  end function additional_data_read

#ifdef HAVE_HDF5

  subroutine additional_data_hdf5(i,j)
!
! Read extra data from a HDF5 file or file set
!
    use read_hdf5

    implicit none
    integer            :: i, j, iaux, nprops
    character(len=500) :: fname
    integer            :: ifile, isnap, istat
    character(len=maxlen), dimension(maxprops) :: propnames, species_names
    type(result_type)  :: res
    integer(kind=index_kind), dimension(maxspecies) :: np
    ! Data buffer for reading
    integer(kind=index_kind) :: ntoread
    integer(i_prop_kind), dimension(:), allocatable :: idata
    real(r_prop_kind),    dimension(:), allocatable :: rdata
    ! HDF5 stuff
    integer               :: rank
    integer(kind=int8byte), dimension(7) :: dims, maxdims
    integer               :: hdferr
    integer(kind=int8byte), dimension(7) :: start, count

    ! Get snapshot number
    call particle_store_contents(pdata, get_isnap=isnap, &
         get_species_names=species_names)

    ! Allocate storage for the new properties
    do iaux = i, j, 1

       ! Check that the property name is unique
       call particle_store_species(pdata, &
            aux_file(iaux)%ispecies, &
            get_propnames = propnames, get_nprops=nprops)
       
       if(any(propnames(1:nprops).eq.aux_file(iaux)%prop_name))then
          ! Property name is already taken, so we can't read this one
          aux_file(iaux)%ok     = .false.
          aux_file(iaux)%error = "Duplicate property name"
          cycle
       endif

       ! Add the new property
       res = particle_store_new_property(pdata, &
            species_names(aux_file(iaux)%ispecies), &
            aux_file(iaux)%prop_name, aux_file(iaux)%type)

       ! May fail if we run out of memory
       if(.not.res%success)then
          aux_file(iaux)%ok        = .false.
          aux_file(iaux)%allocated = .false.
          aux_file(iaux)%error     = "Memory allocation failed"
          cycle
       else
          aux_file(iaux)%allocated = .true.
       endif

       ! Next property to load
    end do

    ! Loop over files in this set
    do ifile = 0, aux_file(i)%nfiles-1, 1
       
       ! Generate the path to this file
       call gadget_path_generate(isnap, ifile, &
            fname, aux_file(i)%path_data)
       
       ! Open the file
       hdferr = hdf5_open_file(fname)

       ! If that failed, we're not going to be able to read anything
       ! from this set so flag it as unreadable
       if(hdferr.ne.0)then
          aux_file(i:j)%ok     = .false.
          aux_file(i:j)%error = "Unable to open file"
          return
       endif

       ! Loop over datasets to read
       do iaux = i, j, 1
          if(aux_file(iaux)%ok)then
             
             ! Open the dataset and determine its size
             hdferr = hdf5_dataset_size(aux_file(iaux)%dataset_name, &
                  rank, dims)
             if(hdferr.lt.0)then
                aux_file(iaux)%ok    = .false.
                aux_file(iaux)%error = "Unable to get dataset dimensions"
                cycle
             endif
             if(rank.ne.1)then
                aux_file(iaux)%ok    = .false.
                aux_file(iaux)%error = "Dataset is not one dimensional"
                cycle
             endif
             
             ! Calculate number of elements to read
             call particle_store_contents(pdata, get_np=np)

             ntoread = np(aux_file(iaux)%ispecies) - aux_file(iaux)%nread
             ntoread = min(int(ntoread), int(dims(1), kind=index_kind))

             ! Go on to next property if there are no elements to read
             ! from this file
             if(ntoread.eq.0)then
                cycle
             endif

             ! Select elements to be read from the file
             start(1) = 0
             count(1) = ntoread

             ! Allocate temporary buffer and read the data
             select case(aux_file(iaux)%type)
             case("REAL")

                allocate(rdata(ntoread),stat=istat)
                if(istat.ne.0)then
                   aux_file(iaux)%ok    = .false.
                   aux_file(iaux)%error = "Failed to allocate memory"
                   cycle
                endif
                hdferr = hdf5_read_dataset(aux_file(iaux)%dataset_name, rdata, &
                     start=start, count=count)
                if(hdferr.ne.0)then
                   aux_file(iaux)%ok    = .false.
                   aux_file(iaux)%error = "Unable to read dataset"
                   deallocate(rdata)
                   cycle
                endif
                res = particle_store_add_data(pdata, &
                     species_names(aux_file(iaux)%ispecies), &
                     aux_file(iaux)%prop_name, rdata=rdata)
                deallocate(rdata)
                if(.not.res%success)then
                   aux_file(iaux)%ok    = .false.
                   aux_file(iaux)%error = "Failed to allocate memory"
                   cycle
                endif
             case("INTEGER")

                allocate(idata(ntoread),stat=istat)
                if(istat.ne.0)then
                   aux_file(iaux)%ok    = .false.
                   aux_file(iaux)%error = "Failed to allocate memory"
                   cycle
                endif
                hdferr = hdf5_read_dataset(aux_file(iaux)%dataset_name, idata, &
                     start=start, count=count)
                if(hdferr.ne.0)then
                   aux_file(iaux)%ok    = .false.
                   aux_file(iaux)%error = "Unable to read dataset"
                   deallocate(idata)
                   cycle
                endif
                res = particle_store_add_data(pdata, &
                     species_names(aux_file(iaux)%ispecies), &
                     aux_file(iaux)%prop_name, idata=idata)
                deallocate(idata)
                if(.not.res%success)then
                   aux_file(iaux)%ok    = .false.
                   aux_file(iaux)%error = "Failed to allocate memory"
                   cycle
                endif
             end select

          endif

          ! Next property to read
       end do

       hdferr = hdf5_close_file()

       ! Next file
    end do

    return
  end subroutine additional_data_hdf5

#endif


  subroutine additional_data_ascii(i,j)
!
! Read extra data from an ascii file or file set
!
    implicit none
    integer            :: i, j, iaux, nprops, ios
    character(len=500) :: fname
    integer            :: ifile, isnap
    integer(kind=index_kind) :: nread
    character(len=maxlen), dimension(maxprops) :: propnames, species_names
    type(result_type)  :: res
    integer(kind=index_kind), dimension(maxspecies) :: np
    ! Line from the file
    integer, parameter         :: line_maxlen = 1000
    character(len=line_maxlen) :: line
    ! Arrays of pointers to the data arrays
    type(r_prop_ptr_type), dimension(:), allocatable :: rprop
    type(i_prop_ptr_type), dimension(:), allocatable :: iprop
    ! Columns
    integer, parameter          :: ncolmax = 100
    integer, dimension(ncolmax) :: icol
    integer                     :: ncol
    integer                     :: jcol
    ! Number of particles to read data for
    integer :: nspecies, ispecies
    character(len=500) :: col
    integer :: istat

    ! Get snapshot number
    call particle_store_contents(pdata, get_isnap=isnap, &
         get_species_names=species_names, get_np=np, get_nspecies=nspecies)

    icol = 0
    ncol = 0

    ! Allocate storage for the new properties
    do iaux = i, j, 1

       ! Check that the property name is unique
       call particle_store_species(pdata, &
            aux_file(iaux)%ispecies, &
            get_propnames = propnames, get_nprops=nprops)
       
       if(any(propnames(1:nprops).eq.aux_file(iaux)%prop_name))then
          ! Property name is already taken, so we can't read this one
          aux_file(iaux)%ok     = .false.
          aux_file(iaux)%error = "Duplicate property name"
          cycle
       endif

       ! Add the new property
       res = particle_store_new_property(pdata, &
            species_names(aux_file(iaux)%ispecies), &
            aux_file(iaux)%prop_name, aux_file(iaux)%type)

       ! May fail if we run out of memory
       if(.not.res%success)then
          aux_file(iaux)%ok        = .false.
          aux_file(iaux)%allocated = .false.
          aux_file(iaux)%error     = "Memory allocation failed"
          cycle
       else
          aux_file(iaux)%allocated = .true.
       endif

       ! Record correspondence between columns and properties
       icol(aux_file(iaux)%column_no) = iaux
       ncol = max(ncol,aux_file(iaux)%column_no)

       ! Check we don't have too more columns than we can handle
       if(ncol.gt.ncolmax)then
          aux_file(iaux)%ok        = .false.
          aux_file(iaux)%allocated = .true.
          aux_file(iaux)%error     = "Too many columns!"
          cycle
       endif

       ! Next property to load
    end do

    ! Allocate read buffers
    allocate(iprop(i:j), rprop(i:j), stat=istat)
    if(istat.ne.0)then
       aux_file(i:j)%ok    = .false.
       aux_file(i:j)%error = "Unable to allocate read buffers!"       
       return
    endif

    ! Allocate storage for data
    istat = 0
    do iaux = i, j, 1
       ispecies = aux_file(iaux)%ispecies
       select case(aux_file(iaux)%type)
       case("INTEGER")
          allocate(iprop(iaux)%ptr(np(ispecies)), stat=istat)
       case("REAL")
          allocate(rprop(iaux)%ptr(np(ispecies)), stat=istat)
       case default
          call terminate('Unknown property type in additional_data_ascii()')
       end select
       if(istat.ne.0)exit
    end do
    
    ! On failure, deallocate and return error code
    if(istat.ne.0)then
       do iaux = i, j, 1
          select case(aux_file(iaux)%type)
          case("INTEGER")
             deallocate(iprop(iaux)%ptr)
          case("REAL")
             deallocate(rprop(iaux)%ptr)
          end select
       end do
       aux_file(i:j)%ok    = .false.
       aux_file(i:j)%error = "Unable to allocate memory!"       
       return
    endif
    
    ! No. of lines read so far
    nread = 0

    ! Loop over files in this set
    do ifile = 0, aux_file(i)%nfiles-1, 1
       
       ! Generate the path to this file
       call gadget_path_generate(isnap, ifile, &
            fname, aux_file(i)%path_data)
       
       ! Open the file
       open(unit=1,file=fname,status="old",form="formatted",iostat=ios)

       ! If that failed, we're not going to be able to read anything
       ! from this set so flag it as unreadable
       if(ios.ne.0)then
          aux_file(i:j)%ok     = .false.
          aux_file(i:j)%error = "Unable to open file"
          call cleanup()
          return
       endif

       ! Skip header lines
       do i = 1, aux_file(i)%header_lines, 1
          read(1,*,iostat=ios)
       end do
       if(ios.ne.0)then
          aux_file(i:j)%ok     = .false.
          aux_file(i:j)%error = "Unable to read from file"
          call cleanup()
          return
       endif

       ! Read lines until there are no more or we have all the data
       ios = 0
       do while(ios.eq.0)
          read(1,'(a)',iostat=ios)line
          if(ios.eq.0.and.len_trim(line).gt.0)then
             nread = nread + 1
             jcol = 0
             do while(len_trim(line).gt.0.and.jcol.le.ncol)
                jcol = jcol + 1
                call extract_next_column(line, col)
                if(icol(jcol).gt.0)then
                   if(nread.gt.np(aux_file(icol(jcol))%ispecies))then
                      aux_file(i:j)%ok     = .false.
                      aux_file(i:j)%error = "There are too many lines"
                      call cleanup()
                      close(1)
                      return
                   endif
                   select case(aux_file(icol(jcol))%type)
                   case("INTEGER")
                      read(col,*,iostat=ios)iprop(icol(jcol))%ptr(nread)
                   case("REAL")
                      read(col,*,iostat=ios)rprop(icol(jcol))%ptr(nread)
                   case default
                      call terminate('Unknown data type in additional_data_ascii')
                   end select
                   if(ios.ne.0)then
                      aux_file(i:j)%ok     = .false.
                      aux_file(i:j)%error = "Unable to interpret line "// &
                           trim(col)
                      call cleanup()
                      close(1)
                      return
                   endif
                endif
                ! Next column
             end do
             if(jcol.lt.ncol)then
                aux_file(i:j)%ok     = .false.
                aux_file(i:j)%error = "Line has too few columns"
                call cleanup()
                close(1)
                return
             endif
          endif
          ! Next line
       end do

       ! Close the file
       close(1)

       ! Next file
    end do

    ! Check we read in enough data
    do iaux = i, j, 1
       ispecies = aux_file(iaux)%ispecies
       if(nread.lt.np(ispecies))then
          if(jcol.lt.ncol)then
             aux_file(i:j)%ok     = .false.
             aux_file(i:j)%error = "There are too few lines"
             call cleanup()
             return
          endif
       endif
    end do

    ! Store results
    do iaux = i, j, 1
       select case(aux_file(iaux)%type)
       case("INTEGER")
          res = particle_store_add_data(pdata, &
               species_names(aux_file(iaux)%ispecies), &
               aux_file(iaux)%prop_name, idata=iprop(iaux)%ptr)
       case("REAL")
          res = particle_store_add_data(pdata, &
               species_names(aux_file(iaux)%ispecies), &
               aux_file(iaux)%prop_name, rdata=rprop(iaux)%ptr)
       end select
       if(.not.res%success)then
          aux_file(iaux)%ok    = .false.
          aux_file(iaux)%error = "Failed to allocate memory"
       endif
    end do

    ! Deallocate
    call cleanup()

    return

  contains

    subroutine cleanup()

      implicit none

      ! Deallocate read buffers
      do iaux = i, j, 1
         select case(aux_file(iaux)%type)
         case("INTEGER")
            deallocate(iprop(iaux)%ptr)
         case("REAL")
            deallocate(rprop(iaux)%ptr)
         end select
      end do
      deallocate(iprop,rprop)
        
      return
    end subroutine cleanup

  end subroutine additional_data_ascii


  subroutine extract_next_column(str, col)
!
! Remove the first number (e.g. a real or integer) from string str
! and return it as a string 
!
    implicit none
    character(len=18), parameter :: num_chars = "01234567890eEdD+-."
    character(len=*)           :: str, col
    integer                    :: i, j

    col = ""
    i = scan(str,num_chars)
    if(i.lt.1)return
    j = verify(str(i:),num_chars)
    if(j.eq.0)j = len_trim(str(i:))+1
    col = str(i:i+j-2)
    if(i+j-1.le.len_trim(str))then
       str = trim(str(i+j-1:))
    else
       str = ""
    endif

    return
  end subroutine extract_next_column


end module additional_data
