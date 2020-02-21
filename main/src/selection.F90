module selection
!
! Module to create and maintain lists of the IDs of selected particles
! and use them to update the 'selected' array in the particle
! data structure.
!
! Separate lists are maintained for each particle type.
!
! TODO: correctly handle selections of >=2**31 particles!
!
#include "../../config.h"

  use f90_gui
  use data_types
  use particle_store
  use sort
  use view_parameters
  use transform
  use return_status
  use string_module
  use data_array
  use format_strings

  implicit none
  private
  save

  ! Callable routines in this module
  public :: selection_init
  public :: selection_clear_all
  public :: selection_apply_all
  public :: selection_open
  public :: selection_close
  public :: selection_process_events
  public :: selection_recentre_view
  public :: selection_get_colour
  public :: selection_is_empty
  public :: selection_get_names
  public :: selection_dump_state
  public :: selection_restore_state
  public :: selection_update_summary
  public :: selection_set_keys
  public :: selection_get_keys
  public :: selection_update_window

  ! Kind used to store IDs - at the moment they're the same type of integer
  ! as any other integer particle property
  integer, parameter :: id_kind = i_prop_kind
  
  ! Data type to describe a list of IDs for one particle type
  type idlist_type
     private
     integer(kind=index_kind) :: np
     integer(kind=id_kind), pointer, dimension(:) :: id
  end type idlist_type

  ! Data type to describe selected particles for a complete dataset
  type selection_type
     private
     logical :: empty
     integer :: nspecies
     type(idlist_type), dimension(maxspecies) :: idlist
  end type selection_type

  ! Public data types
  public :: idlist_type
  public :: selection_type

  ! Widgets for the particle selection window
  type (gui_window)       :: window
  logical                 :: is_open = .false.
  type (gui_combo_box)    :: species_box, prop_box, read_species_box
  type (gui_checkbox)     :: radius_checkbox, prop_checkbox
  type (gui_entrybox)     :: radius_entry, val_min_entry, val_max_entry
  type (gui_radio_button) :: new_radio, subset_radio, add_radio
  type (gui_radio_button) :: full_radio, sample_radio
  type (gui_button)       :: clear_button, apply_button, clear_all_button, centre_button
  type (gui_combo_box)    :: selection_box
  type (gui_colour_button) :: cbutton
  type (gui_entrybox)     :: name_entry
  type (gui_entrybox)     :: np_box
  type (gui_entrybox)     :: fname_box
  type (gui_checkbox)     :: centre_checkbox
  type (gui_button)       :: read_button, browse_button

  ! State of the widgets in the window
  integer            :: ispecies, iprop, ispecies_read
  logical            :: use_radius, use_prop
  character(len=500) :: radius_text, val_min_text, val_max_text
  logical            :: new, subset, add, full, sample
  integer            :: isel = 0

  ! Notebook widgets
  type (gui_notebook) :: notebook
  type (gui_textview) :: textview

  ! Set of selections
  integer, parameter, public                    :: nselmax = 6
  character(len=maxlen), dimension(0:nselmax-1) :: selection_name

  ! Selection for the sampled particles
  type(selection_type), dimension(0:nselmax-1) :: sample_sel

  ! Colours to use for display
  integer, dimension(3,0:nselmax-1) :: icol

  ! Whether each selection should be displayed
  logical, dimension(0:nselmax-1), public :: display_selection = .true.

  ! Whether each selection should be used to centre snapshots
  logical, dimension(0:nselmax-1) :: use_centre = .true.

  ! Whether only selected particles should be drawn
  logical, public :: draw_selected_only = .false.

  ! Whether to recentre on snapshot change
  logical, public :: follow_selection = .true.

contains

  subroutine selection_init()
!
! Initialise a newly declared selection object
!
    implicit none
    integer :: i, j

    sample_sel(0:nselmax-1)%nspecies = 0
    sample_sel(0:nselmax-1)%empty    = .true.

    do i = 0, nselmax-1, 1
       write(selection_name(i),'(a,1i2.2)')"Selection",i+1
       sample_sel(i)%idlist%np = 0
       do j = 1, maxspecies, 1
          nullify(sample_sel(i)%idlist(j)%id)
       end do
    end do

    ! Default highlight colours
    icol(1:3, 0) = (/ 255, 255, 255 /) ! White
    icol(1:3, 1) = (/ 255, 255, 000 /) ! Yellow
    icol(1:3, 2) = (/ 255, 000, 255 /) ! Magenta
    icol(1:3, 3) = (/ 000, 255, 255 /) ! Cyan
    icol(1:3, 4) = (/ 000, 255, 000 /) ! Green
    icol(1:3, 5) = (/ 000, 000, 255 /) ! Blue

    return
  end subroutine selection_init


  subroutine selection_clear(sel, pdata)
!
! Set an empty selection. If pdata is supplied it is used to determine
! how many particle types there are.
!
    implicit none
    integer              :: nspecies
    type(pdata_type), optional :: pdata
    type(selection_type) :: sel
    integer              :: i

    ! Deallocate any existing data
    do i = 1, sel%nspecies, 1
       if(associated(sel%idlist(i)%id))deallocate(sel%idlist(i)%id)
    end do

    if(present(pdata))then

       ! Get number of particle types
       call particle_store_contents(pdata, get_nspecies=nspecies)

       ! Allocate appropriate number of ID lists
       sel%nspecies = nspecies
       
       do i = 1, nspecies, 1
          ! Nullify pointers to ID arrays
          nullify(sel%idlist(i)%id)
          ! Set particle number to zero
          sel%idlist(i)%np = 0
       end do

    else
       sel%nspecies = 0
    endif

    sel%empty    = .true.

    return
  end subroutine selection_clear


  type (result_type) function selection_apply(sel, pdata, isel)
!
! Use the supplied selection object to update the 'selected' arrays in pdata
!
    implicit none
    type(selection_type) :: sel
    type(pdata_type)     :: pdata
    integer              :: isel
    integer(kind=index_kind), dimension(:), allocatable :: idx1, idx2
    integer :: ispecies
    integer, pointer, dimension(:) :: selected
    integer :: nspecies
    integer(kind=index_kind) :: np, nsel
    integer(kind=id_kind), pointer, dimension(:) :: part_id, sel_id
    integer(kind=index_kind) :: i1, i2
    integer :: status
    integer :: mask, invmask

    ! Check number of particle types is consistent
    call particle_store_contents(pdata, get_nspecies=nspecies)

    if(sel%nspecies.ne.nspecies) &
         call terminate('selection_apply(): nspecies incorrect')
    
    ! Bitmask for this selection
    mask    = ibset(0, isel)
    invmask = not(mask)

    ! Loop over particle types
    do ispecies = 1, nspecies, 1
       
       ! Get selection ID arrays and particle numbers
       call particle_store_species(pdata, ispecies, get_selected=selected, &
            get_np=np, get_id=part_id)
       nsel   =  sel%idlist(ispecies)%np
       sel_id => sel%idlist(ispecies)%id
       
       ! Set all particles as unselected in this selection
       do i1 = 1, np, 1
          selected(i1) = iand(selected(i1),invmask)
       end do

       ! Don't waste time sorting if there are no selected particles
       ! or no particles at all
       if(.not.sel%empty.and.np.gt.0)then

          ! Then locate particles in the ID list and select them
          if(nsel.gt.0)then
             allocate(idx1(np), idx2(nsel), stat=status)
             if(status.ne.0)then
                ! No memory for this operation. Clear the selection
                ! and return failure
                call selection_clear(sel,pdata)
                selection_apply%success = .false.
                selection_apply%string = "Not enough memory to apply selection"
                return
             endif
             call openmp_sort_index(part_id(1:np),  idx1(1:np))
             call openmp_sort_index(sel_id(1:nsel), idx2(1:nsel))
             i1 = 1
             do i2 = 1, nsel, 1
                do while(part_id(idx1(i1)).lt.sel_id(idx2(i2)).and.i1.lt.np)
                   i1 = i1 + 1
                end do
                if(part_id(idx1(i1)).eq.sel_id(idx2(i2)))then
                   selected(idx1(i1)) = ior(selected(idx1(i1)),mask)
                endif
             end do
             deallocate(idx1, idx2)
          endif

       endif

       ! Next particle type
    end do

    selection_apply%success = .true.

    return
  end function selection_apply



  type (result_type) function selection_find_particles( &
       sel, pdata, ispecies, centre, radius, &
       iprop, rval_min, rval_max, ival_min, ival_max)
!
! This creates a selection object containing the IDs of particles
! that satisfy the specified criteria.
!
! Particles must be within distance radius of centre(1:3) if radius > 0
! and must have property iprop between val_min and val_max if iprop > 0.
!
! This doesn't update the selection array in the pdata object.
!

    implicit none
    type(selection_type)              :: sel
    type(pdata_type)                  :: pdata
    integer                           :: ispecies
    real(kind=pos_kind)               :: radius
    real(kind=pos_kind), dimension(3) :: centre
    real(kind=real8byte)              :: rval_min, rval_max
    integer(kind=int8byte)            :: ival_min, ival_max
    integer                           :: iprop
    integer(kind=index_kind)          :: i, np, ntot
    integer                           :: nspecies
    real                              :: boxsize
   ! Pointers to data arrays
    real(kind=pos_kind),       dimension(:,:), pointer :: pos
    real(kind=r_prop_kind),    dimension(:),   pointer :: rdata
    integer(kind=i_prop_kind), dimension(:),   pointer :: idata, id
    character(len=20) :: type
    logical           :: is_selected
    integer           :: status

    ! Clear the selection
    call selection_clear(sel, pdata)

    ! Get number of particle types
    call particle_store_contents(pdata, get_nspecies=nspecies)
    call particle_store_get_boxsize(pdata, boxsize)

    ! Get data arrays
    call particle_store_species(pdata, ispecies, get_np=np, get_id=id)
    
    ! Go through the particles and tag those that satisfy the conditions.
    ! First get pointers to the necessary data.
    if(radius.gt.0.0)then
       call particle_store_species(pdata, ispecies, get_pos=pos)
    else
       nullify(pos)
    endif
    if(iprop.gt.0)then
       call particle_store_property(pdata, ispecies, iprop, get_type=type)
       select case(type)
       case("INTEGER")
          call particle_store_property(pdata, ispecies, iprop, get_idata=idata)
          nullify(rdata)
       case("REAL")
          call particle_store_property(pdata, ispecies, iprop, get_rdata=rdata)
          nullify(idata)
       case default
          call terminate('selection_find_particles() - unrecognised property type')
       end select
    else
       nullify(rdata,idata)
    endif

    ! Loop over the particles
    ! Just count them on first pass
    ntot = 0
!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(is_selected) REDUCTION(+:ntot)
    do i = 1, np, 1
       is_selected = .true.
       ! Check if particle satisfies position criterion
       if(associated(pos).and.radius.gt.0.0)then
          if(boxsize.gt.0.0)then
             ! Periodic simulation
             if(mindist2(boxsize, pos(1:3,i), centre(1:3)).gt.radius**2)&
                  is_selected = .false.
          else
             ! Non-periodic
             if(sum((pos(1:3,i)-centre(1:3))**2).gt.radius**2) &
                  is_selected = .false.
          endif
       endif
       ! Check property values
       if(iprop.gt.0)then
          ! Check if integer property value is in range
          if(associated(idata))then
             if(idata(i).lt.ival_min.or.idata(i).gt.ival_max) &
                  is_selected = .false.
          endif
          ! Check if real property value is in range
          if(associated(rdata))then
             if(rdata(i).lt.rval_min.or.rdata(i).gt.rval_max) &
                  is_selected = .false.
          endif
       endif
       if(is_selected)ntot = ntot + 1
          
       ! Next particle
    end do
!$OMP END PARALLEL DO

    allocate(sel%idlist(ispecies)%id(ntot),stat=status)
    if(status.ne.0)then
       ! No memory left, so clear the selection and return failure
       call selection_clear(sel,pdata)
       selection_find_particles%success = .false.
       selection_find_particles%string = &
            "Failed to allocate memory for IDs of selected particles"
       return
    endif

    ! TODO: Critical section probably means this doesn't scale at all well.
    !       Find a better way to parallelise this.

    ntot = 0
!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(is_selected)
    do i = 1, np, 1
       is_selected = .true.
       ! Check if particle satisfies position criterion
       if(associated(pos).and.radius.gt.0.0)then
          if(boxsize.gt.0.0)then
             ! Periodic simulation
             if(mindist2(boxsize, pos(1:3,i), centre(1:3)).gt.radius**2)&
                  is_selected = .false.
          else
             ! Non-periodic
             if(sum((pos(1:3,i)-centre(1:3))**2).gt.radius**2) &
                  is_selected = .false.
          endif
       endif
       ! Check property values
       if(iprop.gt.0)then
          ! Check if integer property value is in range
          if(associated(idata))then
             if(idata(i).lt.ival_min.or.idata(i).gt.ival_max) &
                  is_selected = .false.
          endif
          ! Check if real property value is in range
          if(associated(rdata))then
             if(rdata(i).lt.rval_min.or.rdata(i).gt.rval_max) &
                  is_selected = .false.
          endif
       endif
       if(is_selected)then
!$OMP CRITICAL
          ntot = ntot + 1
          sel%idlist(ispecies)%id(ntot) = id(i)
!$OMP END CRITICAL
       endif
          ! Next particle
    end do
!$OMP END PARALLEL DO

    sel%idlist(ispecies)%np = ntot
 
    sel%empty=(ntot.eq.0)

    selection_find_particles%success = .true.

    return
  contains

    real(kind=pos_kind) function mindist2(boxsize, pos1, pos2) result(r2)
!
! Return minimum distance (squared) between two points taking periodic
! boundary into account
!
      implicit none
      ! Input parameters
      real(kind=pos_kind), dimension(3), intent(in) :: pos1, pos2
      real,                              intent(in) :: boxsize
      ! Internal
      real(kind=pos_kind), dimension(3) :: dr
      integer :: i

      dr = abs(pos2 - pos1)
      do i = 1, 3, 1
         if(dr(i).gt.0.5*boxsize)dr(i) = boxsize - dr(i)
      end do
      r2 = sum(dr**2)
      
      return
    end function mindist2
    
  end function selection_find_particles



  type(result_type) function selection_add(sel1,sel2,ispecies)
!
! Add the IDs listed in sel2 to sel1 and return the result in sel1.
!
    implicit none
    type (selection_type) :: sel1, sel2
    integer(kind=index_kind) :: i, i1, i2
    integer(kind=i_prop_kind), dimension(:), pointer :: id1, id2, idtmp
    integer(kind=index_kind), dimension(:), allocatable :: idx1, idx2
    integer(kind=index_kind) :: np1, np2
    integer(kind=index_kind) :: nadd
    integer :: ipass
    integer, optional :: ispecies
    integer :: status

    if(sel1%nspecies.ne.sel2%nspecies)call terminate( &
         'Number of species in selection doesnt match')
    
    sel1%empty = .true.

    status = 0

    do i = 1, sel1%nspecies, 1
       ! If ispecies is specified, leave other particle type selections
       ! unchanged
       if(present(ispecies))then
          if(i.ne.ispecies)cycle
       endif
       if(sel1%idlist(i)%np.eq.0.and.sel2%idlist(i)%np.gt.0)then
          ! Copy selection from sel2 into sel1
          if(associated(sel1%idlist(i)%id))deallocate(sel1%idlist(i)%id)
          allocate(sel1%idlist(i)%id(sel2%idlist(i)%np),stat=status)
          ! If allocation failed, abort
          if(status.ne.0)exit
          sel1%idlist(i)%np = sel2%idlist(i)%np
          sel1%idlist(i)%id = sel2%idlist(i)%id
       else if(sel2%idlist(i)%np.eq.0)then
          ! No selected particles to add to sel1 in this case
          cycle
       else
          ! Have particles in both selections, so we need to merge them
          id1 => sel1%idlist(i)%id
          id2 => sel2%idlist(i)%id
          np1 =  sel1%idlist(i)%np
          np2 =  sel2%idlist(i)%np
          ! Get sorting order by ID
          allocate(idx1(np1),idx2(np2),stat=status)
          ! If allocation failed, abort
          if(status.ne.0)exit
          call openmp_sort_index(id1(1:np1),idx1(1:np1))
          call openmp_sort_index(id2(1:np2),idx2(1:np2))
          ! Advance through ID lists and find particles in sel2 which aren't
          ! in sel1. On the first pass we just count the particles.
          do ipass = 1, 2, 1
             i1 = 1
             nadd = 0
             do i2 = 1, np2, 1
                do while(id1(idx1(i1)).lt.id2(idx2(i2)).and.i1.lt.np1)
                   i1 = i1 + 1
                end do
                if(id1(idx1(i1)).ne.id2(idx2(i2)))then
                   ! If we didn't find a match for particle i2, count it
                   ! and add it to the end of the id1 array.
                   nadd = nadd + 1
                   if(ipass.eq.2)then
                      id1(np1+nadd) = id2(idx2(i2))
                   endif
                endif
             end do
             ! On first pass, resize the id1 array if necessary
             if(nadd.gt.0.and.ipass.eq.1)then
                allocate(idtmp(np1+nadd),stat=status)
                if(status.ne.0)exit
                idtmp(1:np1) = id1(1:np1)
                if(associated(id1))deallocate(id1)
                id1 => idtmp
                sel1%idlist(i)%id => idtmp
                nullify(idtmp)
             endif
          end do
          deallocate(idx1,idx2)
          ! If allocation failed, abort
          if(status.ne.0)exit
          ! Update particle count
          np1 = np1 + nadd
          sel1%idlist(i)%np = np1

       endif

       if(sel1%idlist(i)%np.gt.0)sel1%empty = .false.

       ! Next particle type
    end do

    ! Check if any memory allocations failed
    if(status.ne.0)then
       selection_add%success = .false.
       selection_add%string  = "Not enough memory to combine selections"
       call selection_clear(sel1)
       call selection_clear(sel2)
    else
       selection_add%success = .true.
    endif

    return
  end function selection_add



  type (result_type) function selection_subset(sel1,sel2,ispecies)
!
! Make a new selection which contains only those IDs which are in both sel1
! and sel2. Overwrites sel1.
!
    implicit none
    type (selection_type) :: sel1, sel2
    integer(kind=index_kind) :: i, i1, i2
    integer(kind=i_prop_kind), dimension(:), pointer :: id1, id2, idtmp
    integer(kind=index_kind), dimension(:), allocatable :: idx1, idx2
    integer(kind=index_kind) :: np1, np2
    integer(kind=index_kind) :: nadd
    integer :: ipass
    integer, optional :: ispecies
    integer :: status

    status = 0

    if(sel1%nspecies.ne.sel2%nspecies)call terminate( &
         'Number of species in selection doesnt match')
    
    sel1%empty = .true.

    do i = 1, sel1%nspecies, 1
       ! If ispecies is specified, leave other particle type selections
       ! unchanged
       if(present(ispecies))then
          if(i.ne.ispecies)cycle
       endif
       if(sel1%idlist(i)%np.eq.0.or.sel2%idlist(i)%np.eq.0)then
          ! If either selection has no particles, we should return an
          ! empty selection in sel1
          if(associated(sel1%idlist(i)%id))deallocate(sel1%idlist(i)%id)
          nullify(sel1%idlist(i)%id)
          sel1%idlist(i)%np = 0
       else
          ! Otherwise we need to go through the IDs in order and just 
          ! keep those that match up
          id1 => sel1%idlist(i)%id
          id2 => sel2%idlist(i)%id
          np1 =  sel1%idlist(i)%np
          np2 =  sel2%idlist(i)%np
          ! Get sorting order by ID
          allocate(idx1(np1),idx2(np2),stat=status)
          ! If allocation failed, abort
          if(status.ne.0)exit
          call openmp_sort_index(id1(1:np1),idx1(1:np1))
          call openmp_sort_index(id2(1:np2),idx2(1:np2))
          ! Advance through ID lists and find particles in sel2 which are
          ! in sel1. On the first pass we just count the particles.
          do ipass = 1, 2, 1
             i1 = 1
             nadd = 0
             do i2 = 1, np2, 1
                do while(id1(idx1(i1)).lt.id2(idx2(i2)).and.i1.lt.np1)
                   i1 = i1 + 1
                end do
                if(id1(idx1(i1)).eq.id2(idx2(i2)))then
                   ! If we found a match for particle i2, count it
                   ! and add it to the temporary array.
                   nadd = nadd + 1
                   if(ipass.eq.2)then
                      idtmp(nadd) = id2(idx2(i2))
                   endif
                endif
             end do
             ! On first pass, allocate temporary array
             if(nadd.gt.0.and.ipass.eq.1)then
                allocate(idtmp(nadd),stat=status)
                ! If allocation failed, abort
                if(status.ne.0)exit
             endif
          end do
          deallocate(idx1,idx2)
          ! If allocation failed, abort
          if(status.ne.0)exit
          ! Update ID array and particle number
          sel1%idlist(i)%np = nadd
          ! Deallocate old array of IDs
          deallocate(sel1%idlist(i)%id)
          if(nadd.gt.0)then
             ! Make ID pointer point to the new array
             sel1%idlist(i)%id => idtmp
             nullify(idtmp)
          else
             ! No IDs, so just set null pointer
             nullify(sel1%idlist(i)%id)
          endif

       endif

       if(sel1%idlist(i)%np.gt.0)sel1%empty = .false.

       ! Next particle type
    end do
    
    ! Check if any memory allocations failed
    if(status.ne.0)then
       selection_subset%success = .false.
       selection_subset%string  = "Not enough memory to combine selections"
       call selection_clear(sel1)
       call selection_clear(sel2)
    else
       selection_subset%success = .true.
    endif

    return
  end function selection_subset




  subroutine selection_open(mainwin)
!
! Open window for selecting particles
!
    implicit none
    type(gui_window) :: mainwin
    type(gui_box)    :: hbox, vbox, inner_vbox, outer_hbox, &
         outer_vbox, nbbox
    type(gui_label)  :: label
    logical, save    :: first_call = .true.

    ! Do nothing if window is already open
    if(is_open)return

    ! Create the window and widgets
    call gui_create_window(window, "Select particles", &
         parent=mainwin, resize=.false.)
    call gui_packing_mode(expand=.false., fill=.false., spacing=3, &
         position=gui_start)
    
    ! Particle selection options
    call gui_packing_mode(spacing=5)
    call gui_create_box(outer_hbox, window, gui_horizontal)
    call gui_create_box(outer_vbox, outer_hbox,   gui_vertical)
    call gui_packing_mode(spacing=3)
    call gui_create_box(hbox, outer_vbox,   gui_horizontal)
    call gui_create_label(label, hbox, "Selection to use: ")
    call gui_create_combo_box(selection_box, hbox, selection_name)
    call gui_packing_mode(position=gui_end)
    call gui_create_entrybox(np_box, hbox, length=8, editable=.false.)
    call gui_set_sensitive(np_box, .false.)
    call gui_create_label(label, hbox, "Particles: ")
    call gui_packing_mode(position=gui_start)

    ! Notebook
    call gui_create_notebook(notebook, outer_vbox) 

    ! Particle selection options
    call gui_create_box_in_notebook(nbbox, notebook, "Modify selection")
    call gui_packing_mode(spacing=5)
    call gui_create_box(vbox, nbbox, gui_vertical, frame=.true.)
    call gui_packing_mode(spacing=3)

    ! Choose particle type
    call gui_create_box(hbox, vbox,   gui_horizontal)
    call gui_create_label(label, hbox, "Select particles of type ")
    call gui_create_combo_box(species_box, hbox, (/ "<particle type>" /))

    ! Choose whether to select from sample or full dataset
    call gui_create_box(hbox, vbox,   gui_horizontal)
    call gui_create_label(label, hbox, " from ")
    call gui_create_radio_button(sample_radio, hbox, " the current sample ")
    call gui_create_radio_button(full_radio,   hbox, &
         " all particles in the snapshot ", previous=sample_radio)

    ! Choose radius
    call gui_create_box(hbox, vbox,   gui_horizontal)
    call gui_create_checkbox(radius_checkbox, hbox, "within radius ")
    call gui_create_entrybox(radius_entry, hbox, 8)
    call gui_create_label(label, hbox, " of selected point")

    ! Choose property and range of values
    call gui_create_box(hbox, vbox,   gui_horizontal)
    call gui_create_checkbox(prop_checkbox, hbox, "with ")
    call gui_create_combo_box(prop_box, hbox, (/ "<particle property>" /))
    call gui_create_label(label, hbox, " between ")
    call gui_create_entrybox(val_min_entry, hbox, 8)
    call gui_create_label(label, hbox, " and ")
    call gui_create_entrybox(val_max_entry, hbox, 8)

    ! Choose how to combine current and new selections
    call gui_packing_mode(spacing=5)
    call gui_create_box(inner_vbox, nbbox, gui_vertical, frame=.true., &
         label="How to combine with existing selection", expander=.true.)
    call gui_packing_mode(spacing=3)
    call gui_create_box(hbox, inner_vbox, gui_horizontal)
    call gui_create_radio_button(new_radio, inner_vbox, &
         "Select only these particles (clears previous selection)")
    call gui_create_radio_button(add_radio, inner_vbox, &
         "Add these particles to the existing selection", previous=new_radio)
    call gui_create_radio_button(subset_radio, inner_vbox, &
         "Select a subset of currently selected particles",previous=add_radio)

    ! Buttons
    call gui_packing_mode(spacing=5)
    call gui_create_box(hbox, nbbox,   gui_horizontal)
    call gui_packing_mode(spacing=3)
    call gui_packing_mode(position=gui_end)
    call gui_create_button(clear_all_button, hbox, "Clear all selections")
    call gui_create_button(clear_button,     hbox, "Clear this selection")
    call gui_create_button(centre_button,    hbox, "Centre selection")
    call gui_create_button(apply_button,     hbox, "Select particles")

    ! Read selection window
    call gui_packing_mode(expand=.true., fill=.true.)
    call gui_packing_mode(position=gui_start)
    call gui_create_box_in_notebook(nbbox, notebook, "Read selection")
    call gui_create_box(vbox, nbbox, gui_vertical)
    call gui_packing_mode(expand=.false., fill=.false.)
    call gui_create_box(hbox, vbox, gui_horizontal)
    call gui_create_label(label, hbox, &
         "Select particles by reading their IDs from a text file.")
    call gui_create_box(hbox, vbox, gui_horizontal)
    call gui_create_label(label, hbox, &
         "The file should contain a single column of particle IDs.")
    call gui_create_box(hbox, vbox,   gui_horizontal)
    call gui_create_label(label, hbox, "Particle type: ")
    call gui_create_combo_box(read_species_box, hbox, (/ "<particle type>" /))
    call gui_create_box(hbox, vbox, gui_horizontal)
    call gui_create_label(label, hbox, "Filename: ")
    call gui_packing_mode(expand=.true., fill=.true.)
    call gui_create_entrybox(fname_box, hbox)
    call gui_packing_mode(expand=.false., fill=.false.)
    call gui_create_button(browse_button, hbox, " Browse ")
    call gui_packing_mode(position=gui_end)
    call gui_create_box(hbox, vbox, gui_horizontal)
    call gui_create_button(read_button, hbox, "Read IDs from file")
    call gui_packing_mode(position=gui_start)

    ! Display options
    call gui_packing_mode(expand=.false., fill=.false.)
    call gui_packing_mode(position=gui_start)
    call gui_create_box_in_notebook(nbbox, notebook, "Display options")
    call gui_create_box(vbox, nbbox,   gui_vertical)
    call gui_create_box(hbox, vbox,    gui_horizontal)
    call gui_create_label(label, hbox, "Name of this selection: ")
    call gui_packing_mode(position=gui_end)
    call gui_create_entrybox(name_entry, hbox)
    call gui_packing_mode(position=gui_start)
    call gui_create_box(hbox, vbox,    gui_horizontal)
    call gui_create_label(label, hbox, "Colour used to highlight particles: ")
    call gui_packing_mode(position=gui_end)
    call gui_create_colour_button(cbutton, hbox)
    call gui_packing_mode(position=gui_start)
    call gui_colour_button_set_colour(cbutton, &
         icol(1,0), icol(2,0), icol(3,0))
    call gui_create_box(hbox, vbox,    gui_horizontal)
    call gui_create_checkbox(centre_checkbox, hbox, &
         "Include these particles when recentering after snapshot change")

    ! Summary window
    call gui_packing_mode(expand=.true., fill=.true.)
    call gui_create_box_in_notebook(nbbox, notebook, "Summary")
    call gui_create_box(hbox, nbbox, gui_horizontal, scrollable=.true.)
    call gui_create_textview(textview, hbox)

    ! Fill in text in  name entry box
    call gui_combo_box_get_index(selection_box, isel)
    isel = isel - 1
    call gui_entrybox_set_text(name_entry, selection_name(isel))

    call gui_show_window(window)
    is_open = .true.

    if(first_call)then
       first_call = .false.
       call selection_store_state()
    else
       call selection_set_state()
    endif
    call selection_update_window()

    call gui_packing_mode(spacing=3)

    call selection_update_summary()

    return
  end subroutine selection_open


  logical function selection_process_events(mainwin)
!
! Process events in the particle selection window
!
    implicit none
    type(selection_type) :: sel

    real(kind=pos_kind)  :: radius
    real(kind=real8byte)   :: rval_min, rval_max
    integer(kind=int8byte) :: ival_min, ival_max
    character(len=100)   :: str
    integer              :: ios
    character(len=10)    :: bt
    logical              :: use_full
    logical              :: clear, subset
    real(kind=pos_kind), dimension(3) :: centre
    character(len=20)    :: type
    type(result_type)    :: res
    type(gui_window)     :: mainwin
    integer              :: r, g, b
    integer              :: nspecies, nprops
    logical              :: ok
    character(len=500)   :: fname

    ! Return true if main window needs to be redrawn
    selection_process_events = .false.

    ! Close window if necessary
    if(gui_window_closed(window))then
       call selection_close()
    endif

    ! Pop up file selector if browse button is clicked
    if(gui_button_clicked(browse_button))then
       call gui_select_file(mainwin, &
            "Select file to read", &
            gui_file_open, ok, fname)
       if(ok)then
          call gui_entrybox_set_text(fname_box, trim(fname))
       endif
    endif

    ! Read file if button clicked
    if(gui_button_clicked(read_button))then
       call gui_combo_box_get_index(read_species_box, ispecies_read)
       call gui_entrybox_get_text(fname_box, fname)
       res = selection_read(fname, ispecies_read)
       if(.not.res%success)bt=gui_display_dialog(mainwin,"error",res%string)
       call selection_update_window()
       selection_process_events = .true.
    endif

    ! Update widgets if tickboxes change
    if(gui_checkbox_changed(radius_checkbox).or. &
         gui_checkbox_changed(prop_checkbox).or. &
         gui_combo_box_changed(species_box).or.  &
         gui_checkbox_changed(prop_checkbox).or. &
         gui_combo_box_changed(read_species_box))then
       call gui_checkbox_get_state(radius_checkbox, use_radius)
       call gui_checkbox_get_state(prop_checkbox,   use_prop)
       call gui_combo_box_get_index(prop_box, iprop)
       call gui_combo_box_get_index(species_box, ispecies)
       call gui_combo_box_get_index(read_species_box, ispecies_read)
       call selection_update_window()
    endif

    ! Update colour button if selection changed
    if(gui_combo_box_changed(selection_box))then
       call gui_combo_box_get_index(selection_box, isel)
       isel = isel - 1
       call gui_colour_button_set_colour(cbutton, &
            icol(1,isel), icol(2,isel), icol(3,isel))
       call gui_entrybox_set_text(name_entry, selection_name(isel))
       call selection_update_window()
       call selection_update_summary()
    endif

    ! Update selection name if entry box is edited
    if(gui_entrybox_changed(name_entry))then
       call gui_entrybox_get_text(name_entry, str)
       if(len_trim(str).gt.0)then
          call gui_combo_box_get_index(selection_box, isel)
          isel = isel - 1
          selection_name(isel) = trim(adjustl(str))
          call gui_combo_box_set_text(selection_box, selection_name)
          call gui_combo_box_set_index(selection_box, isel+1)
          call gui_entrybox_set_text(name_entry, &
               trim(adjustl(selection_name(isel))))
          selection_process_events = .true.
       endif
    endif

    if(gui_checkbox_changed(centre_checkbox))then
       call gui_checkbox_get_state(centre_checkbox, use_centre(isel))
    endif

    ! Store new colour if colour changed
    if(gui_colour_button_changed(cbutton))then
       call gui_combo_box_get_index(selection_box, isel)
       isel = isel - 1
       call gui_colour_button_get_colour(cbutton,r,g,b)
       icol(1:3,isel) = (/ r, g, b /)
       selection_process_events = .true.
    endif

    ! Clear selection if button is pressed. Do this by making an empty
    ! selection and applying it to the sampled dataset.
    if(particle_store_loaded(psample).and. &
         gui_button_clicked(clear_button))then
       call gui_combo_box_get_index(selection_box, isel)
       isel = isel - 1
       call selection_clear(sample_sel(isel),psample)
       res = selection_apply(sample_sel(isel),psample,isel)
       call selection_update_window()
       selection_process_events = .true.
       ! Display error message on failure
       if(.not.res%success)bt=gui_display_dialog(mainwin,"error",res%string)
    endif

    ! Clear all selections if button is pressed. Do this by making an empty
    ! selection and applying it to the sampled dataset.
    if(particle_store_loaded(psample).and. &
         gui_button_clicked(clear_all_button))then
       call selection_clear_all(psample)
       res = selection_apply_all(psample)
       selection_process_events = .true.
       call selection_update_window()
       ! Display error message on failure
       if(.not.res%success)bt=gui_display_dialog(mainwin,"error",res%string)
    endif

    ! Recentre if button pressed
    if(particle_store_loaded(psample).and.&
         gui_button_clicked(centre_button))then
       call selection_recentre_view(use_isel = .true., mainwin=mainwin)
       selection_process_events = .true.
    endif

    ! Modify the current selection if the apply button is pressed
    if(particle_store_loaded(psample).and.&
         gui_button_clicked(apply_button))then
       
       ! Get information from gui widgets
       call gui_combo_box_get_index(selection_box, isel)
       isel = isel - 1
       call gui_combo_box_get_index(species_box, ispecies)
       call particle_store_contents(psample,get_nspecies=nspecies)
       if(ispecies.gt.nspecies)return
       call gui_combo_box_get_index(prop_box,    iprop)
       call particle_store_species(psample, ispecies, get_nprops=nprops)
       if(iprop.gt.nprops)return
       call gui_checkbox_get_state(radius_checkbox, use_radius)
       call gui_checkbox_get_state(prop_checkbox,   use_prop)
       if(use_prop)then
          call particle_store_property(psample, ispecies, iprop, &
               get_type=type)
          call gui_entrybox_get_text(val_min_entry, str)
          select case(type)
          case("REAL")
             read(str,*,iostat=ios)rval_min
             if(ios.ne.0)then
                bt=gui_display_dialog(window,"error",&
                     "Unable to interpret minimum value as real")
                return
             endif
             call gui_entrybox_get_text(val_max_entry, str)
             read(str,*,iostat=ios)rval_max
             if(ios.ne.0)then
                bt=gui_display_dialog(window,"error",&
                     "Unable to interpret maximum value as real")
                return
             endif
          case("INTEGER")
             read(str,*,iostat=ios)ival_min
             if(ios.ne.0)then
                bt=gui_display_dialog(window,"error",&
                     "Unable to interpret minimum value as integer")
                return
             endif
             call gui_entrybox_get_text(val_max_entry, str)
             read(str,*,iostat=ios)ival_max
             if(ios.ne.0)then
                bt=gui_display_dialog(window,"error",&
                     "Unable to interpret maximum value as integer")
                return
             endif
          end select
       else
          iprop = 0
       endif
       if(use_radius)then
          call gui_entrybox_get_text(radius_entry, str)
          read(str,*,iostat=ios)radius
          if(ios.ne.0)then
             bt=gui_display_dialog(window,"error",&
                  "Unable to interpret radius value")
             return
          endif
       else
          radius = 0.0
       endif

       call gui_radio_button_get_state(full_radio,      use_full)
       call gui_radio_button_get_state(new_radio,       clear)
       call gui_radio_button_get_state(subset_radio,    subset)

       ! Get currently selected point
       centre(1:3) = view_transform%centre(1:3)

       ! Make a selection based on the parameters from the gui
       sel%nspecies = 0
       sel%empty    = .true.
       if(use_full)then
          res = selection_find_particles(sel, pdata, ispecies, &
               centre, radius, iprop, rval_min, rval_max, &
               ival_min, ival_max)
       else
          res = selection_find_particles(sel, psample, ispecies, &
               centre, radius, iprop, rval_min, rval_max, &
               ival_min, ival_max)
       endif
       
       ! Display error message if anything went wrong
       if(.not.res%success)then
          bt=gui_display_dialog(mainwin,"error",res%string)
          return
       endif

       ! Combine this with any existing selection according to the
       ! gui settings
       if(clear)call selection_clear(sample_sel(isel), psample)
       if(subset)then
          ! Only select from particles which are already selected
          res = selection_subset(sample_sel(isel), sel)
       else
          ! Add these particles to the existing selection
          res = selection_add(sample_sel(isel), sel)
       endif

       ! Display error message if anything went wrong
       if(.not.res%success)then
          bt=gui_display_dialog(mainwin,"error",res%string)
          return
       endif

       ! Deallocate temporary selection
       call selection_clear(sel)

       ! Apply new selection to the particle data
       res = selection_apply(sample_sel(isel), psample, isel)

       ! Display error message if anything went wrong
       if(.not.res%success)then
          bt=gui_display_dialog(mainwin,"error",res%string)
          return
       endif

       ! Will need to redraw the display
       selection_process_events = .true.
       call selection_update_window()

    endif

    if(selection_process_events)call selection_update_summary()

    return
  end function selection_process_events


  subroutine selection_update_window()
!
! Update the widgets in the selection window
!
    implicit none
    integer :: nspecies
    character(len=maxlen), dimension(maxspecies) :: species_names
    integer :: nprops
    character(len=maxlen), dimension(maxprops)   :: propnames

    if(.not.is_open)return

    ! Get particle type names and update combo box
    call particle_store_contents(psample, get_nspecies=nspecies, &
         get_species_names=species_names)
    if(ispecies.lt.1.or.ispecies.gt.nspecies)then
       ispecies = max(1,min(ispecies, nspecies))
    endif
    if(ispecies_read.lt.1.or.ispecies_read.gt.nspecies)then
       ispecies_read = max(1,min(ispecies_read, nspecies))
    endif
    call gui_combo_box_set_text(species_box, species_names(1:nspecies))
    call gui_combo_box_set_text(read_species_box, species_names(1:nspecies))
    call gui_combo_box_set_index(species_box, ispecies)
    call gui_combo_box_set_index(read_species_box, ispecies_read)

    ! Grey out widgets that aren't in use
    call gui_set_sensitive(radius_entry,  use_radius)
    call gui_set_sensitive(prop_box,      use_prop)
    call gui_set_sensitive(val_min_entry,  use_prop)
    call gui_set_sensitive(val_max_entry,  use_prop)

    ! Set list of particle properties
    if(nspecies.gt.0)then
       call particle_store_species(psample, ispecies, get_nprops=nprops, &
            get_propnames=propnames)
       if(iprop.lt.1.or.iprop.gt.nprops)then
          iprop = max(1,min(nprops,iprop))
       endif
       call gui_combo_box_set_text(prop_box, propnames(1:nprops))
       call gui_combo_box_set_index(prop_box,iprop)
    endif

    ! Set particle number
    call gui_entrybox_set_text(np_box, &
         trim(adjustl(string(sum(sample_sel(isel)%idlist%np)))))

    call gui_checkbox_set_state(centre_checkbox, use_centre(isel))

    ! Need to specify at least one condition before particles can be
    ! selected
    call gui_set_sensitive(apply_button,  use_prop.or.use_radius)

    return
  end subroutine selection_update_window


  subroutine selection_recentre_view(use_isel, mainwin)
!
! Centre the view on the mean position of the selected particles
!
    implicit none
    type (gui_window), optional :: mainwin
    logical, optional :: use_isel
    logical           :: have_isel
    integer,             dimension(:),   pointer :: selected
    real(kind=pos_kind), dimension(:,:), pointer :: pos
    real(kind=pos_kind), dimension(3) :: wrapped_pos, ref_pos
    logical :: found_pos
    integer(kind=index_kind), dimension(maxspecies) :: np
    integer :: nspecies, i, j, k, itype
    double precision, dimension(3) :: centre
    integer                   :: nsel
    integer                   :: bitmask
    integer(kind=i_prop_kind) :: id_find
    logical                   :: found
    integer(kind=i_prop_kind), dimension(:), pointer :: id
    integer :: ipart
    real :: boxsize

    have_isel = .false.
    if(present(use_isel))have_isel=use_isel
    if(have_isel)then
       bitmask = ibset(0, isel)
    else
       bitmask = 0
       do i = 0, nselmax-1, 1
          if(use_centre(i)) &
               bitmask = ibset(bitmask, i)
       end do
    endif

    ! Get size of box in case periodic
    call particle_store_get_boxsize(pdata, boxsize)

    ! Get mean position
    centre(1:3) = 0.0
    nsel        = 0
    call particle_store_contents(psample, get_np=np, get_nspecies=nspecies)
    do i = 1, nspecies, 1
       call particle_store_species(psample, i, get_pos=pos, &
            get_selected=selected)
       found_pos = .false.
       do j = 1, np(i), 1
          if(iand(selected(j),bitmask).ne.0)then
             ! Use first selected particle as reference for periodic wrap
             if(.not.found_pos)then
                ref_pos = pos(1:3,j)
                found_pos = .true.
             endif
             ! Wrap position to copy closest to reference if necessary
             wrapped_pos = pos(1:3,j)
             if(boxsize.gt.0.0)then
                do k = 1, 3, 1
                   if(wrapped_pos(k).gt.ref_pos(k)+0.5*boxsize) &
                        wrapped_pos(k) = wrapped_pos(k) - boxsize
                   if(wrapped_pos(k).lt.ref_pos(k)-0.5*boxsize) &
                        wrapped_pos(k) = wrapped_pos(k) + boxsize
                end do
             end if
             centre(1:3)=centre(1:3)+real(wrapped_pos,kind(centre))
             nsel = nsel + 1
          endif
       end do
    end do

    if(nsel.gt.0)then
       centre(1:3) = centre(1:3) / nsel
       ! Wrap centre back into box
       do k = 1, 3, 1
          if(centre(k).lt.0.0)centre(k) = centre(k) + boxsize
          if(centre(k).ge.boxsize)centre(k) = centre(k) - boxsize
       end do
       ! Recenter view if we found any selected particles
       view_transform%centre(1:3) = real(centre(1:3),kind=pos_kind)
    else
       ! If that failed, will need to look at the full dataset.
       ! Don't want to sort all particles so just find the first
       ! one with a selected ID and hope the selection is a clump...
       found = .false.
       do i = 0, nselmax-1, 1
          if((have_isel.and.isel.eq.i).or. &
               (.not.have_isel.and.use_centre(i)))then
             do j = 1, nspecies, 1
                if(sample_sel(i)%idlist(j)%np.gt.0)then
                   id_find = sample_sel(i)%idlist(j)%id(1)
                   found = .true.
                   itype = j
                   exit
                endif
             end do
          endif
          if(found)exit
       end do
       if(found)then
          ! Find the particle in the full dataset
          call particle_store_contents(pdata, get_np=np)
          call particle_store_species(pdata, itype, get_pos=pos, get_id=id)
          ipart = -1
          do i = 1, np(itype), 1
             if(id(i).eq.id_find)ipart = i
          end do
          if(ipart.gt.0)then
             view_transform%centre(1:3) = real(pos(1:3,ipart),kind=pos_kind)
             if(present(mainwin))&
                  call gui_window_set_statusbar(mainwin, &
                  "No selected particles in current sample")
          endif
       endif
    endif

    return
  end subroutine selection_recentre_view


  subroutine selection_close()
!
! Close the window
!
    implicit none

    if(is_open)then
       call selection_store_state()
       is_open = .false.
       call gui_destroy_window(window)
    endif

    return
  end subroutine selection_close


  subroutine selection_store_state()
!
! Record the state of the window
!
    implicit none

    call gui_combo_box_get_index(species_box,    ispecies)
    call gui_combo_box_get_index(prop_box,       iprop)
    call gui_checkbox_get_state(radius_checkbox, use_radius)
    call gui_checkbox_get_state(prop_checkbox,   use_prop)
    call gui_entrybox_get_text(radius_entry,     radius_text)
    call gui_entrybox_get_text(val_min_entry,    val_min_text)
    call gui_entrybox_get_text(val_max_entry,    val_max_text)
    call gui_radio_button_get_state(full_radio,  full)
    call gui_radio_button_get_state(new_radio,   new)
    call gui_radio_button_get_state(add_radio,   add)
    call gui_radio_button_get_state(subset_radio,subset)

    call gui_combo_box_get_index(selection_box, isel)
    isel = isel - 1
    call gui_entrybox_get_text(name_entry, selection_name(isel))

    return
  end subroutine selection_store_state


  subroutine selection_set_state()
!
! Set the state of the window
!
    implicit none

    call gui_combo_box_set_index(species_box,    ispecies)
    call gui_combo_box_set_index(prop_box,       iprop)
    call gui_checkbox_set_state(radius_checkbox, use_radius)
    call gui_checkbox_set_state(prop_checkbox,   use_prop)
    call gui_entrybox_set_text(radius_entry,     radius_text)
    call gui_entrybox_set_text(val_min_entry,    val_min_text)
    call gui_entrybox_set_text(val_max_entry,    val_max_text)
    call gui_radio_button_set_state(full_radio,  full)
    call gui_radio_button_set_state(new_radio,   new)
    call gui_radio_button_set_state(add_radio,   add)
    call gui_radio_button_set_state(subset_radio,subset)

    call gui_combo_box_set_index(selection_box, isel+1)
    call gui_entrybox_set_text(name_entry, selection_name(isel))

    return
  end subroutine selection_set_state


  type(result_type) function selection_apply_all(pdata)
!
! Apply all current selections
!
    implicit none
    type(pdata_type)  :: pdata
    integer           :: i
    type(result_type) :: res
    
    do i = 0, nselmax-1, 1
       res = selection_apply(sample_sel(i), pdata, i)
       if(.not.res%success)exit
    end do

    selection_apply_all = res

    return
  end function selection_apply_all


  subroutine selection_clear_all(pdata)
!
! Apply all current selections
!
    implicit none
    type(pdata_type) :: pdata
    integer :: i
    
    do i = 0, nselmax-1, 1
       call selection_clear(sample_sel(i), pdata)
    end do

    return
  end subroutine selection_clear_all


  function selection_get_colour(isel) result(res)
!
! Return the colour for the specified selection
!
    implicit none
    integer :: isel
    integer, dimension(3) :: res

    res = icol(1:3,isel)

    return
  end function selection_get_colour


  logical function selection_is_empty(isel)
!
! Return true if the specified selection is empty
!
    implicit none
    integer :: isel

    selection_is_empty = sample_sel(isel)%empty

    return
  end function selection_is_empty

  
  subroutine selection_get_names(names)
!
! Return array of selection names
!
    implicit none
    integer :: isel
    character(len=*), dimension(0:nselmax-1) :: names

    do isel = 0, nselmax-1, 1
       names(isel) = trim(selection_name(isel))
    end do

    return
  end subroutine selection_get_names


  type(result_type) function selection_dump_state(fname) result(res)
!
! Write out the current ID lists
!
    implicit none
    character(len=*) :: fname
    integer          :: isel
    integer          :: ios
    integer          :: ispecies

    open(unit=1,file=fname,status="unknown",form="unformatted",iostat=ios)
    if(ios.ne.0)then
       res%success = .false.
       res%string  = "Unable to open file "//trim(fname)
       return
    endif

    write(1,iostat=ios)nselmax
    if(ios.ne.0)then
       res%success = .false.
       res%string  = "Unable to write to file 1"//trim(fname)
       close(1)
       return
    endif

    write(1,iostat=ios)selection_name
    if(ios.ne.0)then
       res%success = .false.
       res%string  = "Unable to write to file 2"//trim(fname)
       close(1)
       return
    endif

    do isel = 0, nselmax-1, 1
       write(1,iostat=ios)sample_sel(isel)%empty, sample_sel(isel)%nspecies
       if(ios.ne.0)then
          res%success = .false.
          res%string  = "Unable to write to file 3"//trim(fname)
          close(1)
          return
       endif
       do ispecies = 1, sample_sel(isel)%nspecies, 1
          write(1,iostat=ios)sample_sel(isel)%idlist(ispecies)%np
          if(ios.ne.0)then
             res%success = .false.
             res%string  = "Unable to write to file 4"//trim(fname)
             close(1)
             return
          endif
          if(sample_sel(isel)%idlist(ispecies)%np.gt.0)then
             write(1,iostat=ios)sample_sel(isel)%idlist(ispecies)%id
             if(ios.ne.0)then
                res%success = .false.
                res%string  = "Unable to write to file 5"//trim(fname)
                close(1)
                return
             endif
          endif
       end do
    end do

    close(1)

    res%success = .true.

    return
  end function selection_dump_state


  type(result_type) function selection_restore_state(fname) result(res)
!
! Read in previously saved ID lists
!
    implicit none
    character(len=*) :: fname
    integer          :: isel
    integer          :: ios, istat
    integer          :: ispecies
    integer          :: nsel
    integer          :: nspecies
    integer          :: np

    call selection_clear_all(pdata)
    call particle_store_contents(pdata, get_nspecies=nspecies)

    open(unit=1,file=fname,status="old",form="unformatted",iostat=ios)
    if(ios.ne.0)then
       res%success = .false.
       res%string  = "Unable to open file "//trim(fname)
       return
    endif

    read(1,iostat=ios)nsel
    if(ios.ne.0)then
       res%success = .false.
       res%string  = "Unable to read from file "//trim(fname)
       close(1)
       return
    endif

    read(1,iostat=ios)selection_name
    if(ios.ne.0)then
       res%success = .false.
       res%string  = "Unable to read from file "//trim(fname)
       close(1)
       return
    endif

    do isel = 0, min(nsel-1,nselmax-1), 1
       read(1,iostat=ios)sample_sel(isel)%empty, sample_sel(isel)%nspecies
       if(ios.ne.0)then
          res%success = .false.
          res%string  = "Unable to read from file "//trim(fname)
          close(1)
          return
       endif
       do ispecies = 1, min(nspecies,sample_sel(isel)%nspecies), 1
          read(1,iostat=ios)sample_sel(isel)%idlist(ispecies)%np
          if(ios.ne.0)then
             res%success = .false.
             res%string  = "Unable to read from file "//trim(fname)
             close(1)
             return
          endif
          if(associated(sample_sel(isel)%idlist(ispecies)%id))&
               deallocate(sample_sel(isel)%idlist(ispecies)%id)
          np = sample_sel(isel)%idlist(ispecies)%np
          if(np.gt.0)then
             allocate(sample_sel(isel)%idlist(ispecies)%id(np), stat=istat)
             if(istat.ne.0)then
                res%success = .false.
                res%string  = "Unable to allocate memory"
                close(1)
                return
             endif
             read(1,iostat=ios)sample_sel(isel)%idlist(ispecies)%id
             if(ios.ne.0)then
                res%success = .false.
                res%string  = "Unable to read from file "//trim(fname)
                close(1)
                return
             endif
          endif
       end do
    end do

    close(1)

    res%success = .true.

    return
  end function selection_restore_state


  subroutine selection_update_summary()
!
! Write a summary of the selected particles
!
    implicit none
    integer :: ispecies, nspecies
    integer(kind=index_kind), dimension(maxspecies) :: np
    integer :: nsel
    integer, dimension(:), pointer :: selected
    integer :: i, nprops, iprop
    double precision :: mtot, cpos(3), cvel(3)
    real(kind=r_prop_kind), dimension(:), pointer :: mass
    real(kind=pos_kind),    dimension(:,:), pointer :: pos
    real(kind=vel_kind),    dimension(:,:), pointer :: vel
    character(len=maxlen),  dimension(maxprops) :: propnames
    real :: fsample
    character(len=20) :: type
    integer(kind=i_prop_kind), dimension(:), pointer :: idata
    real(kind=r_prop_kind),    dimension(:), pointer :: rdata
    double precision :: propmin, propmax, propmean
    integer(kind=int8byte) :: ipropmin, ipropmax

    if(.not.is_open)return
    if(.not.particle_store_loaded(psample))return

    call particle_store_contents(psample, get_nspecies=nspecies, get_np=np, &
         get_fsample=fsample)

    call gui_textview_clear(textview)
    call gui_textview_add_line(textview, "")
    call gui_textview_add_line(textview, &
         'Properties of particles in selection "'// &
         trim(selection_name(isel))//'"')
    call gui_textview_add_line(textview, &
         "(based on displayed sample, sample rate = "// &
         trim(adjustl(string(fsample,fmt="(1es14.4)")))//")")
    call gui_textview_add_line(textview, "")

    ! Write out info for each type
    do ispecies = 1, nspecies, 1
       call gui_textview_add_line(textview, &
            "Type "//trim(adjustl(string(ispecies-1))))
       call gui_textview_add_line(textview, "")
       nsel = 0
#ifdef READ_VEL
       call particle_store_species(psample, ispecies, get_vel=vel)
#else
       nullify(vel)
#endif
       call particle_store_species(psample, ispecies, &
            get_selected=selected, get_mass=mass, get_pos=pos, &
            get_propnames=propnames, get_nprops=nprops)
       mtot = 0.0
       cpos = 0.0
       cvel = 0.0
       do i = 1, np(ispecies), 1
          if(btest(selected(i),isel))then
             nsel = nsel + 1
             mtot = mtot + mass(i)
             cpos = cpos + pos(1:3,i)*mass(i)
#ifdef READ_VEL
             cvel = cvel + vel(1:3,i)*mass(i)
#endif
          endif
       end do
       if(mtot.gt.0.0)then
          cpos = cpos / mtot
          cvel = cvel / mtot
       else
          cpos = 0.0
          cvel = 0.0
       endif
       call gui_textview_add_line(textview, &
            "  Displayed sample contains "//trim(adjustl(string(nsel)))// &
            " selected particles ")
       if(nsel.gt.0)then
          call gui_textview_add_line(textview, &
               "  Mass: "//trim(adjustl(string(mtot,fmt="("//trim(rprop_fmt)//")"))))
          call gui_textview_add_line(textview, &
               "  Mass corrected for sampling rate: "// &
               trim(adjustl(string(mtot/fsample,fmt="("//trim(rprop_fmt)//")"))))
          call gui_textview_add_line(textview, &
               "  Centre of mass: "// &
               trim(adjustl(string(cpos(1),fmt="("//trim(rprop_fmt)//")")))//", "// &
               trim(adjustl(string(cpos(2),fmt="("//trim(rprop_fmt)//")")))//", "// &
               trim(adjustl(string(cpos(3),fmt="("//trim(rprop_fmt)//")"))))
#ifdef READ_VEL
          call gui_textview_add_line(textview, &
               "  CofM Velocity : "// &
               trim(adjustl(string(cvel(1),fmt="("//trim(rprop_fmt)//")")))//", "// &
               trim(adjustl(string(cvel(2),fmt="("//trim(rprop_fmt)//")")))//", "// &
               trim(adjustl(string(cvel(3),fmt="("//trim(rprop_fmt)//")"))))
#endif
          ! Write min/max/mean for each property
          call gui_textview_add_line(textview, "")
          do iprop = 1, nprops, 1
             call gui_textview_add_line(textview, "  "//trim(propnames(iprop)))
             call particle_store_property(psample, ispecies, iprop, &
                  get_type=type)
             select case(type)
             case("INTEGER")
                call particle_store_property(psample, ispecies, iprop, &
                     get_idata=idata)
                ipropmin  = maxval(idata)
                ipropmax  = minval(idata)
                propmean = 0.0
                do i = 1, np(ispecies), 1
                   if(btest(selected(i),isel))then
                      ipropmin  = min(ipropmin, int(idata(i),int8byte))
                      ipropmax  = max(ipropmax, int(idata(i),int8byte))
                      propmean = propmean + dble(idata(i))
                   endif
                end do
                propmean = propmean / dble(nsel)
                call gui_textview_add_line(textview, "    Min: "// &
                     trim(adjustl(string(ipropmin, fmt="("//trim(iprop_fmt)//")"))))
                call gui_textview_add_line(textview, "    Max: "// &
                     trim(adjustl(string(ipropmax, fmt="("//trim(iprop_fmt)//")"))))
                call gui_textview_add_line(textview, "    Mean: "// &
                     trim(adjustl(string(propmean, fmt="("//trim(rprop_fmt)//")"))))
             case("REAL")
                call particle_store_property(psample, ispecies, iprop, &
                     get_rdata=rdata)
                propmin  = maxval(rdata)
                propmax  = minval(rdata)
                propmean = 0.0
                do i = 1, np(ispecies), 1
                   if(btest(selected(i),isel))then
                      propmin = min(propmin, dble(rdata(i)))
                      propmax = max(propmax, dble(rdata(i)))
                      propmean = propmean + dble(rdata(i))
                   endif
                end do
                propmean = propmean / dble(nsel)
                call gui_textview_add_line(textview, "    Min: "// &
                     trim(adjustl(string(propmin,fmt="("//trim(rprop_fmt)//")"))))
                call gui_textview_add_line(textview, "    Max: "// &
                     trim(adjustl(string(propmax,fmt="("//trim(rprop_fmt)//")"))))
                call gui_textview_add_line(textview, "    Mean: "// &
                     trim(adjustl(string(propmean,fmt="("//trim(rprop_fmt)//")"))))
             end select
             call gui_textview_add_line(textview, "")
          end do

       endif
       call gui_textview_add_line(textview, "")
       ! Next type
    end do

    return
  end subroutine selection_update_summary


  type (result_type) function selection_read(fname, ispecies) result(res)
!
! Read particle IDs from a file
!
    implicit none
    character(len=*)          :: fname
    integer                   :: ios, nread, stat
    type (data_array_type)    :: id_arr
    integer(kind=i_prop_kind) :: id
    integer(kind=i_prop_kind), dimension(:), pointer :: ids
    integer                   :: ispecies, nspecies, i

    res%success = .false.

    ! Read in the IDs
    open(unit=1,file=fname,iostat=ios,form="formatted")
    if(ios.ne.0)then
       res%string = "Unable to open file: "//trim(fname)
       return
    endif

    call data_array_init(id_arr)
    ios   = 0
    nread = 0
    do while(ios.eq.0)
       read(1,*,iostat=ios)id
       if(ios.eq.0)then
          call data_array_add_elements(id_arr, (/id/), stat)
          if(stat.ne.0)then
             call data_array_dealloc(id_arr)
             close(1)
             res%string = "Unable to allocate memory for IDs"
             return
          endif
          nread = nread + 1
       endif
    end do
    close(1)
    if(nread.eq.0)then
       res%string = "Unable to read any IDs from file: "//trim(fname)
       return
    endif
    call data_array_get_data(id_arr, ids, stat)
    if(stat.ne.0)then
       res%string = "Unable to allocate memory for IDs"
       return
    endif

    ! Update the selection in sample_sel(isel)
    call particle_store_contents(pdata, get_nspecies=nspecies)
    sample_sel(isel)%empty    = .false.
    sample_sel(isel)%nspecies = nspecies
    do i = 1, maxspecies, 1
       sample_sel(isel)%idlist(i)%np = 0
       nullify(sample_sel(isel)%idlist(i)%id)
    end do
    sample_sel(isel)%idlist(ispecies)%id => ids
    sample_sel(isel)%idlist(ispecies)%np = nread

    res = selection_apply(sample_sel(isel), psample, isel)
    if(.not.res%success)then
       return
    endif
       
    res%success = .true.
    return
  end function selection_read


  subroutine selection_set_keys()

    use key_file
    implicit none

    call set_key("Selection","Red",   icol(1,:))
    call set_key("Selection","Green", icol(2,:))
    call set_key("Selection","Blue",  icol(3,:))
    call set_key("Selection","Display Selection",  display_selection)
    call set_key("Selection","Use to Recentre",    use_centre)
    call set_key("Selection","Draw Selected Only", draw_selected_only)
    call set_key("Selection","Follow Selection",   follow_selection)

    return
  end subroutine selection_set_keys


  subroutine selection_get_keys()

    use key_file
    implicit none

    call get_key("Selection","Red",   icol(1,:))
    call get_key("Selection","Green", icol(2,:))
    call get_key("Selection","Blue",  icol(3,:))
    call get_key("Selection","Display Selection",  display_selection)
    call get_key("Selection","Use to Recentre",    use_centre)
    call get_key("Selection","Draw Selected Only", draw_selected_only)
    call get_key("Selection","Follow Selection",   follow_selection)

    return
  end subroutine selection_get_keys

end module selection
