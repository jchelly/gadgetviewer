module data_array
!
! Provides a type which can store an array of integer/real variables
! of 4/8 bytes each. Data is stored in blocks so that additional blocks
! can be allocated as required. One big block is allocated if the size
! of the array is supplied in advance.
!
! Subroutines:
!
! data_array_init()         - initialise a new data array
! data_array_dealloc()      - deallocate a data array
! data_array_add_elements() - add elements to the end of the array
! data_array_get_data()     - get pointer to a normal array with the data.
!                             Deallocates the data_array objects 's buffers.
!
! Note that add_elements and get_data may fail if there isn't enough memory.
!
  use f90_util
  use data_types

  implicit none
  private

  ! Size of a data block in elements
  integer, parameter :: DEFAULT_BLOCKSIZE = 100000

  ! Identifiers for data types
  type data_type_id
     private
     integer :: type
  end type data_type_id
  type (data_type_id), parameter :: INTEGER4 = data_type_id(1)
  type (data_type_id), parameter :: INTEGER8 = data_type_id(2)
  type (data_type_id), parameter :: REAL4    = data_type_id(3)
  type (data_type_id), parameter :: REAL8    = data_type_id(4)
  type (data_type_id), parameter :: REAL42D  = data_type_id(5)
  type (data_type_id), parameter :: REAL82D  = data_type_id(6)
  type (data_type_id), parameter :: NO_TYPE  = data_type_id(100)

  ! Data block type
  type data_block_type
     private
     integer(kind=index_kind) :: nelements
     integer(kind=int4byte),  dimension(:),   pointer :: i4ptr
     integer(kind=int8byte),  dimension(:),   pointer :: i8ptr
     real(kind=real4byte),    dimension(:),   pointer :: r4ptr
     real(kind=real8byte),    dimension(:),   pointer :: r8ptr
     real(kind=real4byte),    dimension(:,:), pointer :: r4ptr2d
     real(kind=real8byte),    dimension(:,:), pointer :: r8ptr2d
     type(data_block_type),                   pointer :: next
  end type data_block_type

  ! Data array type definition
  type data_array_type
     private
     ! Size of data blocks (no of elements)
     integer(kind=index_kind) :: blocksize
     ! Width of data block (if 2D array)
     integer :: width
     ! Type of data in the array
     type (data_type_id) :: type
     ! Elements loaded so far
     integer(kind=index_kind) :: nelements
     ! Data blocks
     type(data_block_type), pointer :: first
     type(data_block_type), pointer :: last
  end type data_array_type

  interface operator(.eq.)
     module procedure types_eq
  end interface
  interface operator(.ne.)
     module procedure types_ne
  end interface

  !
  ! Public components of the module:
  !
  ! Data type identifiers
  public :: data_type_id
  public :: INTEGER4
  public :: INTEGER8
  public :: REAL4
  public :: REAL8
  public :: REAL42D
  public :: REAL82D

  ! Array data type
  public :: data_array_type

  ! Generic interfaces
  interface data_array_add_elements
     module procedure data_array_add_elements_i4
     module procedure data_array_add_elements_i8
     module procedure data_array_add_elements_r4
     module procedure data_array_add_elements_r8
     module procedure data_array_add_elements_r4_2d
     module procedure data_array_add_elements_r8_2d
  end interface
  interface data_array_get_data
     module procedure data_array_get_data_i4
     module procedure data_array_get_data_i8
     module procedure data_array_get_data_r4
     module procedure data_array_get_data_r8
     module procedure data_array_get_data_r4_2d
     module procedure data_array_get_data_r8_2d
  end interface

  ! Subroutines
  public :: data_array_init
  public :: data_array_dealloc
  public :: data_array_add_elements
  public :: data_array_get_data

  ! Functions
  public :: data_array_size

contains

  logical function types_eq(t1, t2) result(res)

    implicit none
    type (data_type_id), intent(in) :: t1, t2
    res = t1%type.eq.t2%type

    return
  end function types_eq

  logical function types_ne(t1, t2) result(res)

    implicit none
    type (data_type_id), intent(in) :: t1, t2
    res = t1%type.ne.t2%type

    return
  end function types_ne

  function new_block(type, blocksize, width)
!
! Allocate a new data block. Returns null pointer on failure.
!
    implicit none
    type (data_block_type), pointer :: new_block
    type (data_type_id)             :: type
    integer(kind=index_kind)         :: blocksize
    integer                         :: width
    integer                         :: istat

    allocate(new_block, stat=istat)
    if(istat.ne.0)then
       nullify(new_block)
       return
    endif

    new_block%nelements = 0
    nullify(new_block%next)
    
    nullify(new_block%i4ptr)
    nullify(new_block%i8ptr)
    nullify(new_block%r4ptr)
    nullify(new_block%r8ptr)
    nullify(new_block%r4ptr2d)
    nullify(new_block%r8ptr2d)

    if(type.eq.INTEGER4)then
       allocate(new_block%i4ptr(blocksize), stat=istat)
    else if(type.eq.INTEGER8)then
       allocate(new_block%i8ptr(blocksize), stat=istat)
    else if(type.eq.REAL4)then
       allocate(new_block%r4ptr(blocksize), stat=istat)
    else if(type.eq.REAL8)then
       allocate(new_block%r8ptr(blocksize), stat=istat)
    else if(type.eq.REAL42D)then
       allocate(new_block%r4ptr2d(width,blocksize), stat=istat)
    else if(type.eq.REAL82D)then
       allocate(new_block%r8ptr2d(width,blocksize), stat=istat)
    else
       stop'new_block() - Invalid data type'
    endif

    if(istat.ne.0)nullify(new_block)

    return
  end function new_block
  

  subroutine data_array_init(darray, nelements)
!
! Initialise a newly declared data array
!
    implicit none
    type (data_array_type)            :: darray
    integer(kind=index_kind), optional :: nelements

    darray%nelements = 0
    darray%type      = NO_TYPE
    if(present(nelements))then
       ! If we know size in advance can just make one
       ! big block
       darray%blocksize = nelements
    else
       ! Otherwise will have to split data into fixed size blocks
       darray%blocksize = DEFAULT_BLOCKSIZE
    endif
    nullify(darray%first)
    nullify(darray%last)

    return
  end subroutine data_array_init


  subroutine data_array_dealloc(darray)
!
! Deallocate storage associated with an array
!
    implicit none
    type (data_array_type)          :: darray
    type (data_block_type), pointer :: dblock, tmp

    dblock => darray%first
    do while(associated(dblock))
       if(associated(dblock%i4ptr))deallocate(dblock%i4ptr)
       if(associated(dblock%i8ptr))deallocate(dblock%i8ptr)
       if(associated(dblock%r4ptr))deallocate(dblock%r4ptr)
       if(associated(dblock%r8ptr))deallocate(dblock%r8ptr)
       if(associated(dblock%r4ptr2d))deallocate(dblock%r4ptr2d)
       if(associated(dblock%r8ptr2d))deallocate(dblock%r8ptr2d)
       tmp    => dblock
       dblock => dblock%next
       deallocate(tmp)
    end do
    call data_array_init(darray)

    return
  end subroutine data_array_dealloc
!
! INTEGER*4 routines
! ------------------
!
  subroutine data_array_add_elements_i4(darray, data, stat)
!
! Add data to an array
!
    implicit none
    ! Parameters
    type (data_array_type)               :: darray
    integer(kind=int4byte), dimension(:) :: data
    integer, intent(out) :: stat
    ! Internal
    integer(kind=index_kind) :: nleft, nadd, nadded
    integer(kind=index_kind) :: n_in_block, nleft_in_block
    type (data_type_id) :: this_type = INTEGER4

    stat = 0

    ! Establish type of array
    if(darray%type.eq.NO_TYPE)then
       darray%type = this_type
       darray%width = 1
    else if(darray%type.ne.this_type)then
       write(0,*)"data_array_add_elements() - inconsistent types!"
       stop
    endif

    ! Add the data
    nleft  = size(data)
    nadded = 0
    do while(nleft.gt.0)
       
       ! Make first block if necessary
       if(.not.associated(darray%first))then
          darray%first => new_block(this_type, darray%blocksize, darray%width)
          if(.not.associated(darray%first))then
             stat = -1
             return
          endif
          darray%last  => darray%first
       endif
       
       ! Make a new block if current block is full
       if(darray%last%nelements.eq.darray%blocksize)then
          darray%last%next => new_block(this_type, darray%blocksize, &
               darray%width)
          if(.not.associated(darray%last%next))then
             stat = -1
             return
          endif
          darray%last      => darray%last%next
       endif
       
       ! Add as many elements as will fit
       n_in_block     = darray%last%nelements
       nleft_in_block = darray%blocksize - n_in_block
       nadd = min(nleft_in_block, nleft)
       darray%last%i4ptr(n_in_block+1:n_in_block+nadd) = &
            data(nadded+1:nadded+nadd)
       nadded = nadded + nadd
       darray%last%nelements = darray%last%nelements + nadd
       nleft = nleft - nadd

    end do

    darray%nelements = darray%nelements + size(data)

    return
  end subroutine data_array_add_elements_i4


  subroutine data_array_get_data_i4(darray, res, stat)
!
! Return a pointer to the data, copying it if necessary
!
    implicit none
    ! Parameters
    type (data_array_type)                        :: darray
    integer,                          intent(out) :: stat
    ! Pointer to return
    integer(kind=int4byte), dimension(:), pointer :: res
    ! Internal
    integer(kind=index_kind) :: nblocks, ncopied
    type (data_block_type), pointer :: dblock
    integer :: ierr
    
    stat = 0

    nblocks = 0
    dblock => darray%first
    do while(associated(dblock))
       nblocks = nblocks + 1
       dblock => dblock%next
    end do
    
    if(nblocks.eq.1.and.darray%nelements.eq.darray%blocksize)then
       ! All in one block, so return pointer to existing data
       res => darray%first%i4ptr
       nullify(darray%first%i4ptr)
    else
       ! Multiple blocks, will have to copy the data
       allocate(res(darray%nelements), stat=ierr)
       if(ierr.ne.0)then
          stat = -1
          return
       endif
       dblock => darray%first
       ncopied = 0
       do while(associated(dblock))
          res(ncopied+1:ncopied+dblock%nelements) = &
               dblock%i4ptr(1:dblock%nelements)
          ncopied = ncopied + dblock%nelements
          dblock => dblock%next
       end do
    endif
    
    call data_array_dealloc(darray)    

    return
  end subroutine data_array_get_data_i4

!
! INTEGER*8 routines
! ------------------
!
  subroutine data_array_add_elements_i8(darray, data, stat)
!
! Add data to an array
!
    implicit none
    ! Parameters
    type (data_array_type)               :: darray
    integer(kind=int8byte), dimension(:) :: data
    integer, intent(out) :: stat
    ! Internal
    integer(kind=index_kind) :: nleft, nadd, nadded
    integer(kind=index_kind) :: n_in_block, nleft_in_block
    type (data_type_id) :: this_type = INTEGER8

    stat = 0

    ! Establish type of array
    if(darray%type.eq.NO_TYPE)then
       darray%type = this_type
       darray%width = 1
    else if(darray%type.ne.this_type)then
       write(0,*)"data_array_add_elements() - inconsistent types!"
       stop
    endif

    ! Add the data
    nleft  = size(data)
    nadded = 0
    do while(nleft.gt.0)
       
       ! Make first block if necessary
       if(.not.associated(darray%first))then
          darray%first => new_block(this_type, darray%blocksize, darray%width)
          if(.not.associated(darray%first))then
             stat = -1
             return
          endif
          darray%last  => darray%first
       endif
       
       ! Make a new block if current block is full
       if(darray%last%nelements.eq.darray%blocksize)then
          darray%last%next => new_block(this_type, darray%blocksize, &
               darray%width)
          if(.not.associated(darray%last%next))then
             stat = -1
             return
          endif
          darray%last      => darray%last%next
       endif
       
       ! Add as many elements as will fit
       n_in_block     = darray%last%nelements
       nleft_in_block = darray%blocksize - n_in_block
       nadd = min(nleft_in_block, nleft)
       darray%last%i8ptr(n_in_block+1:n_in_block+nadd) = &
            data(nadded+1:nadded+nadd)
       nadded = nadded + nadd
       darray%last%nelements = darray%last%nelements + nadd
       nleft = nleft - nadd

    end do

    darray%nelements = darray%nelements + size(data)

    return
  end subroutine data_array_add_elements_i8


  subroutine data_array_get_data_i8(darray, res, stat)
!
! Return a pointer to the data, copying it if necessary
!
    implicit none
    ! Parameters
    type (data_array_type)                        :: darray
    integer,                          intent(out) :: stat
    ! Pointer to return
    integer(kind=int8byte), dimension(:), pointer :: res
    ! Internal
    integer(kind=index_kind) :: nblocks, ncopied
    type (data_block_type), pointer :: dblock
    integer :: ierr
    
    stat = 0

    nblocks = 0
    dblock => darray%first
    do while(associated(dblock))
       nblocks = nblocks + 1
       dblock => dblock%next
    end do
    
    if(nblocks.eq.1.and.darray%nelements.eq.darray%blocksize)then
       ! All in one block, so return pointer to existing data
       res => darray%first%i8ptr
       nullify(darray%first%i8ptr)
    else
       ! Multiple blocks, will have to copy the data
       allocate(res(darray%nelements), stat=ierr)
       if(ierr.ne.0)then
          stat = -1
          return
       endif
       dblock => darray%first
       ncopied = 0
       do while(associated(dblock))
          res(ncopied+1:ncopied+dblock%nelements) = &
               dblock%i8ptr(1:dblock%nelements)
          ncopied = ncopied + dblock%nelements
          dblock => dblock%next
       end do
    endif
    
    call data_array_dealloc(darray)    

    return
  end subroutine data_array_get_data_i8

!
! REAL*4 routines
! ------------------
!
  subroutine data_array_add_elements_r4(darray, data, stat)
!
! Add data to an array
!
    implicit none
    ! Parameters
    type (data_array_type)               :: darray
    real(kind=real4byte),   dimension(:) :: data
    integer, intent(out) :: stat
    ! Internal
    integer(kind=index_kind) :: nleft, nadd, nadded
    integer(kind=index_kind) :: n_in_block, nleft_in_block
    type (data_type_id) :: this_type = REAL4

    stat = 0

    ! Establish type of array
    if(darray%type.eq.NO_TYPE)then
       darray%type = this_type
       darray%width = 1
    else if(darray%type.ne.this_type)then
       write(0,*)"data_array_add_elements() - inconsistent types!"
       stop
    endif

    ! Add the data
    nleft  = size(data)
    nadded = 0
    do while(nleft.gt.0)
       
       ! Make first block if necessary
       if(.not.associated(darray%first))then
          darray%first => new_block(this_type, darray%blocksize, darray%width)
          if(.not.associated(darray%first))then
             stat = -1
             return
          endif
          darray%last  => darray%first
       endif
       
       ! Make a new block if current block is full
       if(darray%last%nelements.eq.darray%blocksize)then
          darray%last%next => new_block(this_type, darray%blocksize, &
               darray%width)
          if(.not.associated(darray%last%next))then
             stat = -1
             return
          endif
          darray%last      => darray%last%next
       endif
       
       ! Add as many elements as will fit
       n_in_block     = darray%last%nelements
       nleft_in_block = darray%blocksize - n_in_block
       nadd = min(nleft_in_block, nleft)
       darray%last%r4ptr(n_in_block+1:n_in_block+nadd) = &
            data(nadded+1:nadded+nadd)
       nadded = nadded + nadd
       darray%last%nelements = darray%last%nelements + nadd
       nleft = nleft - nadd

    end do

    darray%nelements = darray%nelements + size(data)

    return
  end subroutine data_array_add_elements_r4


  subroutine data_array_get_data_r4(darray, res, stat)
!
! Return a pointer to the data, copying it if necessary
!
    implicit none
    ! Parameters
    type (data_array_type)                        :: darray
    integer,                          intent(out) :: stat
    ! Pointer to return
    real(kind=real4byte),   dimension(:), pointer :: res
    ! Internal
    integer(kind=index_kind) :: nblocks, ncopied
    type (data_block_type), pointer :: dblock
    integer :: ierr
    
    stat = 0

    nblocks = 0
    dblock => darray%first
    do while(associated(dblock))
       nblocks = nblocks + 1
       dblock => dblock%next
    end do
    
    if(nblocks.eq.1.and.darray%nelements.eq.darray%blocksize)then
       ! All in one block, so return pointer to existing data
       res => darray%first%r4ptr
       nullify(darray%first%r4ptr)
    else
       ! Multiple blocks, will have to copy the data
       allocate(res(darray%nelements), stat=ierr)
       if(ierr.ne.0)then
          stat = -1
          return
       endif
       dblock => darray%first
       ncopied = 0
       do while(associated(dblock))
          res(ncopied+1:ncopied+dblock%nelements) = &
               dblock%r4ptr(1:dblock%nelements)
          ncopied = ncopied + dblock%nelements
          dblock => dblock%next
       end do
    endif
    
    call data_array_dealloc(darray)    

    return
  end subroutine data_array_get_data_r4

!
! REAL*8 routines
! ------------------
!
  subroutine data_array_add_elements_r8(darray, data, stat)
!
! Add data to an array
!
    implicit none
    ! Parameters
    type (data_array_type)               :: darray
    real(kind=real8byte),   dimension(:) :: data
    integer, intent(out) :: stat
    ! Internal
    integer(kind=index_kind) :: nleft, nadd, nadded
    integer(kind=index_kind) :: n_in_block, nleft_in_block
    type (data_type_id) :: this_type = REAL8

    stat = 0

    ! Establish type of array
    if(darray%type.eq.NO_TYPE)then
       darray%type = this_type
       darray%width = 1
    else if(darray%type.ne.this_type)then
       write(0,*)"data_array_add_elements() - inconsistent types!"
       stop
    endif

    ! Add the data
    nleft  = size(data)
    nadded = 0
    do while(nleft.gt.0)
       
       ! Make first block if necessary
       if(.not.associated(darray%first))then
          darray%first => new_block(this_type, darray%blocksize, darray%width)
          if(.not.associated(darray%first))then
             stat = -1
             return
          endif
          darray%last  => darray%first
       endif
       
       ! Make a new block if current block is full
       if(darray%last%nelements.eq.darray%blocksize)then
          darray%last%next => new_block(this_type, darray%blocksize, &
               darray%width)
          if(.not.associated(darray%last%next))then
             stat = -1
             return
          endif
          darray%last      => darray%last%next
       endif
       
       ! Add as many elements as will fit
       n_in_block     = darray%last%nelements
       nleft_in_block = darray%blocksize - n_in_block
       nadd = min(nleft_in_block, nleft)
       darray%last%r8ptr(n_in_block+1:n_in_block+nadd) = &
            data(nadded+1:nadded+nadd)
       nadded = nadded + nadd
       darray%last%nelements = darray%last%nelements + nadd
       nleft = nleft - nadd

    end do

    darray%nelements = darray%nelements + size(data)

    return
  end subroutine data_array_add_elements_r8


  subroutine data_array_get_data_r8(darray, res, stat)
!
! Return a pointer to the data, copying it if necessary
!
    implicit none
    ! Parameters
    type (data_array_type)                        :: darray
    integer,                          intent(out) :: stat
    ! Pointer to return
    real(kind=real8byte),   dimension(:), pointer :: res
    ! Internal
    integer(kind=index_kind) :: nblocks, ncopied
    type (data_block_type), pointer :: dblock
    integer :: ierr
    
    stat = 0

    nblocks = 0
    dblock => darray%first
    do while(associated(dblock))
       nblocks = nblocks + 1
       dblock => dblock%next
    end do
    
    if(nblocks.eq.1.and.darray%nelements.eq.darray%blocksize)then
       ! All in one block, so return pointer to existing data
       res => darray%first%r8ptr
       nullify(darray%first%r8ptr)
    else
       ! Multiple blocks, will have to copy the data
       allocate(res(darray%nelements), stat=ierr)
       if(ierr.ne.0)then
          stat = -1
          return
       endif
       dblock => darray%first
       ncopied = 0
       do while(associated(dblock))
          res(ncopied+1:ncopied+dblock%nelements) = &
               dblock%r8ptr(1:dblock%nelements)
          ncopied = ncopied + dblock%nelements
          dblock => dblock%next
       end do
    endif
    
    call data_array_dealloc(darray)    

    return
  end subroutine data_array_get_data_r8


!
! 2D REAL*4 routines
! ------------------
!
  subroutine data_array_add_elements_r4_2d(darray, data, stat)
!
! Add data to an array
!
    implicit none
    ! Parameters
    type (data_array_type)               :: darray
    real(kind=real4byte), dimension(:,:) :: data
    integer,                 intent(out) :: stat
    ! Internal
    integer(kind=index_kind) :: nleft, nadd, nadded
    integer(kind=index_kind) :: n_in_block, nleft_in_block
    type (data_type_id) :: this_type = REAL42D

    stat = 0

    ! Establish type of array
    if(darray%type.eq.NO_TYPE)then
       darray%type = this_type
       darray%width = size(data,1)
    else if(darray%type.ne.this_type)then
       write(0,*)"data_array_add_elements() - inconsistent types!"
       stop
    endif

    ! Add the data
    nleft  = size(data,2)
    nadded = 0
    do while(nleft.gt.0)
       
       ! Make first block if necessary
       if(.not.associated(darray%first))then
          darray%first => new_block(this_type, darray%blocksize, darray%width)
          if(.not.associated(darray%first))then
             stat = -1
             return
          endif
          darray%last  => darray%first
       endif
       
       ! Make a new block if current block is full
       if(darray%last%nelements.eq.darray%blocksize)then
          darray%last%next => new_block(this_type, darray%blocksize, darray%width)
          if(.not.associated(darray%last%next))then
             stat = -1
             return
          endif
          darray%last      => darray%last%next
       endif
       
       ! Add as many elements as will fit
       n_in_block     = darray%last%nelements
       nleft_in_block = darray%blocksize - n_in_block
       nadd = min(nleft_in_block, nleft)
       darray%last%r4ptr2d(:,n_in_block+1:n_in_block+nadd) = &
            data(:,nadded+1:nadded+nadd)
       nadded = nadded + nadd
       darray%last%nelements = darray%last%nelements + nadd
       nleft = nleft - nadd

    end do

    darray%nelements = darray%nelements + size(data,2)

    return
  end subroutine data_array_add_elements_r4_2d


  subroutine data_array_get_data_r4_2d(darray, res, stat)
!
! Return a pointer to the data, copying it if necessary
!
    implicit none
    ! Parameters
    type (data_array_type)                      :: darray
    ! Pointer to return
    real(kind=real4byte), dimension(:,:), pointer :: res
    integer, intent(out) :: stat
    ! Internal
    integer(kind=index_kind) :: nblocks, ncopied
    type (data_block_type), pointer :: dblock
    integer :: ierr

    stat = 0

    nblocks = 0
    dblock => darray%first
    do while(associated(dblock))
       nblocks = nblocks + 1
       dblock => dblock%next
    end do
    
    if(nblocks.eq.1.and.darray%nelements.eq.darray%blocksize)then
       ! All in one block, so return pointer to existing data
       res => darray%first%r4ptr2d
       nullify(darray%first%r4ptr2d)
    else
       ! Multiple blocks, will have to copy the data
       allocate(res(darray%width, darray%nelements), stat=ierr)
       if(ierr.ne.0)then
          stat = -1
          return
       endif
       dblock => darray%first
       ncopied = 0
       do while(associated(dblock))
          res(:,ncopied+1:ncopied+dblock%nelements) = &
               dblock%r4ptr2d(:,1:dblock%nelements)
          ncopied = ncopied + dblock%nelements
          dblock => dblock%next
       end do
    endif

    call data_array_dealloc(darray)    
    
    return
  end subroutine data_array_get_data_r4_2d


!
! 2D REAL*8 routines
! ------------------
!
  subroutine data_array_add_elements_r8_2d(darray, data, stat)
!
! Add data to an array
!
    implicit none
    ! Parameters
    type (data_array_type)               :: darray
    real(kind=real8byte), dimension(:,:) :: data
    integer,                 intent(out) :: stat
    ! Internal
    integer(kind=index_kind) :: nleft, nadd, nadded
    integer(kind=index_kind) :: n_in_block, nleft_in_block
    type (data_type_id) :: this_type = REAL82D

    stat = 0

    ! Establish type of array
    if(darray%type.eq.NO_TYPE)then
       darray%type = this_type
       darray%width = size(data,1)
    else if(darray%type.ne.this_type)then
       write(0,*)"data_array_add_elements() - inconsistent types!"
       stop
    endif

    ! Add the data
    nleft  = size(data,2)
    nadded = 0
    do while(nleft.gt.0)
       
       ! Make first block if necessary
       if(.not.associated(darray%first))then
          darray%first => new_block(this_type, darray%blocksize, darray%width)
          if(.not.associated(darray%first))then
             stat = -1
             return
          endif
          darray%last  => darray%first
       endif
       
       ! Make a new block if current block is full
       if(darray%last%nelements.eq.darray%blocksize)then
          darray%last%next => new_block(this_type, darray%blocksize, darray%width)
          if(.not.associated(darray%last%next))then
             stat = -1
             return
          endif
          darray%last      => darray%last%next
       endif
       
       ! Add as many elements as will fit
       n_in_block     = darray%last%nelements
       nleft_in_block = darray%blocksize - n_in_block
       nadd = min(nleft_in_block, nleft)
       darray%last%r8ptr2d(:,n_in_block+1:n_in_block+nadd) = &
            data(:,nadded+1:nadded+nadd)
       nadded = nadded + nadd
       darray%last%nelements = darray%last%nelements + nadd
       nleft = nleft - nadd

    end do

    darray%nelements = darray%nelements + size(data,2)

    return
  end subroutine data_array_add_elements_r8_2d


  subroutine data_array_get_data_r8_2d(darray, res, stat)
!
! Return a pointer to the data, copying it if necessary
!
    implicit none
    ! Parameters
    type (data_array_type)                      :: darray
    ! Pointer to return
    real(kind=real8byte), dimension(:,:), pointer :: res
    integer, intent(out) :: stat
    ! Internal
    integer(kind=index_kind) :: nblocks, ncopied
    type (data_block_type), pointer :: dblock
    integer :: ierr

    stat = 0

    nblocks = 0
    dblock => darray%first
    do while(associated(dblock))
       nblocks = nblocks + 1
       dblock => dblock%next
    end do
    
    if(nblocks.eq.1.and.darray%nelements.eq.darray%blocksize)then
       ! All in one block, so return pointer to existing data
       res => darray%first%r8ptr2d
       nullify(darray%first%r8ptr2d)
    else
       ! Multiple blocks, will have to copy the data
       allocate(res(darray%width, darray%nelements), stat=ierr)
       if(ierr.ne.0)then
          stat = -1
          return
       endif
       dblock => darray%first
       ncopied = 0
       do while(associated(dblock))
          res(:,ncopied+1:ncopied+dblock%nelements) = &
               dblock%r8ptr2d(:,1:dblock%nelements)
          ncopied = ncopied + dblock%nelements
          dblock => dblock%next
       end do
    endif

    call data_array_dealloc(darray)    
    
    return
  end subroutine data_array_get_data_r8_2d



  integer(kind=index_kind) function data_array_size(darray)
!
! Return the size of an array
!
    implicit none
    type (data_array_type)               :: darray

    data_array_size = darray%nelements

    return
  end function data_array_size

end module data_array
