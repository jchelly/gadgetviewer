module read_gadget_binary2

  use f90_util
  use key_file
  use read_gadget_definitions

  implicit none
  private
  save

  ! Routines in this module
  public :: binary2_snapshot_file_open
  public :: binary2_snapshot_file_close
  public :: binary2_snapshot_file_get_datasets
  public :: binary2_snapshot_file_read_header
  public :: binary2_snapshot_file_read_dataset

  ! Number of particles in the current file
  integer(kind=int4byte), dimension(0:5) :: npfile

  ! Maximum number of blocks we can read
  integer, parameter :: nblockmax = 500

  ! Whether we need to byteswap the data
  logical :: need_byteswap

  ! Information about each block we know how to read
  type block_definition_type
     ! 4 character TAG for this block
     character(len=4)        :: tag = ""
     ! Long name for this block
     character(len=500)      :: name = ""
     ! Whether this is an integer quantity
     logical                 :: is_integer = .false.
     ! Whether this is a vector quantity
     logical                 :: is_vector = .false.
     ! Which particle types have this quantity
     logical, dimension(0:5) :: type = (/.false., .false., .false., &
          .false., .false., .false. /)
  end type block_definition_type
  integer :: nblockdef = 0
  type (block_definition_type), dimension(nblockmax) :: blockdef

  ! Information about each block in the current file
  type block_type
     ! Which block definition this block corresponds to
     integer :: idef
     ! Size of each number in bytes
     integer :: byte_size
     ! Offset to the start of this block in the file
     integer :: offset
  end type block_type

  ! Whether he have a config file
  logical :: have_config = .false.

contains

  subroutine binary2_read_config_file(fname)

    implicit none
    character(len=*), optional :: fname
    integer          :: nb
    integer, dimension(6) :: list
    character(len=4), dimension(nblockmax) :: tags
    logical :: discard_block
    integer :: i, j, nlist

    ! Define blocks which are always available
    nblockdef = 4
    ! Positions
    blockdef(1)%tag = "POS"
    blockdef(1)%name = "Coordinates"
    blockdef(1)%is_integer = .false.
    blockdef(1)%is_vector  = .true.
    blockdef(1)%type = (/ .true., .true., .true., .true., .true., .true. /) 
    ! Velocities
    blockdef(2)%tag = "VEL"
    blockdef(2)%name = "Velocities"
    blockdef(2)%is_integer = .false.
    blockdef(2)%is_vector  = .true.
    blockdef(2)%type = (/ .true., .true., .true., .true., .true., .true. /) 
    ! Particle IDs
    blockdef(3)%tag = "ID"
    blockdef(3)%name = "ParticleIDs"
    blockdef(3)%is_integer = .true.
    blockdef(3)%is_vector  = .false.
    blockdef(3)%type = (/ .true., .true., .true., .true., .true., .true. /) 
    ! Mass (these need special treatment!)
    blockdef(4)%tag = "MASS"
    blockdef(4)%name = "Mass"
    blockdef(4)%is_integer = .false.
    blockdef(4)%is_vector  = .false.
    blockdef(4)%type = (/ .true., .true., .true., .true., .true., .true. /) 
    ! Read key file if specified
    if(present(fname))then
       call read_key_file(fname)
       call get_group_names(nb, tags)
       have_config = .true.
    else
       nb = 0
    endif
    if(nb.eq.0)then
       ! Make a new file with default settings
       nblockdef = 7
       ! Internal energy
       blockdef(5)%tag        = "U"
       blockdef(5)%name = "InternalEnergy"
       blockdef(5)%is_integer = .false.
       blockdef(5)%is_vector  = .false.
       blockdef(5)%type = (/ .true.,.false.,.false.,.false.,.false.,.false. /) 
       ! Density
       blockdef(6)%tag        = "RHO"
       blockdef(6)%name = "Density"
       blockdef(6)%is_integer = .false.
       blockdef(6)%is_vector  = .false.
       blockdef(6)%type = (/ .true.,.false.,.false.,.false.,.false.,.false. /) 
       ! Smoothing length
       blockdef(7)%tag        = "HSML"
       blockdef(1)%name       = "SmoothingLength"
       blockdef(7)%is_integer = .false.
       blockdef(7)%is_vector  = .false.
       blockdef(7)%type = (/ .true.,.false.,.false.,.false.,.false.,.false. /) 
       ! Set keys
       if(present(fname))then
          do i = 5, nblockdef, 1
             call set_key(blockdef(i)%tag, "Displayed Name", blockdef(i)%name)
             call flags_to_list(blockdef(i)%type, list, nlist)
             call set_key(blockdef(i)%tag, "Particle Types", list(1:nlist))
             call set_key(blockdef(i)%tag, "IsInteger", blockdef(i)%is_integer)
             call set_key(blockdef(i)%tag, "IsVector",  blockdef(i)%is_vector)
          end do
          call write_key_file(fname)
       endif
    else
       ! Read extra blocks from file
       nb = max(nb, nblockmax-4)
       do i = 1, nb, 1
          if(nblockdef.lt.nblockmax)then
             nblockdef = nblockdef + 1
          else
             exit
          endif
          call get_key(tags(i), "Displayed Name", blockdef(nblockdef)%name)
          call get_key(tags(i), "Particle Types", list(1:nlist))
          call get_key(tags(i), "IsInteger", blockdef(nblockdef)%is_integer)
          call get_key(tags(i), "IsVector",  blockdef(nblockdef)%is_vector)
          call list_to_flags(list, blockdef(nblockdef)%type)
          ! Discard if name or tag has zero length
          if(len_trim(blockdef(nblockdef)%name).eq.0)then
             nblockdef = nblockdef - 1
             exit
          endif
          if(len_trim(blockdef(nblockdef)%tag).eq.0)then
             nblockdef = nblockdef - 1
             exit
          endif
          ! Discard if block conflicts with something we always read
          discard_block = .false.
          do j = 1, 4, 1
             if(blockdef(nblockdef)%name.eq.blockdef(j)%name.or. &
                  blockdef(nblockdef)%tag.eq.blockdef(j)%tag)then
                nblockdef = nblockdef - 1
                discard_block = .true.
                exit
             endif
          end do
          if(discard_block)exit
       end do
    endif
    if(present(fname))then
       call close_key_file()
    endif

    return
  end subroutine binary2_read_config_file


  subroutine binary2_snapshot_file_open(fname, iostat)
!
! Open a new snapshot file
!
    implicit none
    character(len=*), intent(in)  :: fname
    integer,          intent(out) :: iostat

    ! Use default set of blocks if no config file is set
    if(.not.have_config)call binary2_read_config_file()

    ! Attempt to open the snapshot file
    call try_open()

    ! Clean up if something went wrong
    if(iostat.ne.0)then
       call close_binary()
    endif

    return

  contains

    subroutine try_open()

      implicit none
      integer(kind=int4byte) :: dummy1

      ! Open the file
      call open_binary(fname, iostat)
      if(iostat.ne.0)return
      call read_binary(dummy1, iostat, pos=0)
      if(iostat.ne.0)return

      ! First record should have size 8
      if(dummy1.eq.8)then
         need_byteswap = .false.
      else if(dummy1.eq.134217728) then
         need_byteswap = .true.
      else
         ! Not a Gadget type 2 file?
         iostat = -1
         return
      endif
    
      return
    end subroutine try_open

  end subroutine binary2_snapshot_file_open


  subroutine binary2_snapshot_file_close(iostat)
!
! Close a snapshot file
!
    implicit none
    integer,          intent(out) :: iostat

    iostat = -50 ! Not implemented yet

    return
  end subroutine binary2_snapshot_file_close


  subroutine binary2_snapshot_file_get_datasets(itype, ndataset, dataset_name,&
       iostat)
!
! Return the number of readable datasets for a particle type and their names.
! Allocates dataset_name to the appropriate size.
!
    implicit none
    ! Parameters
    integer, intent(in)  :: itype
    integer, intent(out) :: ndataset, iostat
    character(len=*), dimension(:), pointer :: dataset_name
    character(len=dset_name_length) :: group_name

    iostat = -50 ! Not implemented yet

    return
  end subroutine binary2_snapshot_file_get_datasets


  subroutine binary2_snapshot_file_read_header(header, iostat)
!
! Read the header from a snapshot file
!
    implicit none
    type (snapshot_header_type) :: header
    integer,          intent(out) :: iostat

    iostat = -50 ! Not implemented yet

    return
  end subroutine binary2_snapshot_file_read_header


  subroutine binary2_snapshot_file_read_dataset(itype, dataset_name, data, &
       iostat)
!
! Read a dataset
!
    implicit none
    integer,             intent(in)  :: itype
    character(len=*),    intent(in)  :: dataset_name
    type (dataset_type), intent(out) :: data
    integer,             intent(out) :: iostat

    iostat = -50 ! Not implemented yet

    return
  end subroutine binary2_snapshot_file_read_dataset

  subroutine flags_to_list(flags, list, n)

    implicit none
    logical, dimension(:), intent(in)  :: flags
    integer, dimension(:), intent(out) :: list
    integer,               intent(out) :: n
    integer :: i
    
    n = 0
    do i = 1, size(flags), 1
       if(flags(i))then
          n = n + 1
          list(n) = i - 1
       endif
       if(n.gt.size(list))exit
    end do

    return
  end subroutine flags_to_list


  subroutine list_to_flags(list, flags)

    implicit none
    logical, dimension(:), intent(out) :: flags
    integer, dimension(:), intent(in)  :: list
    integer :: i, n

    flags = .false.
    do i = 1, size(list), 1
       if(list(i).ge.0.and.list(i).lt.size(flags))then
          flags(list(i)+1) = .true.
       endif
    end do

    return
  end subroutine list_to_flags

end module read_gadget_binary2
