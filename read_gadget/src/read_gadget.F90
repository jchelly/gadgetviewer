module read_gadget

  use f90_util
  use read_gadget_definitions
  use read_gadget_hdf5
  use read_gadget_binary1
  use read_gadget_binary2

  implicit none
  private
  save

  ! Routines in this module
  public :: snapshot_file_open
  public :: snapshot_file_close
  public :: snapshot_file_get_datasets
  public :: snapshot_file_read_header
  public :: snapshot_file_read_dataset

  ! Export type definitions from read_gadget_definitions module
  public :: snapshot_header_type
  public :: dataset_type
  public :: dataset_init
  public :: dataset_dealloc
  public :: get_data

  ! Type of snapshot we're reading
  integer :: snapformat

  ! Routine to read directly into an array rather than a dataset object
  interface snapshot_file_read_data
     module procedure snapshot_file_read_data_r4_1d
     module procedure snapshot_file_read_data_r4_2d
     module procedure snapshot_file_read_data_r8_1d
     module procedure snapshot_file_read_data_r8_2d
     module procedure snapshot_file_read_data_i4_1d
     module procedure snapshot_file_read_data_i4_2d
     module procedure snapshot_file_read_data_i8_1d
     module procedure snapshot_file_read_data_i8_2d
  end interface
  public :: snapshot_file_read_data

contains


  subroutine snapshot_file_open(fname, iostat)
!
! Open a new snapshot file
!
    implicit none
    character(len=*), intent(in)  :: fname
    integer,          intent(out) :: iostat

    ! Try HDF5
    call hdf5_snapshot_file_open(fname, iostat)
    if(iostat.eq.0)then
       snapformat = GADGET_HDF5
       return
    endif

    ! Try binary #1
    call binary1_snapshot_file_open(fname, iostat)
    if(iostat.eq.0)then
       snapformat = GADGET_BINARY1
       return
    endif

    ! Try binary #2
    call binary2_snapshot_file_open(fname, iostat)
    if(iostat.eq.0)then
       snapformat = GADGET_BINARY2
       return
    endif

    ! Failed to open it
    iostat = -1

    return
  end subroutine snapshot_file_open


  subroutine snapshot_file_close(iostat)
!
! Close a snapshot file
!
    implicit none
    integer,          intent(out) :: iostat

    select case(snapformat)
    case(GADGET_HDF5)
       call hdf5_snapshot_file_close(iostat)
    case(GADGET_BINARY1)
       call binary1_snapshot_file_close(iostat)
    case(GADGET_BINARY2)
       call binary2_snapshot_file_close(iostat)
    end select

    return
  end subroutine snapshot_file_close


  subroutine snapshot_file_get_datasets(itype, ndataset, dataset_name, iostat)
!
! Return the number of datasets for a particle type and their names.
! Allocates dataset_name to the appropriate size.
!
    implicit none
    integer, intent(in)  :: itype
    integer, intent(out) :: ndataset, iostat
    character(len=*), dimension(:), pointer :: dataset_name

    ! Get the dataset list
    select case(snapformat)
    case(GADGET_HDF5)
       call hdf5_snapshot_file_get_datasets(itype, ndataset, dataset_name, iostat)
    case(GADGET_BINARY1)
       call binary1_snapshot_file_get_datasets(itype, ndataset, dataset_name, iostat)
    case(GADGET_BINARY2)
       call binary2_snapshot_file_get_datasets(itype, ndataset, dataset_name, iostat)
    end select

    return
  end subroutine snapshot_file_get_datasets


  subroutine snapshot_file_read_header(header, iostat)
!
! Read the header from a snapshot file
!
    implicit none
    type (snapshot_header_type) :: header
    integer,          intent(out) :: iostat

    ! Get the file header
    select case(snapformat)
    case(GADGET_HDF5)
       call hdf5_snapshot_file_read_header(header, iostat)
    case(GADGET_BINARY1)
       call binary1_snapshot_file_read_header(header, iostat)
    case(GADGET_BINARY2)
       call binary2_snapshot_file_read_header(header, iostat)
    end select

    return
  end subroutine snapshot_file_read_header


  subroutine snapshot_file_read_dataset(itype, dataset_name, data, &
       iostat)
!
! Read a dataset
!
    implicit none
    integer,             intent(in)  :: itype
    character(len=*),    intent(in)  :: dataset_name
    type (dataset_type), intent(out) :: data
    integer,             intent(out) :: iostat

    ! Read the dataset
    select case(snapformat)
    case(GADGET_HDF5)
       call hdf5_snapshot_file_read_dataset(itype, dataset_name, data, iostat)
    case(GADGET_BINARY1)
       call binary1_snapshot_file_read_dataset(itype, dataset_name, data, iostat)
    case(GADGET_BINARY2)
       call binary2_snapshot_file_read_dataset(itype, dataset_name, data, iostat)
    end select

    return
  end subroutine snapshot_file_read_dataset

!
! Routines to read a dataset directly into an array
!
  subroutine snapshot_file_read_data_r4_1d(itype, dataset_name, arr, iostat)

    implicit none
    integer,             intent(in)    :: itype
    character(len=*),    intent(in)    :: dataset_name
    real(kind=real4byte), dimension(:) :: arr
    integer                            :: iostat
    type (dataset_type)                :: data

    call dataset_init(data)
    call snapshot_file_read_dataset(itype, dataset_name, data, &
         iostat)
    if(iostat.eq.0)then
       call get_data(data, arr)
       call dataset_dealloc(data)
    endif

    return
  end subroutine snapshot_file_read_data_r4_1d


  subroutine snapshot_file_read_data_r4_2d(itype, dataset_name, arr, iostat)

    implicit none
    integer,             intent(in)    :: itype
    character(len=*),    intent(in)    :: dataset_name
    real(kind=real4byte), dimension(:,:) :: arr
    integer                            :: iostat
    type (dataset_type)                :: data

    call dataset_init(data)
    call snapshot_file_read_dataset(itype, dataset_name, data, &
         iostat)
    if(iostat.eq.0)then
       call get_data(data, arr)
       call dataset_dealloc(data)
    endif

    return
  end subroutine snapshot_file_read_data_r4_2d


  subroutine snapshot_file_read_data_r8_1d(itype, dataset_name, arr, iostat)

    implicit none
    integer,             intent(in)    :: itype
    character(len=*),    intent(in)    :: dataset_name
    real(kind=real8byte), dimension(:) :: arr
    integer                            :: iostat
    type (dataset_type)                :: data

    call dataset_init(data)
    call snapshot_file_read_dataset(itype, dataset_name, data, &
         iostat)
    if(iostat.eq.0)then
       call get_data(data, arr)
       call dataset_dealloc(data)
    endif

    return
  end subroutine snapshot_file_read_data_r8_1d


  subroutine snapshot_file_read_data_r8_2d(itype, dataset_name, arr, iostat)

    implicit none
    integer,             intent(in)    :: itype
    character(len=*),    intent(in)    :: dataset_name
    real(kind=real8byte), dimension(:,:) :: arr
    integer                            :: iostat
    type (dataset_type)                :: data

    call dataset_init(data)
    call snapshot_file_read_dataset(itype, dataset_name, data, &
         iostat)
    if(iostat.eq.0)then
       call get_data(data, arr)
       call dataset_dealloc(data)
    endif

    return
  end subroutine snapshot_file_read_data_r8_2d


  subroutine snapshot_file_read_data_i4_1d(itype, dataset_name, arr, iostat)

    implicit none
    integer,             intent(in)    :: itype
    character(len=*),    intent(in)    :: dataset_name
    integer(kind=int4byte), dimension(:) :: arr
    integer                            :: iostat
    type (dataset_type)                :: data

    call dataset_init(data)
    call snapshot_file_read_dataset(itype, dataset_name, data, &
         iostat)
    if(iostat.eq.0)then
       call get_data(data, arr)
       call dataset_dealloc(data)
    endif

    return
  end subroutine snapshot_file_read_data_i4_1d


  subroutine snapshot_file_read_data_i4_2d(itype, dataset_name, arr, iostat)

    implicit none
    integer,             intent(in)    :: itype
    character(len=*),    intent(in)    :: dataset_name
    integer(kind=int4byte), dimension(:,:) :: arr
    integer                            :: iostat
    type (dataset_type)                :: data

    call dataset_init(data)
    call snapshot_file_read_dataset(itype, dataset_name, data, &
         iostat)
    if(iostat.eq.0)then
       call get_data(data, arr)
       call dataset_dealloc(data)
    endif

    return
  end subroutine snapshot_file_read_data_i4_2d


  subroutine snapshot_file_read_data_i8_1d(itype, dataset_name, arr, iostat)

    implicit none
    integer,             intent(in)    :: itype
    character(len=*),    intent(in)    :: dataset_name
    integer(kind=int8byte), dimension(:) :: arr
    integer                            :: iostat
    type (dataset_type)                :: data

    call dataset_init(data)
    call snapshot_file_read_dataset(itype, dataset_name, data, &
         iostat)
    if(iostat.eq.0)then
       call get_data(data, arr)
       call dataset_dealloc(data)
    endif

    return
  end subroutine snapshot_file_read_data_i8_1d


  subroutine snapshot_file_read_data_i8_2d(itype, dataset_name, arr, iostat)

    implicit none
    integer,             intent(in)    :: itype
    character(len=*),    intent(in)    :: dataset_name
    integer(kind=int8byte), dimension(:,:) :: arr
    integer                            :: iostat
    type (dataset_type)                :: data

    call dataset_init(data)
    call snapshot_file_read_dataset(itype, dataset_name, data, &
         iostat)
    if(iostat.eq.0)then
       call get_data(data, arr)
       call dataset_dealloc(data)
    endif

    return
  end subroutine snapshot_file_read_data_i8_2d

end module read_gadget
