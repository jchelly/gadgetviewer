module read_gadget_definitions

  use f90_util
  use byteswapper

  implicit none
  private
  save
  
  ! Format constants
  integer, parameter, public :: GADGET_BINARY1 = 1
  integer, parameter, public :: GADGET_BINARY2 = 2
  integer, parameter, public :: GADGET_HDF5    = 3

  ! Maximum length of a dataset name
  integer, parameter, public :: dset_name_length = 500

  ! Information from the snapshot header
  type snapshot_header_type
     integer(kind=int4byte), dimension(6) :: NumPart_ThisFile
     integer(kind=int4byte), dimension(6) :: NumPart_Total
     integer(kind=int4byte), dimension(6) :: NumPart_Total_HighWord
     real(kind=real8byte)                 :: BoxSize
     real(kind=real8byte),   dimension(6) :: MassTable
     integer(kind=int4byte)               :: NumFilesPerSnapshot
     real(kind=real8byte)                 :: ExpansionFactor
     real(kind=real8byte)                 :: Redshift
  end type snapshot_header_type
  public :: snapshot_header_type

  ! Data for a dataset from one snapshot file
  type dataset_type
     integer               :: rank
     integer, dimension(7) :: dims
     integer               :: type
     integer(kind=int4byte), dimension(:),   pointer :: i4_1d => NULL()
     integer(kind=int4byte), dimension(:,:), pointer :: i4_2d => NULL()
     integer(kind=int8byte), dimension(:),   pointer :: i8_1d => NULL()
     integer(kind=int8byte), dimension(:,:), pointer :: i8_2d => NULL()
     real(kind=real4byte), dimension(:),   pointer :: r4_1d => NULL()
     real(kind=real4byte), dimension(:,:), pointer :: r4_2d => NULL()
     real(kind=real8byte), dimension(:),   pointer :: r8_1d => NULL()
     real(kind=real8byte), dimension(:,:), pointer :: r8_2d => NULL()
     character(len=dset_name_length) :: name
  end type dataset_type
  public :: dataset_type

  ! Routine to extract data from a dataset
  interface get_data
     module procedure get_data_real4_1d
     module procedure get_data_real4_2d
     module procedure get_data_real8_1d
     module procedure get_data_real8_2d
     module procedure get_data_integer4_1d
     module procedure get_data_integer4_2d
     module procedure get_data_integer8_1d
     module procedure get_data_integer8_2d
  end interface
  public :: get_data

  public :: dataset_init
  public :: dataset_dealloc

  public :: byteswap_header
  public :: byteswap_dataset

contains

  subroutine byteswap_header(header)

    implicit none
    type (snapshot_header_type) :: header

    call byteswap(header%NumPart_ThisFile)
    call byteswap(header%NumPart_Total)
    call byteswap(header%NumPart_Total_HighWord)
    call byteswap(header%BoxSize)
    call byteswap(header%MassTable)
    call byteswap(header%NumFilesPerSnapshot)
    call byteswap(header%ExpansionFactor)
    call byteswap(header%Redshift)

    return
  end subroutine byteswap_header

!
! Initialize a dataset
!
  subroutine dataset_init(dataset)

    implicit none
    type (dataset_type), intent(inout) :: dataset

    nullify(dataset%r4_1d)
    nullify(dataset%r4_2d)
    nullify(dataset%r8_1d)
    nullify(dataset%r8_2d)
    nullify(dataset%i4_1d)
    nullify(dataset%i4_2d)
    nullify(dataset%i8_1d)
    nullify(dataset%i8_2d)

    return
  end subroutine dataset_init
!
! Deallocate a dataset
!
  subroutine dataset_dealloc(dataset)

    implicit none
    type (dataset_type), intent(inout) :: dataset

    if(associated(dataset%r4_1d))deallocate(dataset%r4_1d)
    if(associated(dataset%r4_2d))deallocate(dataset%r4_2d)
    if(associated(dataset%r8_1d))deallocate(dataset%r8_1d)
    if(associated(dataset%r8_2d))deallocate(dataset%r8_2d)
    if(associated(dataset%i4_1d))deallocate(dataset%i4_1d)
    if(associated(dataset%i4_2d))deallocate(dataset%i4_2d)
    if(associated(dataset%i8_1d))deallocate(dataset%i8_1d)
    if(associated(dataset%i8_2d))deallocate(dataset%i8_2d)

    return
  end subroutine dataset_dealloc
!
! Routines to copy data out of a dataset object, converting to different
! precision if necessary
!
  subroutine get_data_real4_1d(dataset, out)

    implicit none
    type (dataset_type), intent(in) :: dataset
    real(kind=real4byte), dimension(:) :: out

    if(associated(dataset%r4_1d))then
       out = dataset%r4_1d
    else if(associated(dataset%r8_1d))then
       out = dataset%r8_1d
    else
       stop 'Dataset not allocated in get_data()!'
    endif

    return
  end subroutine get_data_real4_1d

  subroutine get_data_real4_2d(dataset, out)

    implicit none
    type (dataset_type), intent(in) :: dataset
    real(kind=real4byte), dimension(:,:) :: out

    if(associated(dataset%r4_2d))then
       out = dataset%r4_2d
    else if(associated(dataset%r8_2d))then
       out = dataset%r8_2d
    else
       stop 'Dataset not allocated in get_data()!'
    endif

    return
  end subroutine get_data_real4_2d

  subroutine get_data_real8_1d(dataset, out)

    implicit none
    type (dataset_type), intent(in) :: dataset
    real(kind=real8byte), dimension(:) :: out

    if(associated(dataset%r4_1d))then
       out = dataset%r4_1d
    else if(associated(dataset%r8_1d))then
       out = dataset%r8_1d
    else
       stop 'Dataset not allocated in get_data()!'
    endif

    return
  end subroutine get_data_real8_1d

  subroutine get_data_real8_2d(dataset, out)

    implicit none
    type (dataset_type), intent(in) :: dataset
    real(kind=real8byte), dimension(:,:) :: out

    if(associated(dataset%r4_2d))then
       out = dataset%r4_2d
    else if(associated(dataset%r8_2d))then
       out = dataset%r8_2d
    else
       stop 'Dataset not allocated in get_data()!'
    endif

    return
  end subroutine get_data_real8_2d

  subroutine get_data_integer4_1d(dataset, out)

    implicit none
    type (dataset_type), intent(in) :: dataset
    integer(kind=int4byte), dimension(:) :: out

    if(associated(dataset%i4_1d))then
       out = dataset%i4_1d
    else if(associated(dataset%i8_1d))then
       out = dataset%r8_1d
    else
       stop 'Dataset not allocated in get_data()!'
    endif

    return
  end subroutine get_data_integer4_1d

  subroutine get_data_integer4_2d(dataset, out)

    implicit none
    type (dataset_type), intent(in) :: dataset
    integer(kind=int4byte), dimension(:,:) :: out

    if(associated(dataset%i4_2d))then
       out = dataset%i4_2d
    else if(associated(dataset%i8_2d))then
       out = dataset%i8_2d
    else
       stop 'Dataset not allocated in get_data()!'
    endif

    return
  end subroutine get_data_integer4_2d

  subroutine get_data_integer8_1d(dataset, out)

    implicit none
    type (dataset_type), intent(in) :: dataset
    integer(kind=int8byte), dimension(:) :: out

    if(associated(dataset%i4_1d))then
       out = dataset%i4_1d
    else if(associated(dataset%i8_1d))then
       out = dataset%i8_1d
    else
       stop 'Dataset not allocated in get_data()!'
    endif

    return
  end subroutine get_data_integer8_1d

  subroutine get_data_integer8_2d(dataset, out)

    implicit none
    type (dataset_type), intent(in) :: dataset
    integer(kind=int8byte), dimension(:,:) :: out

    if(associated(dataset%i4_2d))then
       out = dataset%i4_2d
    else if(associated(dataset%i8_2d))then
       out = dataset%i8_2d
    else
       stop 'Dataset not allocated in get_data()!'
    endif

    return
  end subroutine get_data_integer8_2d

  subroutine byteswap_dataset(dataset)

    implicit none
    type (dataset_type) :: dataset

    if(associated(dataset%i4_1d))call byteswap(dataset%i4_1d)
    if(associated(dataset%i4_2d))call byteswap(dataset%i4_2d)
    if(associated(dataset%i8_1d))call byteswap(dataset%i8_1d)
    if(associated(dataset%i8_2d))call byteswap(dataset%i8_2d)
    if(associated(dataset%r4_1d))call byteswap(dataset%r4_1d)
    if(associated(dataset%r4_2d))call byteswap(dataset%r4_2d)
    if(associated(dataset%r8_1d))call byteswap(dataset%r8_1d)
    if(associated(dataset%r8_2d))call byteswap(dataset%r8_2d)

    return
  end subroutine byteswap_dataset
  
end module read_gadget_definitions
