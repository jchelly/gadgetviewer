module byteswapper

  use f90_util
  use c_types

  implicit none
  private
  save
  
  public :: byteswap

  interface byteswap
     module procedure byteswap_real4
     module procedure byteswap_real4_1d
     module procedure byteswap_real4_2d
     module procedure byteswap_real8
     module procedure byteswap_real8_1d
     module procedure byteswap_real8_2d
     module procedure byteswap_integer4
     module procedure byteswap_integer4_1d
     module procedure byteswap_integer4_2d
     module procedure byteswap_integer8
     module procedure byteswap_integer8_1d
     module procedure byteswap_integer8_2d
  end interface

contains

!
! 4 byte real data
!
  subroutine byteswap_real4(data)

    implicit none
    real(kind=real4byte), intent(inout) :: data

    call swapendian(data, 1_C_INT, 4_C_INT)

    return
  end subroutine byteswap_real4
  

  subroutine byteswap_real4_1d(data)

    implicit none
    real(kind=real4byte), intent(inout), dimension(:) :: data

    call swapendian(data, int(size(data),kind=C_INT), 4_C_INT)
    
    return
  end subroutine byteswap_real4_1d


  subroutine byteswap_real4_2d(data)

    implicit none
    real(kind=real4byte), intent(inout), dimension(:,:) :: data

    call swapendian(data, int(size(data),kind=C_INT), 4_C_INT)

    return
  end subroutine byteswap_real4_2d


!
! 8 byte real data
!
  subroutine byteswap_real8(data)

    implicit none
    real(kind=real8byte), intent(inout) :: data

    call swapendian(data, 1_C_INT, 8_C_INT)

    return
  end subroutine byteswap_real8
  

  subroutine byteswap_real8_1d(data)

    implicit none
    real(kind=real8byte), intent(inout), dimension(:) :: data

    call swapendian(data, int(size(data),kind=C_INT), 8_C_INT)
    
    return
  end subroutine byteswap_real8_1d


  subroutine byteswap_real8_2d(data)

    implicit none
    real(kind=real8byte), intent(inout), dimension(:,:) :: data

    call swapendian(data, int(size(data),kind=C_INT), 8_C_INT)

    return
  end subroutine byteswap_real8_2d


!
! 4 byte integer data
!
  subroutine byteswap_integer4(data)

    implicit none
    integer(kind=int4byte), intent(inout) :: data

    call swapendian(data, 1_C_INT, 4_C_INT)

    return
  end subroutine byteswap_integer4
  

  subroutine byteswap_integer4_1d(data)

    implicit none
    integer(kind=int4byte), intent(inout), dimension(:) :: data

    call swapendian(data, int(size(data),kind=C_INT), 4_C_INT)
    
    return
  end subroutine byteswap_integer4_1d


  subroutine byteswap_integer4_2d(data)

    implicit none
    integer(kind=int4byte), intent(inout), dimension(:,:) :: data

    call swapendian(data, int(size(data),kind=C_INT), 4_C_INT)

    return
  end subroutine byteswap_integer4_2d


!
! 8 byte integer data
!
  subroutine byteswap_integer8(data)

    implicit none
    integer(kind=int8byte), intent(inout) :: data

    call swapendian(data, 1_C_INT, 8_C_INT)

    return
  end subroutine byteswap_integer8
  

  subroutine byteswap_integer8_1d(data)

    implicit none
    integer(kind=int8byte), intent(inout), dimension(:) :: data

    call swapendian(data, int(size(data),kind=C_INT), 8_C_INT)
    
    return
  end subroutine byteswap_integer8_1d


  subroutine byteswap_integer8_2d(data)

    implicit none
    integer(kind=int8byte), intent(inout), dimension(:,:) :: data

    call swapendian(data, int(size(data),kind=C_INT), 8_C_INT)

    return
  end subroutine byteswap_integer8_2d

end module byteswapper
