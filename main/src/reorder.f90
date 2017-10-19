module reorder_array

  use data_types

  implicit none

  interface reorder
     module procedure reorder_real4
     module procedure reorder_real8
     module procedure reorder_integer4
     module procedure reorder_integer8
  end interface

contains

  subroutine reorder_real4(n, arr, idx)

    implicit none

    integer :: n
    real(kind=real4byte), dimension(:) :: arr
    integer,              dimension(:) :: idx
    real(kind=real4byte), dimension(n) :: tmp
    integer :: i
    
    do i = 1, n, 1
       tmp(i) = arr(idx(i))
    end do
    
    arr(1:n) = tmp(1:n)

    return
  end subroutine reorder_real4

  subroutine reorder_real8(n, arr, idx)

    implicit none

    integer :: n
    real(kind=real8byte), dimension(:) :: arr
    integer,              dimension(:) :: idx
    real(kind=real8byte), dimension(n) :: tmp
    integer :: i
    
    do i = 1, n, 1
       tmp(i) = arr(idx(i))
    end do
    
    arr(1:n) = tmp(1:n)

    return
  end subroutine reorder_real8


  subroutine reorder_integer4(n, arr, idx)

    implicit none

    integer :: n
    integer(kind=int4byte), dimension(:) :: arr
    integer,                dimension(:) :: idx
    integer(kind=int4byte), dimension(n) :: tmp
    integer :: i
    
    do i = 1, n, 1
       tmp(i) = arr(idx(i))
    end do
    
    arr(1:n) = tmp(1:n)

    return
  end subroutine reorder_integer4


  subroutine reorder_integer8(n, arr, idx)

    implicit none

    integer :: n
    integer(kind=int8byte), dimension(:) :: arr
    integer,                dimension(:) :: idx
    integer(kind=int8byte), dimension(n) :: tmp
    integer :: i
    
    do i = 1, n, 1
       tmp(i) = arr(idx(i))
    end do
    
    arr(1:n) = tmp(1:n)

    return
  end subroutine reorder_integer8


end module reorder_array
