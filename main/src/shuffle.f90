module random_shuffle
!
! Generate an array of indexes to access another array in
! random order
!
  use data_types

  implicit none
  private

  interface shuffle
     module procedure shuffle_int4
     module procedure shuffle_int8
  end interface
  public :: shuffle

contains

  subroutine shuffle_int4(idx)

    implicit none
    integer(kind=int4byte), dimension(:) :: idx
    integer                     :: n, m
    integer(kind=int4byte), dimension(size(idx)) :: ind
    integer :: i, j
    real    :: rnd

    n = size(idx)
    do i = 1, n, 1
       ind(i) = i
    end do

    m = n
    do i = 1, n, 1
       call random_number(rnd)
       j = int(rnd*m)+1
       idx(i) = ind(j)
       ind(j) = ind(m)
       m = m - 1
    end do

    return
  end subroutine shuffle_int4

  subroutine shuffle_int8(idx)

    implicit none
    integer(kind=int8byte), dimension(:) :: idx
    integer                     :: n, m
    integer(kind=int8byte), dimension(size(idx)) :: ind
    integer :: i, j
    real    :: rnd

    n = size(idx)
    do i = 1, n, 1
       ind(i) = i
    end do

    m = n
    do i = 1, n, 1
       call random_number(rnd)
       j = int(rnd*m)+1
       idx(i) = ind(j)
       ind(j) = ind(m)
       m = m - 1
    end do

    return
  end subroutine shuffle_int8

end module random_shuffle
