module array_shrinker

  use data_types

  implicit none
  private
  save

  public :: shrink_array
  interface shrink_array
     module procedure shrink_array_real4
     module procedure shrink_array_real8
     module procedure shrink_array_integer4
     module procedure shrink_array_integer8
  end interface

contains

  function shrink_array_real4(arr, mask) result(n)
    
    implicit none
    real(kind=real4byte), dimension(:), intent(inout) :: arr
    logical,              dimension(:), intent(in)    :: mask
    integer(kind=index_kind) :: i
    integer(kind=index_kind) :: n

    n = 0
    do i = 1, size(arr), 1
       if(mask(i))then
          n = n + 1
          arr(n) = arr(i)
       endif
    end do

    return
  end function shrink_array_real4

  function shrink_array_real8(arr, mask) result(n)
    
    implicit none
    real(kind=real8byte), dimension(:), intent(inout) :: arr
    logical,              dimension(:), intent(in)    :: mask
    integer(kind=index_kind) :: i
    integer(kind=index_kind) :: n

    n = 0
    do i = 1, size(arr), 1
       if(mask(i))then
          n = n + 1
          arr(n) = arr(i)
       endif
    end do

    return
  end function shrink_array_real8

  function shrink_array_integer4(arr, mask) result(n)
    
    implicit none
    integer(kind=int4byte), dimension(:), intent(inout) :: arr
    logical,                dimension(:), intent(in)    :: mask
    integer(kind=index_kind) :: i
    integer(kind=index_kind) :: n

    n = 0
    do i = 1, size(arr), 1
       if(mask(i))then
          n = n + 1
          arr(n) = arr(i)
       endif
    end do

    return
  end function shrink_array_integer4

  function shrink_array_integer8(arr, mask) result(n)
    
    implicit none
    integer(kind=int8byte), dimension(:), intent(inout) :: arr
    logical,                dimension(:), intent(in)    :: mask
    integer(kind=index_kind) :: i
    integer(kind=index_kind) :: n

    n = 0
    do i = 1, size(arr), 1
       if(mask(i))then
          n = n + 1
          arr(n) = arr(i)
       endif
    end do

    return
  end function shrink_array_integer8

end module array_shrinker
