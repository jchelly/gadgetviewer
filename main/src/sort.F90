module sort
!
! Module to sort arrays of reals, integers or strings
!
! Uses heap sort for large arrays and shell sort for small arrays.
!
  use data_types
  use f90_util

  implicit none
  private

  ! Callable routines in this module
  public :: sort_in_place
  public :: sort_index
  public :: openmp_sort_index

  ! Array size below which we use a shell sort
  integer, parameter :: nswitch = 100

  ! Subroutine to sort an array
  interface sort_in_place
     module procedure sort_in_place_real4
     module procedure sort_in_place_real8
     module procedure sort_in_place_integer4
     module procedure sort_in_place_integer8
     module procedure sort_in_place_string
  end interface

  ! Subroutine to produce an index to access elements in sorted order
  interface sort_index
     module procedure sort_index_real4
     module procedure sort_index_real8
     module procedure sort_index_integer4
     module procedure sort_index_integer8
     module procedure sort_index_string
     module procedure sort_index_real4_idx8
     module procedure sort_index_real8_idx8
     module procedure sort_index_integer4_idx8
     module procedure sort_index_integer8_idx8
     module procedure sort_index_string_idx8
  end interface

  ! OpenMP parallel sort index
  interface openmp_sort_index
     module procedure openmp_sort_index_real4
     module procedure openmp_sort_index_real8
     module procedure openmp_sort_index_integer4
     module procedure openmp_sort_index_integer8
     module procedure openmp_sort_index_string
     module procedure openmp_sort_index_real4_idx8
     module procedure openmp_sort_index_real8_idx8
     module procedure openmp_sort_index_integer4_idx8
     module procedure openmp_sort_index_integer8_idx8
     module procedure openmp_sort_index_string_idx8
  end interface

contains

  subroutine sort_in_place_real4(arr)
!
! Perform an in place heap sort on a real array
!
    implicit none
    ! Use zero based array index in this routine
    real(kind=real4byte), dimension(0:), intent(inout) :: arr
    ! Internal variables
    integer(kind=int8byte) :: start, finish
    integer(kind=int8byte) :: root, child
    integer(kind=int8byte) :: n
    integer(kind=int8byte) :: i, j, inc
    ! Temporary variable for swapping values
    ! This needs to be of the same type as the data array.
    real(kind=real4byte) :: tmp

    n = size(arr)
    if(n.lt.2)return

    ! If there aren't many elements, use a shell sort
    if(n.lt.nswitch)then
       inc = n / 2
       do while(inc.gt.0)
          do i = inc, n-1, 1
             j = i
             tmp = arr(i)
             do while(j.ge.inc)
                if(arr(j-inc).le.tmp)exit
                arr(j) = arr(j - inc)
                j = j - inc
             end do
             arr(j) = tmp;
          end do
          if(inc.eq.2)then
             inc = 1
          else 
             inc = floor(inc/2.2)
          end if
       end do
       return
    endif

    ! Rearrange the data into a heap structure
    start = (n-1)/2
    do while(start.ge.0)
       finish = n - 1
       root   = start
       do while(2*root+1.le.finish) 
          child = root * 2 + 1
          if(child.lt.finish)then
             if(arr(child).lt.arr(child+1))child = child + 1
          endif
          if(arr(root).lt.arr(child)) then
             tmp        = arr(root)
             arr(root)  = arr(child)
             arr(child) = tmp
             root = child
          else
             exit
          endif
       end do
       start = start - 1
    end do

    ! Remove elements from the heap in order
    finish = n - 1
    do while(finish.gt.0)
       tmp         = arr(0)
       arr(0)      = arr(finish)
       arr(finish) = tmp
       finish = finish - 1
       root = 0
       do while(2*root+1.le.finish) 
          child = root * 2 + 1
          if(child.lt.finish)then
             if(arr(child).lt.arr(child+1))child = child + 1
          endif
          if(arr(root).lt.arr(child)) then
             tmp        = arr(root)
             arr(root)  = arr(child)
             arr(child) = tmp
             root = child
          else
             exit
          endif
       end do
    end do

    return
  end subroutine sort_in_place_real4

  subroutine sort_in_place_real8(arr)
!
! Perform an in place heap sort on a real array
!
    implicit none
    ! Use zero based array index in this routine
    real(kind=real8byte), dimension(0:), intent(inout) :: arr
    ! Internal variables
    integer(kind=int8byte) :: start, finish
    integer(kind=int8byte) :: root, child
    integer(kind=int8byte) :: n
    integer(kind=int8byte) :: i, j, inc
    ! Temporary variable for swapping values
    ! This needs to be of the same type as the data array.
    real(kind=real8byte) :: tmp

    n = size(arr)
    if(n.lt.2)return

    ! If there aren't many elements, use a shell sort
    if(n.lt.nswitch)then
       inc = n / 2
       do while(inc.gt.0)
          do i = inc, n-1, 1
             j = i
             tmp = arr(i)
             do while(j.ge.inc)
                if(arr(j-inc).le.tmp)exit
                arr(j) = arr(j - inc)
                j = j - inc
             end do
             arr(j) = tmp;
          end do
          if(inc.eq.2)then
             inc = 1
          else 
             inc = floor(inc/2.2)
          end if
       end do
       return
    endif

    ! Rearrange the data into a heap structure
    start = (n-1)/2
    do while(start.ge.0)
       finish = n - 1
       root   = start
       do while(2*root+1.le.finish) 
          child = root * 2 + 1
          if(child.lt.finish)then
             if(arr(child).lt.arr(child+1))child = child + 1
          endif
          if(arr(root).lt.arr(child)) then
             tmp        = arr(root)
             arr(root)  = arr(child)
             arr(child) = tmp
             root = child
          else
             exit
          endif
       end do
       start = start - 1
    end do

    ! Remove elements from the heap in order
    finish = n - 1
    do while(finish.gt.0)
       tmp         = arr(0)
       arr(0)      = arr(finish)
       arr(finish) = tmp
       finish = finish - 1
       root = 0
       do while(2*root+1.le.finish) 
          child = root * 2 + 1
          if(child.lt.finish)then
             if(arr(child).lt.arr(child+1))child = child + 1
          endif
          if(arr(root).lt.arr(child)) then
             tmp        = arr(root)
             arr(root)  = arr(child)
             arr(child) = tmp
             root = child
          else
             exit
          endif
       end do
    end do

    return
  end subroutine sort_in_place_real8

  subroutine sort_in_place_integer4(arr)
!
! Perform an in place heap sort on an integer array
!
    implicit none
    ! Use zero based array index in this routine
    integer(kind=int4byte), dimension(0:), intent(inout) :: arr
    ! Internal variables
    integer(kind=int8byte) :: start, finish
    integer(kind=int8byte) :: root, child
    integer(kind=int8byte) :: n
    integer(kind=int8byte) :: i, j, inc
    ! Temporary variable for swapping values
    ! This needs to be of the same type as the data array.
    integer(kind=int4byte) :: tmp

    n = size(arr)
    if(n.lt.2)return

    ! If there aren't many elements, use a shell sort
    if(n.lt.nswitch)then
       inc = n / 2
       do while(inc.gt.0)
          do i = inc, n-1, 1
             j = i
             tmp = arr(i)
             do while (j.ge.inc)
                if(arr(j-inc).le.tmp)exit
                arr(j) = arr(j - inc)
                j = j - inc
             end do
             arr(j) = tmp;
          end do
          if(inc.eq.2)then
             inc = 1
          else 
             inc = floor(inc/2.2)
          end if
       end do
       return
    endif

    ! Rearrange the data into a heap structure
    start = (n-1)/2
    do while(start.ge.0)
       finish = n - 1
       root   = start
       do while(2*root+1.le.finish) 
          child = root * 2 + 1
          if(child.lt.finish)then
             if(arr(child).lt.arr(child+1))child = child + 1
          endif
          if(arr(root).lt.arr(child)) then
             tmp        = arr(root)
             arr(root)  = arr(child)
             arr(child) = tmp
             root = child
          else
             exit
          endif
       end do
       start = start - 1
    end do

    ! Remove elements from the heap in order
    finish = n - 1
    do while(finish.gt.0)
       tmp         = arr(0)
       arr(0)      = arr(finish)
       arr(finish) = tmp
       finish = finish - 1
       root = 0
       do while(2*root+1.le.finish) 
          child = root * 2 + 1
          if(child.lt.finish)then
             if(arr(child).lt.arr(child+1))child = child + 1
          endif
          if(arr(root).lt.arr(child)) then
             tmp        = arr(root)
             arr(root)  = arr(child)
             arr(child) = tmp
             root = child
          else
             exit
          endif
       end do
    end do

    return
  end subroutine sort_in_place_integer4

  subroutine sort_in_place_integer8(arr)
!
! Perform an in place heap sort on an integer array
!
    implicit none
    ! Use zero based array index in this routine
    integer(kind=int8byte), dimension(0:), intent(inout) :: arr
    ! Internal variables
    integer(kind=int8byte) :: start, finish
    integer(kind=int8byte) :: root, child
    integer(kind=int8byte) :: n
    integer(kind=int8byte) :: i, j, inc
    ! Temporary variable for swapping values
    ! This needs to be of the same type as the data array.
    integer(kind=int8byte) :: tmp

    n = size(arr)
    if(n.lt.2)return

    ! If there aren't many elements, use a shell sort
    if(n.lt.nswitch)then
       inc = n / 2
       do while(inc.gt.0)
          do i = inc, n-1, 1
             j = i
             tmp = arr(i)
             do while (j.ge.inc)
                if(arr(j-inc).le.tmp)exit
                arr(j) = arr(j - inc)
                j = j - inc
             end do
             arr(j) = tmp;
          end do
          if(inc.eq.2)then
             inc = 1
          else 
             inc = floor(inc/2.2)
          end if
       end do
       return
    endif

    ! Rearrange the data into a heap structure
    start = (n-1)/2
    do while(start.ge.0)
       finish = n - 1
       root   = start
       do while(2*root+1.le.finish) 
          child = root * 2 + 1
          if(child.lt.finish)then
             if(arr(child).lt.arr(child+1))child = child + 1
          endif
          if(arr(root).lt.arr(child)) then
             tmp        = arr(root)
             arr(root)  = arr(child)
             arr(child) = tmp
             root = child
          else
             exit
          endif
       end do
       start = start - 1
    end do

    ! Remove elements from the heap in order
    finish = n - 1
    do while(finish.gt.0)
       tmp         = arr(0)
       arr(0)      = arr(finish)
       arr(finish) = tmp
       finish = finish - 1
       root = 0
       do while(2*root+1.le.finish) 
          child = root * 2 + 1
          if(child.lt.finish)then
             if(arr(child).lt.arr(child+1))child = child + 1
          endif
          if(arr(root).lt.arr(child)) then
             tmp        = arr(root)
             arr(root)  = arr(child)
             arr(child) = tmp
             root = child
          else
             exit
          endif
       end do
    end do

    return
  end subroutine sort_in_place_integer8


  subroutine sort_in_place_string(arr)
!
! Perform an in place heap sort on an integer array
!
    implicit none
    ! Use zero based array index in this routine
    character(len=*), dimension(0:), intent(inout) :: arr
    ! Internal variables
    integer(kind=int8byte) :: start, finish
    integer(kind=int8byte) :: root, child
    integer(kind=int8byte) :: n
    integer(kind=int8byte) :: i, j, inc
    ! Temporary variable for swapping values
    ! This needs to be of the same type as the data array.
    character(len=len(arr)) :: tmp

    n = size(arr)
    if(n.lt.2)return

    ! If there aren't many elements, use a shell sort
    if(n.lt.nswitch)then
       inc = n / 2
       do while(inc.gt.0)
          do i = inc, n-1, 1
             j = i
             tmp = arr(i)
             do while (j.ge.inc)
                if(arr(j-inc).le.tmp)exit
                arr(j) = arr(j - inc)
                j = j - inc
             end do
             arr(j) = tmp;
          end do
          if(inc.eq.2)then
             inc = 1
          else 
             inc = floor(inc/2.2)
          end if
       end do
       return
    endif

    ! Rearrange the data into a heap structure
    start = (n-1)/2
    do while(start.ge.0)
       finish = n - 1
       root   = start
       do while(2*root+1.le.finish) 
          child = root * 2 + 1
          if(child.lt.finish)then
             if(arr(child).lt.arr(child+1))child = child + 1
          endif
          if(arr(root).lt.arr(child)) then
             tmp        = arr(root)
             arr(root)  = arr(child)
             arr(child) = tmp
             root = child
          else
             exit
          endif
       end do
       start = start - 1
    end do

    ! Remove elements from the heap in order
    finish = n - 1
    do while(finish.gt.0)
       tmp         = arr(0)
       arr(0)      = arr(finish)
       arr(finish) = tmp
       finish = finish - 1
       root = 0
       do while(2*root+1.le.finish) 
          child = root * 2 + 1
          if(child.lt.finish)then
             if(arr(child).lt.arr(child+1))child = child + 1
          endif
          if(arr(root).lt.arr(child)) then
             tmp        = arr(root)
             arr(root)  = arr(child)
             arr(child) = tmp
             root = child
          else
             exit
          endif
       end do
    end do

    return
  end subroutine sort_in_place_string




  subroutine sort_index_real4(arr,idx)
!
! Generate an index to access array elements in sorted order.
! This is doing an in place sort on the index array, but comparing the
! referenced element in arr rather than the value stored in idx. 
!
! Note: the array indexes returned in idx start at one, but otherwise we use
!       zero based indexes in this routine.
!
    implicit none
    ! Use zero based array index in this routine
    real(kind=real4byte), dimension(0:), intent(inout) :: arr
    integer(kind=int4byte), dimension(0:), intent(out)   :: idx
    ! Internal variables
    integer(kind=int8byte) :: start, finish
    integer(kind=int8byte) :: root, child
    integer(kind=int8byte) :: n
    integer(kind=int8byte) :: i, j, inc
    ! Temporary variable for swapping values
    ! This needs to be of the same type as the index array.
    integer(kind=int4byte) :: tmp

    ! Check array sizes
    n = size(arr)
    if(size(idx).lt.n)call terminate('sort_index(): Index array is too small!')

    ! If we have 0 or 1 elements, no sorting required!
    if(n.eq.0)return
    if(n.eq.1)then
       idx(0) = 1
       return
    endif

    ! Initialise index array
    do i = 0, n-1, 1
       idx(i) = i
    end do

    ! If there aren't many elements, use a shell sort
    if(n.lt.nswitch)then
       inc = n / 2
       do while(inc.gt.0)
          do i = inc, n-1, 1
             j = i
             tmp = idx(i)
             do while (j.ge.inc)
                if(arr(idx(j-inc)).le.arr(tmp))exit
                idx(j) = idx(j - inc)
                j = j - inc
             end do
             idx(j) = tmp;
          end do
          if(inc.eq.2)then
             inc = 1
          else 
             inc = floor(inc/2.2)
          end if
       end do
       idx = idx + 1
       return
    endif

    ! Rearrange the data into a heap structure
    start = (n-1)/2
    do while(start.ge.0)
       finish = n - 1
       root   = start
       do while(2*root+1.le.finish) 
          child = root * 2 + 1
          if(child.lt.finish)then
             if(arr(idx(child)).lt.arr(idx(child+1))) &
                  child = child + 1
          endif
          if(arr(idx(root)).lt.arr(idx(child))) then
             tmp        = idx(root)
             idx(root)  = idx(child)
             idx(child) = tmp
             root = child
          else
             exit
          endif
       end do
       start = start - 1
    end do

    ! Remove elements from the heap in order
    finish = n - 1
    do while(finish.gt.0)
       tmp         = idx(0)
       idx(0)      = idx(finish)
       idx(finish) = tmp
       finish = finish - 1
       root = 0
       do while(2*root+1.le.finish) 
          child = root * 2 + 1
          if(child.lt.finish)then
             if(arr(idx(child)).lt.arr(idx(child+1)))&
                  child = child + 1
          endif
          if(arr(idx(root)).lt.arr(idx(child))) then
             tmp        = idx(root)
             idx(root)  = idx(child)
             idx(child) = tmp
             root = child
          else
             exit
          endif
       end do
    end do

    ! Return one-based indexes
    idx = idx + 1

    return
  end subroutine sort_index_real4

  subroutine sort_index_real8(arr,idx)
!
! Generate an index to access array elements in sorted order.
! This is doing an in place sort on the index array, but comparing the
! referenced element in arr rather than the value stored in idx. 
!
! Note: the array indexes returned in idx start at one, but otherwise we use
!       zero based indexes in this routine.
!
    implicit none
    ! Use zero based array index in this routine
    real(kind=real8byte), dimension(0:), intent(inout) :: arr
    integer(kind=int4byte), dimension(0:), intent(out)   :: idx
    ! Internal variables
    integer(kind=int8byte) :: start, finish
    integer(kind=int8byte) :: root, child
    integer(kind=int8byte) :: n
    integer(kind=int8byte) :: i, j, inc
    ! Temporary variable for swapping values
    ! This needs to be of the same type as the index array.
    integer(kind=int4byte) :: tmp

    ! Check array sizes
    n = size(arr)
    if(size(idx).lt.n)call terminate('sort_index(): Index array is too small!')

    ! If we have 0 or 1 elements, no sorting required!
    if(n.eq.0)return
    if(n.eq.1)then
       idx(0) = 1
       return
    endif

    ! Initialise index array
    do i = 0, n-1, 1
       idx(i) = i
    end do

    ! If there aren't many elements, use a shell sort
    if(n.lt.nswitch)then
       inc = n / 2
       do while(inc.gt.0)
          do i = inc, n-1, 1
             j = i
             tmp = idx(i)
             do while(j.ge.inc)
                if(arr(idx(j-inc)).le.arr(tmp))exit
                idx(j) = idx(j - inc)
                j = j - inc
             end do
             idx(j) = tmp;
          end do
          if(inc.eq.2)then
             inc = 1
          else 
             inc = floor(inc/2.2)
          end if
       end do
       idx = idx + 1
       return
    endif

    ! Rearrange the data into a heap structure
    start = (n-1)/2
    do while(start.ge.0)
       finish = n - 1
       root   = start
       do while(2*root+1.le.finish) 
          child = root * 2 + 1
          if(child.lt.finish)then
             if(arr(idx(child)).lt.arr(idx(child+1))) &
                  child = child + 1
          endif
          if(arr(idx(root)).lt.arr(idx(child))) then
             tmp        = idx(root)
             idx(root)  = idx(child)
             idx(child) = tmp
             root = child
          else
             exit
          endif
       end do
       start = start - 1
    end do

    ! Remove elements from the heap in order
    finish = n - 1
    do while(finish.gt.0)
       tmp         = idx(0)
       idx(0)      = idx(finish)
       idx(finish) = tmp
       finish = finish - 1
       root = 0
       do while(2*root+1.le.finish) 
          child = root * 2 + 1
          if(child.lt.finish)then
             if(arr(idx(child)).lt.arr(idx(child+1)))&
                  child = child + 1
          endif
          if(arr(idx(root)).lt.arr(idx(child))) then
             tmp        = idx(root)
             idx(root)  = idx(child)
             idx(child) = tmp
             root = child
          else
             exit
          endif
       end do
    end do

    ! Return one-based indexes
    idx = idx + 1

    return
  end subroutine sort_index_real8

  subroutine sort_index_integer4(arr,idx)
!
! Generate an index to access array elements in sorted order.
! This is doing an in place sort on the index array, but comparing the
! referenced element in arr rather than the value stored in idx. 
!
! Note: the array indexes returned in idx start at one, but otherwise we use
!       zero based indexes in this routine.
!
    implicit none
    ! Use zero based array index in this routine
    integer(kind=int4byte), dimension(0:), intent(inout) :: arr
    integer(kind=int4byte), dimension(0:), intent(out)   :: idx
    ! Internal variables
    integer(kind=int8byte) :: start, finish
    integer(kind=int8byte) :: root, child
    integer(kind=int8byte) :: n
    integer(kind=int8byte) :: i, j, inc
    ! Temporary variable for swapping values
    ! This needs to be of the same type as the index array.
    integer(kind=int4byte) :: tmp

    ! Check array sizes
    n = size(arr)
    if(size(idx).lt.n)call terminate('sort_index(): Index array is too small!')

    ! If we have 0 or 1 elements, no sorting required!
    if(n.eq.0)return
    if(n.eq.1)then
       idx(0) = 1
       return
    endif

    ! Initialise index array
    do i = 0, n-1, 1
       idx(i) = i
    end do

    ! If there aren't many elements, use a shell sort
    if(n.lt.nswitch)then
       inc = n / 2
       do while(inc.gt.0)
          do i = inc, n-1, 1
             j = i
             tmp = idx(i)
             do while (j.ge.inc)
                if(arr(idx(j-inc)).le.arr(tmp))exit
                idx(j) = idx(j - inc)
                j = j - inc
             end do
             idx(j) = tmp;
          end do
          if(inc.eq.2)then
             inc = 1
          else 
             inc = floor(inc/2.2)
          end if
       end do
       idx = idx + 1
       return
    endif

    ! Rearrange the data into a heap structure
    start = (n-1)/2
    do while(start.ge.0)
       finish = n - 1
       root   = start
       do while(2*root+1.le.finish) 
          child = root * 2 + 1
          if(child.lt.finish)then
             if(arr(idx(child)).lt.arr(idx(child+1))) &
                  child = child + 1
          endif
          if(arr(idx(root)).lt.arr(idx(child))) then
             tmp        = idx(root)
             idx(root)  = idx(child)
             idx(child) = tmp
             root = child
          else
             exit
          endif
       end do
       start = start - 1
    end do

    ! Remove elements from the heap in order
    finish = n - 1
    do while(finish.gt.0)
       tmp         = idx(0)
       idx(0)      = idx(finish)
       idx(finish) = tmp
       finish = finish - 1
       root = 0
       do while(2*root+1.le.finish) 
          child = root * 2 + 1
          if(child.lt.finish)then
             if(arr(idx(child)).lt.arr(idx(child+1)))&
                  child = child + 1
          endif
          if(arr(idx(root)).lt.arr(idx(child))) then
             tmp        = idx(root)
             idx(root)  = idx(child)
             idx(child) = tmp
             root = child
          else
             exit
          endif
       end do
    end do

    ! Return one-based indexes
    idx = idx + 1

    return
  end subroutine sort_index_integer4

  subroutine sort_index_integer8(arr,idx)
!
! Generate an index to access array elements in sorted order.
! This is doing an in place sort on the index array, but comparing the
! referenced element in arr rather than the value stored in idx. 
!
! Note: the array indexes returned in idx start at one, but otherwise we use
!       zero based indexes in this routine.
!
    implicit none
    ! Use zero based array index in this routine
    integer(kind=int8byte), dimension(0:), intent(inout) :: arr
    integer(kind=int4byte), dimension(0:), intent(out)   :: idx
    ! Internal variables
    integer(kind=int8byte) :: start, finish
    integer(kind=int8byte) :: root, child
    integer(kind=int8byte) :: n
    integer(kind=int8byte) :: i, j, inc
    ! Temporary variable for swapping values
    ! This needs to be of the same type as the index array.
    integer(kind=int4byte) :: tmp

    ! Check array sizes
    n = size(arr)
    if(size(idx).lt.n)call terminate('sort_index(): Index array is too small!')

    ! If we have 0 or 1 elements, no sorting required!
    if(n.eq.0)return
    if(n.eq.1)then
       idx(0) = 1
       return
    endif

    ! Initialise index array
    do i = 0, n-1, 1
       idx(i) = i
    end do

    ! If there aren't many elements, use a shell sort
    if(n.lt.nswitch)then
       inc = n / 2
       do while(inc.gt.0)
          do i = inc, n-1, 1
             j = i
             tmp = idx(i)
             do while(j.ge.inc)
                if(arr(idx(j-inc)).le.arr(tmp))exit
                idx(j) = idx(j - inc)
                j = j - inc
             end do
             idx(j) = tmp;
          end do
          if(inc.eq.2)then
             inc = 1
          else 
             inc = floor(inc/2.2)
          end if
       end do
       idx = idx + 1
       return
    endif

    ! Rearrange the data into a heap structure
    start = (n-1)/2
    do while(start.ge.0)
       finish = n - 1
       root   = start
       do while(2*root+1.le.finish) 
          child = root * 2 + 1
          if(child.lt.finish)then
             if(arr(idx(child)).lt.arr(idx(child+1))) &
                  child = child + 1
          endif
          if(arr(idx(root)).lt.arr(idx(child))) then
             tmp        = idx(root)
             idx(root)  = idx(child)
             idx(child) = tmp
             root = child
          else
             exit
          endif
       end do
       start = start - 1
    end do

    ! Remove elements from the heap in order
    finish = n - 1
    do while(finish.gt.0)
       tmp         = idx(0)
       idx(0)      = idx(finish)
       idx(finish) = tmp
       finish = finish - 1
       root = 0
       do while(2*root+1.le.finish) 
          child = root * 2 + 1
          if(child.lt.finish)then
             if(arr(idx(child)).lt.arr(idx(child+1)))&
                  child = child + 1
          endif
          if(arr(idx(root)).lt.arr(idx(child))) then
             tmp        = idx(root)
             idx(root)  = idx(child)
             idx(child) = tmp
             root = child
          else
             exit
          endif
       end do
    end do

    ! Return one-based indexes
    idx = idx + 1

    return
  end subroutine sort_index_integer8
  




  subroutine sort_index_string(arr,idx)
!
! Generate an index to access array elements in sorted order.
! This is doing an in place sort on the index array, but comparing the
! referenced element in arr rather than the value stored in idx. 
!
! Note: the array indexes returned in idx start at one, but otherwise we use
!       zero based indexes in this routine.
!
    implicit none
    ! Use zero based array index in this routine
    character(len=*),       dimension(0:), intent(inout) :: arr
    integer(kind=int4byte), dimension(0:), intent(out)   :: idx
    ! Internal variables
    integer(kind=int8byte) :: start, finish
    integer(kind=int8byte) :: root, child
    integer(kind=int8byte) :: n
    integer(kind=int8byte) :: i, j, inc
    ! Temporary variable for swapping values
    ! This needs to be of the same type as the index array.
    integer(kind=int4byte) :: tmp

    ! Check array sizes
    n = size(arr)
    if(size(idx).lt.n)call terminate('sort_index(): Index array is too small!')

    ! If we have 0 or 1 elements, no sorting required!
    if(n.eq.0)return
    if(n.eq.1)then
       idx(0) = 1
       return
    endif

    ! Initialise index array
    do i = 0, n-1, 1
       idx(i) = i
    end do

    ! If there aren't many elements, use a shell sort
    if(n.lt.nswitch)then
       inc = n / 2
       do while(inc.gt.0)
          do i = inc, n-1, 1
             j = i
             tmp = idx(i)
             do while(j.ge.inc)
                if(arr(idx(j-inc)).le.arr(tmp))exit
                idx(j) = idx(j - inc)
                j = j - inc
             end do
             idx(j) = tmp;
          end do
          if(inc.eq.2)then
             inc = 1
          else 
             inc = floor(inc/2.2)
          end if
       end do
       idx = idx + 1
       return
    endif

    ! Rearrange the data into a heap structure
    start = (n-1)/2
    do while(start.ge.0)
       finish = n - 1
       root   = start
       do while(2*root+1.le.finish) 
          child = root * 2 + 1
          if(child.lt.finish)then
             if(arr(idx(child)).lt.arr(idx(child+1))) &
                  child = child + 1
          endif
          if(arr(idx(root)).lt.arr(idx(child))) then
             tmp        = idx(root)
             idx(root)  = idx(child)
             idx(child) = tmp
             root = child
          else
             exit
          endif
       end do
       start = start - 1
    end do

    ! Remove elements from the heap in order
    finish = n - 1
    do while(finish.gt.0)
       tmp         = idx(0)
       idx(0)      = idx(finish)
       idx(finish) = tmp
       finish = finish - 1
       root = 0
       do while(2*root+1.le.finish) 
          child = root * 2 + 1
          if(child.lt.finish)then
             if(arr(idx(child)).lt.arr(idx(child+1)))&
                  child = child + 1
          endif
          if(arr(idx(root)).lt.arr(idx(child))) then
             tmp        = idx(root)
             idx(root)  = idx(child)
             idx(child) = tmp
             root = child
          else
             exit
          endif
       end do
    end do

    ! Return one-based indexes
    idx = idx + 1

    return
  end subroutine sort_index_string


!
! Sort index for 8 byte indexes
!

  subroutine sort_index_real4_idx8(arr,idx)
!
! Generate an index to access array elements in sorted order.
! This is doing an in place sort on the index array, but comparing the
! referenced element in arr rather than the value stored in idx. 
!
! Note: the array indexes returned in idx start at one, but otherwise we use
!       zero based indexes in this routine.
!
    implicit none
    ! Use zero based array index in this routine
    real(kind=real4byte), dimension(0:), intent(inout) :: arr
    integer(kind=int8byte), dimension(0:), intent(out)   :: idx
    ! Internal variables
    integer(kind=int8byte) :: start, finish
    integer(kind=int8byte) :: root, child
    integer(kind=int8byte) :: n
    integer(kind=int8byte) :: i, j, inc
    ! Temporary variable for swapping values
    ! This needs to be of the same type as the index array.
    integer(kind=int8byte) :: tmp

    ! Check array sizes
    n = size(arr)
    if(size(idx).lt.n)call terminate('sort_index(): Index array is too small!')

    ! If we have 0 or 1 elements, no sorting required!
    if(n.eq.0)return
    if(n.eq.1)then
       idx(0) = 1
       return
    endif

    ! Initialise index array
    do i = 0, n-1, 1
       idx(i) = i
    end do

    ! If there aren't many elements, use a shell sort
    if(n.lt.nswitch)then
       inc = n / 2
       do while(inc.gt.0)
          do i = inc, n-1, 1
             j = i
             tmp = idx(i)
             do while (j.ge.inc)
                if(arr(idx(j-inc)).le.arr(tmp))exit
                idx(j) = idx(j - inc)
                j = j - inc
             end do
             idx(j) = tmp;
          end do
          if(inc.eq.2)then
             inc = 1
          else 
             inc = floor(inc/2.2)
          end if
       end do
       idx = idx + 1
       return
    endif

    ! Rearrange the data into a heap structure
    start = (n-1)/2
    do while(start.ge.0)
       finish = n - 1
       root   = start
       do while(2*root+1.le.finish) 
          child = root * 2 + 1
          if(child.lt.finish)then
             if(arr(idx(child)).lt.arr(idx(child+1))) &
                  child = child + 1
          endif
          if(arr(idx(root)).lt.arr(idx(child))) then
             tmp        = idx(root)
             idx(root)  = idx(child)
             idx(child) = tmp
             root = child
          else
             exit
          endif
       end do
       start = start - 1
    end do

    ! Remove elements from the heap in order
    finish = n - 1
    do while(finish.gt.0)
       tmp         = idx(0)
       idx(0)      = idx(finish)
       idx(finish) = tmp
       finish = finish - 1
       root = 0
       do while(2*root+1.le.finish) 
          child = root * 2 + 1
          if(child.lt.finish)then
             if(arr(idx(child)).lt.arr(idx(child+1)))&
                  child = child + 1
          endif
          if(arr(idx(root)).lt.arr(idx(child))) then
             tmp        = idx(root)
             idx(root)  = idx(child)
             idx(child) = tmp
             root = child
          else
             exit
          endif
       end do
    end do

    ! Return one-based indexes
    idx = idx + 1

    return
  end subroutine sort_index_real4_idx8

  subroutine sort_index_real8_idx8(arr,idx)
!
! Generate an index to access array elements in sorted order.
! This is doing an in place sort on the index array, but comparing the
! referenced element in arr rather than the value stored in idx. 
!
! Note: the array indexes returned in idx start at one, but otherwise we use
!       zero based indexes in this routine.
!
    implicit none
    ! Use zero based array index in this routine
    real(kind=real8byte), dimension(0:), intent(inout) :: arr
    integer(kind=int8byte), dimension(0:), intent(out)   :: idx
    ! Internal variables
    integer(kind=int8byte) :: start, finish
    integer(kind=int8byte) :: root, child
    integer(kind=int8byte) :: n
    integer(kind=int8byte) :: i, j, inc
    ! Temporary variable for swapping values
    ! This needs to be of the same type as the index array.
    integer(kind=int8byte) :: tmp

    ! Check array sizes
    n = size(arr)
    if(size(idx).lt.n)call terminate('sort_index(): Index array is too small!')

    ! If we have 0 or 1 elements, no sorting required!
    if(n.eq.0)return
    if(n.eq.1)then
       idx(0) = 1
       return
    endif

    ! Initialise index array
    do i = 0, n-1, 1
       idx(i) = i
    end do

    ! If there aren't many elements, use a shell sort
    if(n.lt.nswitch)then
       inc = n / 2
       do while(inc.gt.0)
          do i = inc, n-1, 1
             j = i
             tmp = idx(i)
             do while(j.ge.inc)
                if(arr(idx(j-inc)).le.arr(tmp))exit
                idx(j) = idx(j - inc)
                j = j - inc
             end do
             idx(j) = tmp;
          end do
          if(inc.eq.2)then
             inc = 1
          else 
             inc = floor(inc/2.2)
          end if
       end do
       idx = idx + 1
       return
    endif

    ! Rearrange the data into a heap structure
    start = (n-1)/2
    do while(start.ge.0)
       finish = n - 1
       root   = start
       do while(2*root+1.le.finish) 
          child = root * 2 + 1
          if(child.lt.finish)then
             if(arr(idx(child)).lt.arr(idx(child+1))) &
                  child = child + 1
          endif
          if(arr(idx(root)).lt.arr(idx(child))) then
             tmp        = idx(root)
             idx(root)  = idx(child)
             idx(child) = tmp
             root = child
          else
             exit
          endif
       end do
       start = start - 1
    end do

    ! Remove elements from the heap in order
    finish = n - 1
    do while(finish.gt.0)
       tmp         = idx(0)
       idx(0)      = idx(finish)
       idx(finish) = tmp
       finish = finish - 1
       root = 0
       do while(2*root+1.le.finish) 
          child = root * 2 + 1
          if(child.lt.finish)then
             if(arr(idx(child)).lt.arr(idx(child+1)))&
                  child = child + 1
          endif
          if(arr(idx(root)).lt.arr(idx(child))) then
             tmp        = idx(root)
             idx(root)  = idx(child)
             idx(child) = tmp
             root = child
          else
             exit
          endif
       end do
    end do

    ! Return one-based indexes
    idx = idx + 1

    return
  end subroutine sort_index_real8_idx8

  subroutine sort_index_integer4_idx8(arr,idx)
!
! Generate an index to access array elements in sorted order.
! This is doing an in place sort on the index array, but comparing the
! referenced element in arr rather than the value stored in idx. 
!
! Note: the array indexes returned in idx start at one, but otherwise we use
!       zero based indexes in this routine.
!
    implicit none
    ! Use zero based array index in this routine
    integer(kind=int4byte), dimension(0:), intent(inout) :: arr
    integer(kind=int8byte), dimension(0:), intent(out)   :: idx
    ! Internal variables
    integer(kind=int8byte) :: start, finish
    integer(kind=int8byte) :: root, child
    integer(kind=int8byte) :: n
    integer(kind=int8byte) :: i, j, inc
    ! Temporary variable for swapping values
    ! This needs to be of the same type as the index array.
    integer(kind=int8byte) :: tmp

    ! Check array sizes
    n = size(arr)
    if(size(idx).lt.n)call terminate('sort_index(): Index array is too small!')

    ! If we have 0 or 1 elements, no sorting required!
    if(n.eq.0)return
    if(n.eq.1)then
       idx(0) = 1
       return
    endif

    ! Initialise index array
    do i = 0, n-1, 1
       idx(i) = i
    end do

    ! If there aren't many elements, use a shell sort
    if(n.lt.nswitch)then
       inc = n / 2
       do while(inc.gt.0)
          do i = inc, n-1, 1
             j = i
             tmp = idx(i)
             do while (j.ge.inc)
                if(arr(idx(j-inc)).le.arr(tmp))exit
                idx(j) = idx(j - inc)
                j = j - inc
             end do
             idx(j) = tmp;
          end do
          if(inc.eq.2)then
             inc = 1
          else 
             inc = floor(inc/2.2)
          end if
       end do
       idx = idx + 1
       return
    endif

    ! Rearrange the data into a heap structure
    start = (n-1)/2
    do while(start.ge.0)
       finish = n - 1
       root   = start
       do while(2*root+1.le.finish) 
          child = root * 2 + 1
          if(child.lt.finish)then
             if(arr(idx(child)).lt.arr(idx(child+1))) &
                  child = child + 1
          endif
          if(arr(idx(root)).lt.arr(idx(child))) then
             tmp        = idx(root)
             idx(root)  = idx(child)
             idx(child) = tmp
             root = child
          else
             exit
          endif
       end do
       start = start - 1
    end do

    ! Remove elements from the heap in order
    finish = n - 1
    do while(finish.gt.0)
       tmp         = idx(0)
       idx(0)      = idx(finish)
       idx(finish) = tmp
       finish = finish - 1
       root = 0
       do while(2*root+1.le.finish) 
          child = root * 2 + 1
          if(child.lt.finish)then
             if(arr(idx(child)).lt.arr(idx(child+1)))&
                  child = child + 1
          endif
          if(arr(idx(root)).lt.arr(idx(child))) then
             tmp        = idx(root)
             idx(root)  = idx(child)
             idx(child) = tmp
             root = child
          else
             exit
          endif
       end do
    end do

    ! Return one-based indexes
    idx = idx + 1

    return
  end subroutine sort_index_integer4_idx8

  subroutine sort_index_integer8_idx8(arr,idx)
!
! Generate an index to access array elements in sorted order.
! This is doing an in place sort on the index array, but comparing the
! referenced element in arr rather than the value stored in idx. 
!
! Note: the array indexes returned in idx start at one, but otherwise we use
!       zero based indexes in this routine.
!
    implicit none
    ! Use zero based array index in this routine
    integer(kind=int8byte), dimension(0:), intent(inout) :: arr
    integer(kind=int8byte), dimension(0:), intent(out)   :: idx
    ! Internal variables
    integer(kind=int8byte) :: start, finish
    integer(kind=int8byte) :: root, child
    integer(kind=int8byte) :: n
    integer(kind=int8byte) :: i, j, inc
    ! Temporary variable for swapping values
    ! This needs to be of the same type as the index array.
    integer(kind=int8byte) :: tmp

    ! Check array sizes
    n = size(arr)
    if(size(idx).lt.n)call terminate('sort_index(): Index array is too small!')

    ! If we have 0 or 1 elements, no sorting required!
    if(n.eq.0)return
    if(n.eq.1)then
       idx(0) = 1
       return
    endif

    ! Initialise index array
    do i = 0, n-1, 1
       idx(i) = i
    end do

    ! If there aren't many elements, use a shell sort
    if(n.lt.nswitch)then
       inc = n / 2
       do while(inc.gt.0)
          do i = inc, n-1, 1
             j = i
             tmp = idx(i)
             do while(j.ge.inc)
                if(arr(idx(j-inc)).le.arr(tmp))exit
                idx(j) = idx(j - inc)
                j = j - inc
             end do
             idx(j) = tmp;
          end do
          if(inc.eq.2)then
             inc = 1
          else 
             inc = floor(inc/2.2)
          end if
       end do
       idx = idx + 1
       return
    endif

    ! Rearrange the data into a heap structure
    start = (n-1)/2
    do while(start.ge.0)
       finish = n - 1
       root   = start
       do while(2*root+1.le.finish) 
          child = root * 2 + 1
          if(child.lt.finish)then
             if(arr(idx(child)).lt.arr(idx(child+1))) &
                  child = child + 1
          endif
          if(arr(idx(root)).lt.arr(idx(child))) then
             tmp        = idx(root)
             idx(root)  = idx(child)
             idx(child) = tmp
             root = child
          else
             exit
          endif
       end do
       start = start - 1
    end do

    ! Remove elements from the heap in order
    finish = n - 1
    do while(finish.gt.0)
       tmp         = idx(0)
       idx(0)      = idx(finish)
       idx(finish) = tmp
       finish = finish - 1
       root = 0
       do while(2*root+1.le.finish) 
          child = root * 2 + 1
          if(child.lt.finish)then
             if(arr(idx(child)).lt.arr(idx(child+1)))&
                  child = child + 1
          endif
          if(arr(idx(root)).lt.arr(idx(child))) then
             tmp        = idx(root)
             idx(root)  = idx(child)
             idx(child) = tmp
             root = child
          else
             exit
          endif
       end do
    end do

    ! Return one-based indexes
    idx = idx + 1

    return
  end subroutine sort_index_integer8_idx8
  




  subroutine sort_index_string_idx8(arr,idx)
!
! Generate an index to access array elements in sorted order.
! This is doing an in place sort on the index array, but comparing the
! referenced element in arr rather than the value stored in idx. 
!
! Note: the array indexes returned in idx start at one, but otherwise we use
!       zero based indexes in this routine.
!
    implicit none
    ! Use zero based array index in this routine
    character(len=*),       dimension(0:), intent(inout) :: arr
    integer(kind=int8byte), dimension(0:), intent(out)   :: idx
    ! Internal variables
    integer(kind=int8byte) :: start, finish
    integer(kind=int8byte) :: root, child
    integer(kind=int8byte) :: n
    integer(kind=int8byte) :: i, j, inc
    ! Temporary variable for swapping values
    ! This needs to be of the same type as the index array.
    integer(kind=int8byte) :: tmp

    ! Check array sizes
    n = size(arr)
    if(size(idx).lt.n)call terminate('sort_index(): Index array is too small!')

    ! If we have 0 or 1 elements, no sorting required!
    if(n.eq.0)return
    if(n.eq.1)then
       idx(0) = 1
       return
    endif

    ! Initialise index array
    do i = 0, n-1, 1
       idx(i) = i
    end do

    ! If there aren't many elements, use a shell sort
    if(n.lt.nswitch)then
       inc = n / 2
       do while(inc.gt.0)
          do i = inc, n-1, 1
             j = i
             tmp = idx(i)
             do while(j.ge.inc)
                if(arr(idx(j-inc)).le.arr(tmp))exit
                idx(j) = idx(j - inc)
                j = j - inc
             end do
             idx(j) = tmp;
          end do
          if(inc.eq.2)then
             inc = 1
          else 
             inc = floor(inc/2.2)
          end if
       end do
       idx = idx + 1
       return
    endif

    ! Rearrange the data into a heap structure
    start = (n-1)/2
    do while(start.ge.0)
       finish = n - 1
       root   = start
       do while(2*root+1.le.finish) 
          child = root * 2 + 1
          if(child.lt.finish)then
             if(arr(idx(child)).lt.arr(idx(child+1))) &
                  child = child + 1
          endif
          if(arr(idx(root)).lt.arr(idx(child))) then
             tmp        = idx(root)
             idx(root)  = idx(child)
             idx(child) = tmp
             root = child
          else
             exit
          endif
       end do
       start = start - 1
    end do

    ! Remove elements from the heap in order
    finish = n - 1
    do while(finish.gt.0)
       tmp         = idx(0)
       idx(0)      = idx(finish)
       idx(finish) = tmp
       finish = finish - 1
       root = 0
       do while(2*root+1.le.finish) 
          child = root * 2 + 1
          if(child.lt.finish)then
             if(arr(idx(child)).lt.arr(idx(child+1)))&
                  child = child + 1
          endif
          if(arr(idx(root)).lt.arr(idx(child))) then
             tmp        = idx(root)
             idx(root)  = idx(child)
             idx(child) = tmp
             root = child
          else
             exit
          endif
       end do
    end do

    ! Return one-based indexes
    idx = idx + 1

    return
  end subroutine sort_index_string_idx8




  subroutine openmp_sort_index_real4(arr, idx)
!
! Sort index routine parallelised with OpenMP. Falls back to serial
! sort if openmp is not available.
!
    implicit none

    ! Use ONE based array index in this routine
    real(kind=real4byte),   dimension(:), intent(inout) :: arr
    integer(kind=int4byte), dimension(:), intent(out)   :: idx
    ! Use serial sort for small arrays
    integer(kind=int4byte) :: n
    integer, parameter :: nmin = 100000
    ! OpenMP stuff
    integer :: numprocs, myid
#ifdef _OPENMP
    ! Number of threads etc
    integer, external  :: OMP_GET_MAX_THREADS
    integer, external  :: OMP_GET_NUM_PROCS
    integer, parameter :: NTHREADMAX = 1024
    ! Partitioning of array across threads
    integer(kind=int4byte), dimension(0:NTHREADMAX-1) :: first, last, nleft
    integer(kind=int4byte) :: nperproc
    integer(kind=int4byte) :: i, j, k, l
    integer(kind=int4byte), dimension(size(idx)) :: tmp
    ! Stuff for merging array sections
    real(kind=real4byte) :: valmin
    integer(kind=int4byte) :: imin, idxmin
    integer(kind=int4byte) :: np
    ! OpenMP functions
    integer, external :: OMP_GET_NUM_THREADS
    integer, external :: OMP_GET_THREAD_NUM
#endif

    ! Check array sizes
    n = size(arr)
    if(size(idx).lt.n)call terminate('sort_index(): Index array is too small!')

#ifdef _OPENMP
    ! Use serial sort for small arrays
    if(n.lt.nmin)then
       call sort_index(arr, idx)
       return
    endif

!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(myid, numprocs, nperproc)
    
    ! Decide which sections each thread should sort
    myid        = OMP_GET_THREAD_NUM()
    numprocs    = OMP_GET_NUM_THREADS()

    nperproc    = n/numprocs
    first(myid) = nperproc*myid + 1
    if(myid.lt.numprocs-1)then
       last(myid)  = first(myid) + nperproc - 1
    else
       last(myid) = n
    endif

    ! Sort the sections
    call sort_index(arr(first(myid):last(myid)), tmp(first(myid):last(myid)))

    ! Store number of threads used in a shared variable so we can access
    ! it outside the parallel region
    if(myid.eq.0)np = numprocs

!$OMP END PARALLEL

    ! Now have a set of sorted sections. Need to merge these into a single
    ! index array
    nleft = last - first + 1
    do i = 1, n, 1
       valmin = huge(valmin)
       do j = 0, np-1, 1
          if(nleft(j).gt.0)then
             k = last(j) - nleft(j) + 1
             l = tmp(k) + first(j) - 1
             if(arr(l).le.valmin)then
                valmin = arr(l)
                imin   = j
                idxmin = l
             endif
          endif
       end do
       idx(i) = idxmin
       nleft(imin) = nleft(imin) - 1
    end do

#else
    ! No OpenMP support, so just do serial sort
    call sort_index(arr, idx)
#endif

    return
  end subroutine openmp_sort_index_real4


  subroutine openmp_sort_index_real8(arr, idx)
!
! Sort index routine parallelised with OpenMP. Falls back to serial
! sort if openmp is not available.
!
    implicit none

    ! Use ONE based array index in this routine
    real(kind=real8byte),   dimension(:), intent(inout) :: arr
    integer(kind=int4byte), dimension(:), intent(out)   :: idx
    ! Use serial sort for small arrays
    integer(kind=int4byte) :: n
    integer, parameter :: nmin = 100000
    ! OpenMP stuff
    integer :: numprocs, myid
#ifdef _OPENMP
    ! Number of threads etc
    integer, external  :: OMP_GET_MAX_THREADS
    integer, external  :: OMP_GET_NUM_PROCS
    integer, parameter :: NTHREADMAX = 1024
    ! Partitioning of array across threads
    integer(kind=int4byte), dimension(0:NTHREADMAX-1) :: first, last, nleft
    integer(kind=int4byte) :: nperproc
    integer(kind=int4byte) :: i, j, k, l
    integer(kind=int4byte), dimension(size(idx)) :: tmp
    ! Stuff for merging array sections
    real(kind=real8byte) :: valmin
    integer(kind=int4byte) :: imin, idxmin
    integer(kind=int4byte) :: np
    ! OpenMP functions
    integer, external :: OMP_GET_NUM_THREADS
    integer, external :: OMP_GET_THREAD_NUM
#endif

    ! Check array sizes
    n = size(arr)
    if(size(idx).lt.n)call terminate('sort_index(): Index array is too small!')

#ifdef _OPENMP
    ! Use serial sort for small arrays
    if(n.lt.nmin)then
       call sort_index(arr, idx)
       return
    endif

!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(myid, numprocs, nperproc)
    
    ! Decide which sections each thread should sort
    myid        = OMP_GET_THREAD_NUM()
    numprocs    = OMP_GET_NUM_THREADS()

    nperproc    = n/numprocs
    first(myid) = nperproc*myid + 1
    if(myid.lt.numprocs-1)then
       last(myid)  = first(myid) + nperproc - 1
    else
       last(myid) = n
    endif

    ! Sort the sections
    call sort_index(arr(first(myid):last(myid)), tmp(first(myid):last(myid)))

    ! Store number of threads used in a shared variable so we can access
    ! it outside the parallel region
    if(myid.eq.0)np = numprocs

!$OMP END PARALLEL

    ! Now have a set of sorted sections. Need to merge these into a single
    ! index array
    nleft = last - first + 1
    do i = 1, n, 1
       valmin = huge(valmin)
       do j = 0, np-1, 1
          if(nleft(j).gt.0)then
             k = last(j) - nleft(j) + 1
             l = tmp(k) + first(j) - 1
             if(arr(l).le.valmin)then
                valmin = arr(l)
                imin   = j
                idxmin = l
             endif
          endif
       end do
       idx(i) = idxmin
       nleft(imin) = nleft(imin) - 1
    end do

#else
    ! No OpenMP support, so just do serial sort
    call sort_index(arr, idx)
#endif

    return
  end subroutine openmp_sort_index_real8


  subroutine openmp_sort_index_integer4(arr, idx)
!
! Sort index routine parallelised with OpenMP. Falls back to serial
! sort if openmp is not available.
!
    implicit none

    ! Use ONE based array index in this routine
    integer(kind=int4byte), dimension(:), intent(inout) :: arr
    integer(kind=int4byte), dimension(:), intent(out)   :: idx
    ! Use serial sort for small arrays
    integer(kind=int4byte) :: n
    integer, parameter :: nmin = 100000
    ! OpenMP stuff
    integer :: numprocs, myid
#ifdef _OPENMP
    ! Number of threads etc
    integer, external  :: OMP_GET_MAX_THREADS
    integer, external  :: OMP_GET_NUM_PROCS
    integer, parameter :: NTHREADMAX = 1024
    ! Partitioning of array across threads
    integer(kind=int4byte), dimension(0:NTHREADMAX-1) :: first, last, nleft
    integer(kind=int4byte) :: nperproc
    integer(kind=int4byte) :: i, j, k, l
    integer(kind=int4byte), dimension(size(idx)) :: tmp
    ! Stuff for merging array sections
    integer(kind=int4byte) :: valmin
    integer(kind=int4byte)              :: imin, idxmin
    integer(kind=int4byte)              :: np
    ! OpenMP functions
    integer, external :: OMP_GET_NUM_THREADS
    integer, external :: OMP_GET_THREAD_NUM
#endif

    ! Check array sizes
    n = size(arr)
    if(size(idx).lt.n)call terminate('sort_index(): Index array is too small!')

#ifdef _OPENMP
    ! Use serial sort for small arrays
    if(n.lt.nmin)then
       call sort_index(arr, idx)
       return
    endif

!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(myid, numprocs, nperproc)
    
    ! Decide which sections each thread should sort
    myid        = OMP_GET_THREAD_NUM()
    numprocs    = OMP_GET_NUM_THREADS()

    nperproc    = n/numprocs
    first(myid) = nperproc*myid + 1
    if(myid.lt.numprocs-1)then
       last(myid)  = first(myid) + nperproc - 1
    else
       last(myid) = n
    endif

    ! Sort the sections
    call sort_index(arr(first(myid):last(myid)), tmp(first(myid):last(myid)))

    ! Store number of threads used in a shared variable so we can access
    ! it outside the parallel region
    if(myid.eq.0)np = numprocs

!$OMP END PARALLEL

    ! Now have a set of sorted sections. Need to merge these into a single
    ! index array
    nleft = last - first + 1
    do i = 1, n, 1
       valmin = huge(valmin)
       do j = 0, np-1, 1
          if(nleft(j).gt.0)then
             k = last(j) - nleft(j) + 1
             l = tmp(k) + first(j) - 1
             if(arr(l).le.valmin)then
                valmin = arr(l)
                imin   = j
                idxmin = l
             endif
          endif
       end do
       idx(i) = idxmin
       nleft(imin) = nleft(imin) - 1
    end do

#else
    ! No OpenMP support, so just do serial sort
    call sort_index(arr, idx)
#endif

    return
  end subroutine openmp_sort_index_integer4





  subroutine openmp_sort_index_integer8(arr, idx)
!
! Sort index routine parallelised with OpenMP. Falls back to serial
! sort if openmp is not available.
!
    implicit none

    ! Use ONE based array index in this routine
    integer(kind=int8byte), dimension(:), intent(inout) :: arr
    integer(kind=int4byte), dimension(:), intent(out)   :: idx
    ! Use serial sort for small arrays
    integer(kind=int4byte) :: n
    integer, parameter :: nmin = 100000
    ! OpenMP stuff
    integer :: numprocs, myid
#ifdef _OPENMP
    ! Number of threads etc
    integer, external  :: OMP_GET_MAX_THREADS
    integer, external  :: OMP_GET_NUM_PROCS
    integer, parameter :: NTHREADMAX = 1024
    ! Partitioning of array across threads
    integer(kind=int4byte), dimension(0:NTHREADMAX-1) :: first, last, nleft
    integer(kind=int4byte) :: nperproc
    integer(kind=int4byte) :: i, j, k, l
    integer(kind=int4byte), dimension(size(idx)) :: tmp
    ! Stuff for merging array sections
    integer(kind=int8byte) :: valmin
    integer(kind=int4byte)              :: imin, idxmin
    integer(kind=int4byte)              :: np
    ! OpenMP functions
    integer, external :: OMP_GET_NUM_THREADS
    integer, external :: OMP_GET_THREAD_NUM
#endif

    ! Check array sizes
    n = size(arr)
    if(size(idx).lt.n)call terminate('sort_index(): Index array is too small!')

#ifdef _OPENMP
    ! Use serial sort for small arrays
    if(n.lt.nmin)then
       call sort_index(arr, idx)
       return
    endif

!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(myid, numprocs, nperproc)
    
    ! Decide which sections each thread should sort
    myid        = OMP_GET_THREAD_NUM()
    numprocs    = OMP_GET_NUM_THREADS()

    nperproc    = n/numprocs
    first(myid) = nperproc*myid + 1
    if(myid.lt.numprocs-1)then
       last(myid)  = first(myid) + nperproc - 1
    else
       last(myid) = n
    endif

    ! Sort the sections
    call sort_index(arr(first(myid):last(myid)), tmp(first(myid):last(myid)))

    ! Store number of threads used in a shared variable so we can access
    ! it outside the parallel region
    if(myid.eq.0)np = numprocs

!$OMP END PARALLEL

    ! Now have a set of sorted sections. Need to merge these into a single
    ! index array
    nleft = last - first + 1
    do i = 1, n, 1
       valmin = huge(valmin)
       do j = 0, np-1, 1
          if(nleft(j).gt.0)then
             k = last(j) - nleft(j) + 1
             l = tmp(k) + first(j) - 1
             if(arr(l).le.valmin)then
                valmin = arr(l)
                imin   = j
                idxmin = l
             endif
          endif
       end do
       idx(i) = idxmin
       nleft(imin) = nleft(imin) - 1
    end do

#else
    ! No OpenMP support, so just do serial sort
    call sort_index(arr, idx)
#endif

    return
  end subroutine openmp_sort_index_integer8


  subroutine openmp_sort_index_string(arr, idx)
!
! Sort index routine parallelised with OpenMP. Falls back to serial
! sort if openmp is not available.
!
    implicit none

    ! Use ONE based array index in this routine
    character(len=*),       dimension(:), intent(inout) :: arr
    integer(kind=int4byte), dimension(:), intent(out)   :: idx
    ! Use serial sort for small arrays
    integer(kind=int4byte) :: n
    integer, parameter :: nmin = 100000
    ! OpenMP stuff
    integer :: numprocs, myid
#ifdef _OPENMP
    ! Number of threads etc
    integer, external  :: OMP_GET_MAX_THREADS
    integer, external  :: OMP_GET_NUM_PROCS
    integer, parameter :: NTHREADMAX = 1024
    ! Partitioning of array across threads
    integer(kind=int4byte), dimension(0:NTHREADMAX-1) :: first, last, nleft
    integer(kind=int4byte) :: nperproc
    integer(kind=int4byte) :: i, j, k, l
    integer(kind=int4byte), dimension(size(idx)) :: tmp
    ! Stuff for merging array sections
    character(len=len(arr)) :: valmin
    integer(kind=int4byte)              :: imin, idxmin
    integer(kind=int4byte)              :: np
    ! OpenMP functions
    integer, external :: OMP_GET_NUM_THREADS
    integer, external :: OMP_GET_THREAD_NUM
#endif

    ! Check array sizes
    n = size(arr)
    if(size(idx).lt.n)call terminate('sort_index(): Index array is too small!')

#ifdef _OPENMP
    ! Use serial sort for small arrays
    if(n.lt.nmin)then
       call sort_index(arr, idx)
       return
    endif

!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(myid, numprocs, nperproc)
    
    ! Decide which sections each thread should sort
    myid        = OMP_GET_THREAD_NUM()
    numprocs    = OMP_GET_NUM_THREADS()

    nperproc    = n/numprocs
    first(myid) = nperproc*myid + 1
    if(myid.lt.numprocs-1)then
       last(myid)  = first(myid) + nperproc - 1
    else
       last(myid) = n
    endif

    ! Sort the sections
    call sort_index(arr(first(myid):last(myid)), tmp(first(myid):last(myid)))

    ! Store number of threads used in a shared variable so we can access
    ! it outside the parallel region
    if(myid.eq.0)np = numprocs

!$OMP END PARALLEL

    ! Now have a set of sorted sections. Need to merge these into a single
    ! index array
    nleft = last - first + 1
    do i = 1, n, 1
       valmin = ""
       do j = 0, np-1, 1
          if(nleft(j).gt.0)then
             k = last(j) - nleft(j) + 1
             l = tmp(k) + first(j) - 1
             if(arr(l).le.valmin.or.valmin.eq."")then
                valmin = arr(l)
                imin   = j
                idxmin = l
             endif
          endif
       end do
       idx(i) = idxmin
       nleft(imin) = nleft(imin) - 1
    end do

#else
    ! No OpenMP support, so just do serial sort
    call sort_index(arr, idx)
#endif

    return
  end subroutine openmp_sort_index_string


!
! Versions for 8 byte sorting indexes
!

  subroutine openmp_sort_index_real4_idx8(arr, idx)
!
! Sort index routine parallelised with OpenMP. Falls back to serial
! sort if openmp is not available.
!
    implicit none

    ! Use ONE based array index in this routine
    real(kind=real4byte),   dimension(:), intent(inout) :: arr
    integer(kind=int8byte), dimension(:), intent(out)   :: idx
    ! Use serial sort for small arrays
    integer(kind=int8byte) :: n
    integer, parameter :: nmin = 100000
    ! OpenMP stuff
    integer :: numprocs, myid
#ifdef _OPENMP
    ! Number of threads etc
    integer, external  :: OMP_GET_MAX_THREADS
    integer, external  :: OMP_GET_NUM_PROCS
    integer, parameter :: NTHREADMAX = 1024
    ! Partitioning of array across threads
    integer(kind=int8byte), dimension(0:NTHREADMAX-1) :: first, last, nleft
    integer(kind=int8byte) :: nperproc
    integer(kind=int8byte) :: i, j, k, l
    integer(kind=int8byte), dimension(size(idx)) :: tmp
    ! Stuff for merging array sections
    real(kind=real4byte) :: valmin
    integer(kind=int8byte)              :: imin, idxmin
    integer(kind=int8byte)              :: np
    ! OpenMP functions
    integer, external :: OMP_GET_NUM_THREADS
    integer, external :: OMP_GET_THREAD_NUM
#endif

    ! Check array sizes
    n = size(arr)
    if(size(idx).lt.n)call terminate('sort_index(): Index array is too small!')

#ifdef _OPENMP
    ! Use serial sort for small arrays
    if(n.lt.nmin)then
       call sort_index(arr, idx)
       return
    endif

!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(myid, numprocs, nperproc)
    
    ! Decide which sections each thread should sort
    myid        = OMP_GET_THREAD_NUM()
    numprocs    = OMP_GET_NUM_THREADS()

    nperproc    = n/numprocs
    first(myid) = nperproc*myid + 1
    if(myid.lt.numprocs-1)then
       last(myid)  = first(myid) + nperproc - 1
    else
       last(myid) = n
    endif

    ! Sort the sections
    call sort_index(arr(first(myid):last(myid)), tmp(first(myid):last(myid)))

    ! Store number of threads used in a shared variable so we can access
    ! it outside the parallel region
    if(myid.eq.0)np = numprocs

!$OMP END PARALLEL

    ! Now have a set of sorted sections. Need to merge these into a single
    ! index array
    nleft = last - first + 1
    do i = 1, n, 1
       valmin = huge(valmin)
       do j = 0, np-1, 1
          if(nleft(j).gt.0)then
             k = last(j) - nleft(j) + 1
             l = tmp(k) + first(j) - 1
             if(arr(l).le.valmin)then
                valmin = arr(l)
                imin   = j
                idxmin = l
             endif
          endif
       end do
       idx(i) = idxmin
       nleft(imin) = nleft(imin) - 1
    end do

#else
    ! No OpenMP support, so just do serial sort
    call sort_index(arr, idx)
#endif

    return
  end subroutine openmp_sort_index_real4_idx8


  subroutine openmp_sort_index_real8_idx8(arr, idx)
!
! Sort index routine parallelised with OpenMP. Falls back to serial
! sort if openmp is not available.
!
    implicit none

    ! Use ONE based array index in this routine
    real(kind=real8byte),   dimension(:), intent(inout) :: arr
    integer(kind=int8byte), dimension(:), intent(out)   :: idx
    ! Use serial sort for small arrays
    integer(kind=int8byte) :: n
    integer, parameter :: nmin = 100000
    ! OpenMP stuff
    integer :: numprocs, myid
#ifdef _OPENMP
    ! Number of threads etc
    integer, external  :: OMP_GET_MAX_THREADS
    integer, external  :: OMP_GET_NUM_PROCS
    integer, parameter :: NTHREADMAX = 1024
    ! Partitioning of array across threads
    integer(kind=int8byte), dimension(0:NTHREADMAX-1) :: first, last, nleft
    integer(kind=int8byte) :: nperproc
    integer(kind=int8byte) :: i, j, k, l
    integer(kind=int8byte), dimension(size(idx)) :: tmp
    ! Stuff for merging array sections
    real(kind=real8byte) :: valmin
    integer(kind=int8byte)              :: imin, idxmin
    integer(kind=int8byte)              :: np
    ! OpenMP functions
    integer, external :: OMP_GET_NUM_THREADS
    integer, external :: OMP_GET_THREAD_NUM
#endif

    ! Check array sizes
    n = size(arr)
    if(size(idx).lt.n)call terminate('sort_index(): Index array is too small!')

#ifdef _OPENMP
    ! Use serial sort for small arrays
    if(n.lt.nmin)then
       call sort_index(arr, idx)
       return
    endif

!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(myid, numprocs, nperproc)
    
    ! Decide which sections each thread should sort
    myid        = OMP_GET_THREAD_NUM()
    numprocs    = OMP_GET_NUM_THREADS()

    nperproc    = n/numprocs
    first(myid) = nperproc*myid + 1
    if(myid.lt.numprocs-1)then
       last(myid)  = first(myid) + nperproc - 1
    else
       last(myid) = n
    endif

    ! Sort the sections
    call sort_index(arr(first(myid):last(myid)), tmp(first(myid):last(myid)))

    ! Store number of threads used in a shared variable so we can access
    ! it outside the parallel region
    if(myid.eq.0)np = numprocs

!$OMP END PARALLEL

    ! Now have a set of sorted sections. Need to merge these into a single
    ! index array
    nleft = last - first + 1
    do i = 1, n, 1
       valmin = huge(valmin)
       do j = 0, np-1, 1
          if(nleft(j).gt.0)then
             k = last(j) - nleft(j) + 1
             l = tmp(k) + first(j) - 1
             if(arr(l).le.valmin)then
                valmin = arr(l)
                imin   = j
                idxmin = l
             endif
          endif
       end do
       idx(i) = idxmin
       nleft(imin) = nleft(imin) - 1
    end do

#else
    ! No OpenMP support, so just do serial sort
    call sort_index(arr, idx)
#endif

    return
  end subroutine openmp_sort_index_real8_idx8


  subroutine openmp_sort_index_integer4_idx8(arr, idx)
!
! Sort index routine parallelised with OpenMP. Falls back to serial
! sort if openmp is not available.
!
    implicit none

    ! Use ONE based array index in this routine
    integer(kind=int4byte), dimension(:), intent(inout) :: arr
    integer(kind=int8byte), dimension(:), intent(out)   :: idx
    ! Use serial sort for small arrays
    integer(kind=int8byte) :: n
    integer, parameter :: nmin = 100000
    ! OpenMP stuff
    integer :: numprocs, myid
#ifdef _OPENMP
    ! Number of threads etc
    integer, external  :: OMP_GET_MAX_THREADS
    integer, external  :: OMP_GET_NUM_PROCS
    integer, parameter :: NTHREADMAX = 1024
    ! Partitioning of array across threads
    integer(kind=int8byte), dimension(0:NTHREADMAX-1) :: first, last, nleft
    integer(kind=int8byte) :: nperproc
    integer(kind=int8byte) :: i, j, k, l
    integer(kind=int8byte), dimension(size(idx)) :: tmp
    ! Stuff for merging array sections
    integer(kind=int4byte) :: valmin
    integer(kind=int8byte)              :: imin, idxmin
    integer(kind=int8byte)              :: np
    ! OpenMP functions
    integer, external :: OMP_GET_NUM_THREADS
    integer, external :: OMP_GET_THREAD_NUM
#endif

    ! Check array sizes
    n = size(arr)
    if(size(idx).lt.n)call terminate('sort_index(): Index array is too small!')

#ifdef _OPENMP
    ! Use serial sort for small arrays
    if(n.lt.nmin)then
       call sort_index(arr, idx)
       return
    endif

!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(myid, numprocs, nperproc)
    
    ! Decide which sections each thread should sort
    myid        = OMP_GET_THREAD_NUM()
    numprocs    = OMP_GET_NUM_THREADS()

    nperproc    = n/numprocs
    first(myid) = nperproc*myid + 1
    if(myid.lt.numprocs-1)then
       last(myid)  = first(myid) + nperproc - 1
    else
       last(myid) = n
    endif

    ! Sort the sections
    call sort_index(arr(first(myid):last(myid)), tmp(first(myid):last(myid)))

    ! Store number of threads used in a shared variable so we can access
    ! it outside the parallel region
    if(myid.eq.0)np = numprocs

!$OMP END PARALLEL

    ! Now have a set of sorted sections. Need to merge these into a single
    ! index array
    nleft = last - first + 1
    do i = 1, n, 1
       valmin = huge(valmin)
       do j = 0, np-1, 1
          if(nleft(j).gt.0)then
             k = last(j) - nleft(j) + 1
             l = tmp(k) + first(j) - 1
             if(arr(l).le.valmin)then
                valmin = arr(l)
                imin   = j
                idxmin = l
             endif
          endif
       end do
       idx(i) = idxmin
       nleft(imin) = nleft(imin) - 1
    end do

#else
    ! No OpenMP support, so just do serial sort
    call sort_index(arr, idx)
#endif

    return
  end subroutine openmp_sort_index_integer4_idx8





  subroutine openmp_sort_index_integer8_idx8(arr, idx)
!
! Sort index routine parallelised with OpenMP. Falls back to serial
! sort if openmp is not available.
!
    implicit none

    ! Use ONE based array index in this routine
    integer(kind=int8byte), dimension(:), intent(inout) :: arr
    integer(kind=int8byte), dimension(:), intent(out)   :: idx
    ! Use serial sort for small arrays
    integer(kind=int8byte) :: n
    integer, parameter :: nmin = 100000
    ! OpenMP stuff
    integer :: numprocs, myid
#ifdef _OPENMP
    ! Number of threads etc
    integer, external  :: OMP_GET_MAX_THREADS
    integer, external  :: OMP_GET_NUM_PROCS
    integer, parameter :: NTHREADMAX = 1024
    ! Partitioning of array across threads
    integer(kind=int8byte), dimension(0:NTHREADMAX-1) :: first, last, nleft
    integer(kind=int8byte) :: nperproc
    integer(kind=int8byte) :: i, j, k, l
    integer(kind=int8byte), dimension(size(idx)) :: tmp
    ! Stuff for merging array sections
    integer(kind=int8byte) :: valmin
    integer(kind=int8byte)              :: imin, idxmin
    integer(kind=int8byte)              :: np
    ! OpenMP functions
    integer, external :: OMP_GET_NUM_THREADS
    integer, external :: OMP_GET_THREAD_NUM
#endif

    ! Check array sizes
    n = size(arr)
    if(size(idx).lt.n)call terminate('sort_index(): Index array is too small!')

#ifdef _OPENMP
    ! Use serial sort for small arrays
    if(n.lt.nmin)then
       call sort_index(arr, idx)
       return
    endif

!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(myid, numprocs, nperproc)
    
    ! Decide which sections each thread should sort
    myid        = OMP_GET_THREAD_NUM()
    numprocs    = OMP_GET_NUM_THREADS()

    nperproc    = n/numprocs
    first(myid) = nperproc*myid + 1
    if(myid.lt.numprocs-1)then
       last(myid)  = first(myid) + nperproc - 1
    else
       last(myid) = n
    endif

    ! Sort the sections
    call sort_index(arr(first(myid):last(myid)), tmp(first(myid):last(myid)))

    ! Store number of threads used in a shared variable so we can access
    ! it outside the parallel region
    if(myid.eq.0)np = numprocs

!$OMP END PARALLEL

    ! Now have a set of sorted sections. Need to merge these into a single
    ! index array
    nleft = last - first + 1
    do i = 1, n, 1
       valmin = huge(valmin)
       do j = 0, np-1, 1
          if(nleft(j).gt.0)then
             k = last(j) - nleft(j) + 1
             l = tmp(k) + first(j) - 1
             if(arr(l).le.valmin)then
                valmin = arr(l)
                imin   = j
                idxmin = l
             endif
          endif
       end do
       idx(i) = idxmin
       nleft(imin) = nleft(imin) - 1
    end do

#else
    ! No OpenMP support, so just do serial sort
    call sort_index(arr, idx)
#endif

    return
  end subroutine openmp_sort_index_integer8_idx8


  subroutine openmp_sort_index_string_idx8(arr, idx)
!
! Sort index routine parallelised with OpenMP. Falls back to serial
! sort if openmp is not available.
!
    implicit none

    ! Use ONE based array index in this routine
    character(len=*),       dimension(:), intent(inout) :: arr
    integer(kind=int8byte), dimension(:), intent(out)   :: idx
    ! Use serial sort for small arrays
    integer(kind=int8byte) :: n
    integer, parameter :: nmin = 100000
    ! OpenMP stuff
    integer :: numprocs, myid
#ifdef _OPENMP
    ! Number of threads etc
    integer, external  :: OMP_GET_MAX_THREADS
    integer, external  :: OMP_GET_NUM_PROCS
    integer, parameter :: NTHREADMAX = 1024
    ! Partitioning of array across threads
    integer(kind=int8byte), dimension(0:NTHREADMAX-1) :: first, last, nleft
    integer(kind=int8byte) :: nperproc
    integer(kind=int8byte) :: i, j, k, l
    integer(kind=int8byte), dimension(size(idx)) :: tmp
    ! Stuff for merging array sections
    character(len=len(arr)) :: valmin
    integer(kind=int8byte)              :: imin, idxmin
    integer(kind=int8byte)              :: np
    ! OpenMP functions
    integer, external :: OMP_GET_NUM_THREADS
    integer, external :: OMP_GET_THREAD_NUM
#endif

    ! Check array sizes
    n = size(arr)
    if(size(idx).lt.n)call terminate('sort_index(): Index array is too small!')

#ifdef _OPENMP
    ! Use serial sort for small arrays
    if(n.lt.nmin)then
       call sort_index(arr, idx)
       return
    endif

!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(myid, numprocs, nperproc)
    
    ! Decide which sections each thread should sort
    myid        = OMP_GET_THREAD_NUM()
    numprocs    = OMP_GET_NUM_THREADS()

    nperproc    = n/numprocs
    first(myid) = nperproc*myid + 1
    if(myid.lt.numprocs-1)then
       last(myid)  = first(myid) + nperproc - 1
    else
       last(myid) = n
    endif

    ! Sort the sections
    call sort_index(arr(first(myid):last(myid)), tmp(first(myid):last(myid)))

    ! Store number of threads used in a shared variable so we can access
    ! it outside the parallel region
    if(myid.eq.0)np = numprocs

!$OMP END PARALLEL

    ! Now have a set of sorted sections. Need to merge these into a single
    ! index array
    nleft = last - first + 1
    do i = 1, n, 1
       valmin = ""
       do j = 0, np-1, 1
          if(nleft(j).gt.0)then
             k = last(j) - nleft(j) + 1
             l = tmp(k) + first(j) - 1
             if(arr(l).le.valmin.or.valmin.eq."")then
                valmin = arr(l)
                imin   = j
                idxmin = l
             endif
          endif
       end do
       idx(i) = idxmin
       nleft(imin) = nleft(imin) - 1
    end do

#else
    ! No OpenMP support, so just do serial sort
    call sort_index(arr, idx)
#endif

    return
  end subroutine openmp_sort_index_string_idx8




end module sort



