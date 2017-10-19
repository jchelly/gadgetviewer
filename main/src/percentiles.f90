module percentiles
!
! Module to calculate median and/or percentiles
!
  use sort
  use data_types

  implicit none
  private
  
  interface percentile
     module procedure percentile_real4
     module procedure percentile_real8
  end interface
  public :: percentile

contains

  real function percentile_real4(p, n, data)
!
! Calculate the (p*100)'th percentile of the first n elements
! in the array data. p must be between 0 and 1 (not a 
! percentage!). p=0.5 gives the median.
! 
! Uses linear interpolation between the nearest two data
! points.
!
    implicit none
    integer :: n
    real    :: p
    real(kind=real4byte), dimension(:) :: data
    integer, dimension(n) :: idx
    integer :: ilower, ihigher
    real    :: r_lower, r_higher
    real    :: f

    if(n.eq.0)then
       ! Zero points - can't do anything
       stop'Cannot calculate percentile for zero data points!'
    else if(n.eq.1)then
       ! One point - just return that data point
       percentile_real4 = data(1)
    else
       ! Sort the data points
       call openmp_sort_index(data(1:n), idx(1:n))
       ilower  = min(n,max(1,floor(p*(n-1.0)) + 1))
       ihigher = min(n,max(1,ceiling(p*(n-1.0)) + 1))
       if(ilower.eq.ihigher)then
          ! Percentile lands exactly on one of the data points,
          ! so just return it
          percentile_real4 = data(idx(ilower))
       else
          ! Percentile is between data points. Do linear
          ! interpolation
          r_lower  = data(idx(ilower))
          r_higher = data(idx(ihigher))
          f = ((p*(n-1.0))-real(ilower-1.0))
          if(f.lt.0.0.or.f.gt.1.0)stop'f out of range'
          percentile_real4 = (f*r_higher) + ((1-f)*r_lower)
       endif
    endif

    return
  end function percentile_real4


  real function percentile_real8(p, n, data)
!
! Calculate the (p*100)'th percentile of the first n elements
! in the array data. p must be between 0 and 1 (not a 
! percentage!). p=0.5 gives the median.
! 
! Uses linear interpolation between the nearest two data
! points.
!
    implicit none
    integer :: n
    real    :: p
    real(kind=real8byte), dimension(:) :: data
    integer, dimension(n) :: idx
    integer :: ilower, ihigher
    real    :: r_lower, r_higher
    real    :: f

    if(n.eq.0)then
       ! Zero points - can't do anything
       stop'Cannot calculate percentile for zero data points!'
    else if(n.eq.1)then
       ! One point - just return that data point
       percentile_real8 = data(1)
    else
       ! Sort the data points
       call openmp_sort_index(data(1:n), idx(1:n))
       ilower  = min(n,max(1,floor(p*(n-1.0)) + 1))
       ihigher = min(n,max(1,ceiling(p*(n-1.0)) + 1))
       if(ilower.eq.ihigher)then
          ! Percentile lands exactly on one of the data points,
          ! so just return it
          percentile_real8 = data(idx(ilower))
       else
          ! Percentile is between data points. Do linear
          ! interpolation
          r_lower  = data(idx(ilower))
          r_higher = data(idx(ihigher))
          f = ((p*(n-1.0))-real(ilower-1.0))
          if(f.lt.0.0.or.f.gt.1.0)stop'f out of range'
          percentile_real8 = (f*r_higher) + ((1-f)*r_lower)
       endif
    endif

    return
  end function percentile_real8


end module percentiles
