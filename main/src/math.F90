module math

  use data_types

  implicit none
  private
  save

  ! Log function which returns -1000.0 for values <= 0.0
  interface mylog10
     module procedure mylog10_r4
     module procedure mylog10_r8
  end interface
  public :: mylog10

contains

  elemental function mylog10_r4(x) result(res)
!
! Return log10(x) if x is positive, -1000 otherwise
!
    implicit none
    real(kind=real4byte), intent(in) :: x
    real(kind=real4byte)             :: res

    if(x.le.0.0)then
       res = -1000.0
    else
       res = log10(x)
    endif

    return
  end function mylog10_r4


  elemental function mylog10_r8(x) result(res)
!
! Return log10(x) if x is positive, -1000 otherwise
!
    implicit none
    real(kind=real8byte), intent(in) :: x
    real(kind=real8byte)             :: res

    if(x.le.0.0)then
       res = -1000.0
    else
       res = log10(x)
    endif

    return
  end function mylog10_r8

end module math
