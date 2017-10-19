module colour_bar

#include "../../config.h"
!
! Module to draw a colour bar on an image stored in memory
!
  use drawing
  use data_types
  use colour_table

  implicit none
  private
  save

  public :: colour_bar_draw

contains

  subroutine colour_bar_draw(image, width, height, &
       x, y, cb_width, cb_height, itab, rmin, rmax, name)

    implicit none
    ! Parameters
    integer :: width, height
    character(len=1), dimension(0:3*width*height-1) :: image
    integer :: x, y, cb_width, cb_height, itab
    real    :: rmin, rmax
    ! Variables for drawing colour bar
    integer          :: i,j,icol
    character(len=*) :: name

    ! Draw the colour bar
    do i = x, x+cb_width-1, 1
       icol = min(floor(real(i-x)/real(cb_width)*256.0),255)
       do j = y, y+cb_height-1, 1
          if(i.ge.0.and.i.lt.width.and.j.ge.0.and.j.lt.height)then
             image((3*width*j+3*i+0):(3*width*j+3*i+2)) = &
                  coltab(itab)%cdata(1:3,icol)
          endif
       end do
    end do

    ! Draw on image
    call draw_init_mem(image,width,height)
    call draw_box(real(x)/real(width), &
         1.0-real(y)/real(height)-real(cb_height)/real(height), &
         real(cb_width)/real(width), real(cb_height)/real(height), &
         rmin, rmax, 0.0, 1.0, &
         colour=1, xlabel=trim(name), ytick=.false.)
    call draw_end()

    return
  end subroutine colour_bar_draw

end module colour_bar
