module drawing
!
! This module provides various drawing operations used in the main
! window and the graph window.
!
! Drawing can be done with PLPlot or Cairo.
!
#include "../../config.h"

  use c_types

  implicit none
  private
  save

  public :: draw_init_mem
  public :: draw_init_ps
  public :: draw_line
  public :: draw_text
  public :: draw_box
  public :: draw_scatterplot
  public :: draw_histogram
  public :: draw_end
  public :: draw_library

contains

  subroutine draw_init_mem(image, width, height, cmap)
!
! Initialise for drawing on memory
!
    implicit none
    integer,                   intent(in)            :: width, height
    character, dimension(:),   intent(inout)         :: image
    integer,   dimension(:,:), intent(in), optional  :: cmap
    integer(kind=C_INT), dimension(:,:), allocatable :: map
    integer(kind=C_INT) :: ncol

    if(present(cmap))then
       ncol = size(cmap,2)
       allocate(map(3,ncol))
       map = cmap
    else
       ncol = 2
       allocate(map(3,ncol))
       map(1:3,1) = (/0  ,0  ,0  /)
       map(1:3,2) = (/255,255,255/)
    endif
    call cdinitmem(image, int(width, C_INT), int(height, C_INT), ncol, map)
    deallocate(map)
    
    return
  end subroutine draw_init_mem


  subroutine draw_init_ps(filename, cmap)
!
! Initialise for writing a postscript file
!
    implicit none
    character(len=*), intent(in) :: filename
    integer,   dimension(:,:), intent(in), optional  :: cmap
    integer(kind=C_INT), dimension(:,:), allocatable :: map
    integer(kind=C_INT) :: ncol
    if(present(cmap))then
       ncol = size(cmap,2)
       allocate(map(3,ncol))
       map = cmap
    else
       ncol = 2
       allocate(map(3,ncol))
       map(1:3,1) = (/0  ,0  ,0  /)
       map(1:3,2) = (/255,255,255/)
    endif
    call cdinitps(trim(filename)//achar(0), ncol, map)
    
    return
  end subroutine draw_init_ps


  subroutine draw_line(x,y,colour)
!
! Draw a line
!
    implicit none
    real,    dimension(:), intent(in) :: x, y
    integer, intent(in), optional     :: colour
    integer(kind=C_INT)               :: icol
    integer(kind=C_INT)               :: n

    n = min(size(x),size(y))
    if(present(colour))then
       icol = colour
    else
       icol = 1
    endif
    call cdline(real(x,C_DOUBLE),real(y,C_DOUBLE),icol,n)
    
    return
  end subroutine draw_line


  subroutine draw_text(text,x,y,colour)
!
! Draw text
!
    implicit none
    character(len=*),      intent(in) :: text
    real,                  intent(in) :: x,y
    integer,               intent(in), optional :: colour
    integer(kind=C_INT) :: icol
 
    if(present(colour))then
       icol = colour
    else
       icol = 1
    endif
    call cdtext(trim(text)//achar(0),real(x,C_DOUBLE),real(y,C_DOUBLE), &
         icol)

    return
  end subroutine draw_text

  
  subroutine draw_box(x,y,width,height,xmin,xmax,ymin,ymax, &
       title, xlabel, ylabel, colour, xtick, ytick)
!
! Draw x and y axes
!
    implicit none
    real,                  intent(in) :: x,y,width,height
    real,                  intent(in) :: xmin,xmax,ymin,ymax
    character(len=*),      intent(in), optional :: title, xlabel, ylabel
    integer,               intent(in), optional :: colour
    character(len=500) :: ctitle, cxlabel, cylabel
    integer(kind=C_INT) :: icol, cxtick, cytick
    logical, optional :: xtick, ytick

    if(present(title))then
       ctitle = trim(title)//achar(0)
    else
       ctitle = " "//achar(0)
    endif
    if(present(xlabel))then
       cxlabel = trim(xlabel)//achar(0)
    else
       cxlabel = " "//achar(0)
    endif
    if(present(ylabel))then
       cylabel = trim(ylabel)//achar(0)
    else
       cylabel = " "//achar(0)
    endif
    if(present(colour))then
       icol = colour
    else
       icol = 1
    endif
    cxtick = 1
    if(present(xtick))then
       if(xtick)then
          cxtick=1
       else
          cxtick=0
       endif
    endif
    cytick = 1
    if(present(ytick))then
       if(ytick)then
          cytick=1
       else
          cytick=0
       endif
    endif

    call cdbox(real(x,C_DOUBLE), real(y,C_DOUBLE), &
         real(width,C_DOUBLE), real(height,C_DOUBLE), &
         real(xmin, C_DOUBLE), real(xmax, C_DOUBLE), &
         real(ymin, C_DOUBLE), real(ymax, C_DOUBLE), &
         ctitle, cxlabel, cylabel, icol, cxtick, cytick)

    return
  end subroutine draw_box


  subroutine draw_scatterplot(x,y,colour)
!
! Draw a scatterplot
!
    implicit none
    real,    dimension(:), intent(in) :: x, y
    integer, optional,     intent(in) :: colour
    integer(kind=C_INT)               :: icol, n

    n = min(size(x),size(y))
    if(present(colour))then
       icol = colour
    else
       icol = 1
    endif
    call cdpoints(n, real(x, C_DOUBLE), real(y, C_DOUBLE), icol)

    return
  end subroutine draw_scatterplot


  subroutine draw_histogram(x,y,colour)
!
! Draw a histogram
!
    implicit none
    real,    dimension(:), intent(in) :: x, y
    integer, optional,     intent(in) :: colour
    integer(kind=C_INT)               :: icol, n

    n = min(size(x),size(y))
    if(present(colour))then
       icol = colour
    else
       icol = 1
    endif
    call cdhist(n, real(x, C_DOUBLE), real(y, C_DOUBLE), icol)

    return
  end subroutine draw_histogram


  subroutine draw_end()
!
! Close device
!
    implicit none

    call cdend()

    return
  end subroutine draw_end


  character(len=80) function draw_library()
!
! Get graphics library name and version
! 
    implicit none
    integer :: i
    logical :: flag

    call cdversion(draw_library, len(draw_library))
    flag = .false.
    do i = 1, len(draw_library), 1
       if(draw_library(i:i).eq.achar(0))flag = .true.
       if(flag)draw_library(i:i) = " "
    end do

    return
  end function draw_library

end module drawing
