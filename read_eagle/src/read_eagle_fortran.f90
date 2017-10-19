module read_eagle
!
! Module for reading spatially indexed Eagle snapshots
!
! Requirements:
!
!  read_eagle.c
!  read_eagle_fortran.c.
!  HDF5 C interface
!
! TODO: use F2003 C interop features if available
!
  implicit none
  private

  ! Kind numbers for standard types
  integer, parameter :: real4byte = selected_real_kind(6,37)
  integer, parameter :: real8byte = selected_real_kind(15,307)
  integer, parameter :: int4byte = selected_int_kind(9)
  integer, parameter :: int8byte = selected_int_kind(18)

  ! Type big enough to hold a C pointer
  integer, parameter :: C_PTR = int8byte
  
  ! Types corresponding to other C datatypes
  integer, parameter :: C_FLOAT     = real4byte
  integer, parameter :: C_DOUBLE    = real8byte
  integer, parameter :: C_INT       = int4byte
  integer, parameter :: C_LONG_LONG = int8byte
  integer, parameter :: C_NULL = 0

  ! Callable routines in this module
  public :: get_error
  public :: open_snapshot
  public :: close_snapshot
  public :: select_region
  public :: count_particles
  public :: read_dataset
  public :: clear_selection

  ! Type to represent a snapshot
  type eaglesnapshot
     integer(kind=C_PTR)                  :: ptr
     real(kind=real8byte)                 :: boxsize
     integer(kind=int8byte), dimension(6) :: numpart_total
     integer                              :: numfiles
     integer                              :: hashbits
  end type eaglesnapshot
  public :: eaglesnapshot

  !
  ! Private components only below here
  ! ----------------------------------------------------------
  !

  interface read_dataset
     module procedure read_dataset_int4_1d
     module procedure read_dataset_int8_1d
     module procedure read_dataset_real4_1d
     module procedure read_dataset_real8_1d
     module procedure read_dataset_int4_2d
     module procedure read_dataset_int8_2d
     module procedure read_dataset_real4_2d
     module procedure read_dataset_real8_2d
  end interface

contains

  subroutine get_error(str)

    implicit none
    character(len=*)    :: str
    integer(kind=C_INT) :: length
    integer             :: i
    logical             :: found_null

    found_null = .false.
    length = len(str)
    call geterrorf(length, str)
    do i = 1, length, 1
       if(str(i:i).eq.achar(0))found_null = .true.
       if(found_null)then
          str(i:i) = " "
       endif
    end do

    return
  end subroutine get_error


  function open_snapshot(fname, iostat) result(res)

    implicit none
    character(len=*)     :: fname
    type (eaglesnapshot) :: res
    integer, optional    :: iostat
    real(kind=C_DOUBLE)  :: cboxsize
    integer(kind=C_INT)  :: chashbits, cnumfiles
    integer(kind=C_LONG_LONG), dimension(6) :: cnumpart_total

    if(present(iostat))then
       call abortonerrorf(int(0, kind=C_INT))
    else
       call abortonerrorf(int(1, kind=C_INT))
    endif
    call opensnapshotf(res%ptr, trim(fname)//achar(0),&
         cboxsize, cnumpart_total, cnumfiles, chashbits)
    if(present(iostat))then
       if(res%ptr.eq.C_NULL)then
          iostat = -1
       else
          iostat = 0
       endif
    endif

    res%boxsize = cboxsize
    res%numpart_total = cnumpart_total
    res%numfiles = cnumfiles
    res%hashbits = chashbits

    return
  end function open_snapshot


  subroutine close_snapshot(snap)
    
    implicit none
    type (eaglesnapshot) :: snap

    call closesnapshotf(snap%ptr)

    return
  end subroutine close_snapshot


  subroutine select_region(snap, xmin, xmax, ymin, ymax, zmin, zmax)
    
    implicit none
    type (eaglesnapshot) :: snap
    real :: xmin, xmax, ymin, ymax, zmin, zmax
    real(kind=C_DOUBLE) :: dxmin, dxmax
    real(kind=C_DOUBLE) :: dymin, dymax
    real(kind=C_DOUBLE) :: dzmin, dzmax

    dxmin = xmin
    dxmax = xmax
    dymin = ymin
    dymax = ymax
    dzmin = zmin
    dzmax = zmax
    call selectregionf(snap%ptr, dxmin, dxmax, dymin, dymax, dzmin, dzmax)

    return
  end subroutine select_region


  subroutine clear_selection(snap)

    implicit none
    type (eaglesnapshot) :: snap

    call clearselectionf(snap%ptr)

    return
  end subroutine clear_selection


  function count_particles(snap, itype, iostat) result(np)

    implicit none
    type (eaglesnapshot) :: snap
    integer              :: itype
    integer(kind=C_INT)  :: citype
    integer(kind=int8byte) :: np
    integer,    optional :: iostat
    integer(kind=C_LONG_LONG)  :: cnp

    if(present(iostat))then
       call abortonerrorf(int(0, kind=C_INT))
    else
       call abortonerrorf(int(1, kind=C_INT))
    endif
    citype = itype
    call countparticlesf(cnp, snap%ptr, citype)
    np = cnp
    if(present(iostat))then
       if(np.lt.0)then
          iostat = -1
       else
          iostat = 0
       endif
    endif

    return
  end function count_particles


  function read_dataset_int4_1d(snap, itype, name, buf, iostat) result(np)

    implicit none
    integer(kind=int8byte)               :: np
    type (eaglesnapshot)                 :: snap
    integer                              :: itype
    character(len=*)                     :: name
    integer(kind=int4byte), dimension(:) :: buf
    integer,                    optional :: iostat
    integer(kind=C_LONG_LONG)            :: cnp
    integer(kind=C_INT)                  :: citype
    integer(kind=C_LONG_LONG)            :: csize

    if(present(iostat))then
       call abortonerrorf(int(0, kind=C_INT))
    else
       call abortonerrorf(int(1, kind=C_INT))
    endif
    citype = itype
    csize  = size(buf, kind=kind(csize))
    call readdatasetf(cnp, snap%ptr, citype, int(0,C_INT), buf, csize, trim(name)//achar(0))
    np = cnp
    if(present(iostat))then
       if(np.lt.0)then
          iostat = -1
       else
          iostat = 0
       endif
    endif

    return
  end function read_dataset_int4_1d


  function read_dataset_int8_1d(snap, itype, name, buf, iostat) result(np)

    implicit none
    integer(kind=int8byte)               :: np
    type (eaglesnapshot)                 :: snap
    integer                              :: itype
    character(len=*)                     :: name
    integer(kind=int8byte), dimension(:) :: buf
    integer,                    optional :: iostat
    integer(kind=C_LONG_LONG)            :: cnp
    integer(kind=C_INT)                  :: citype
    integer(kind=C_LONG_LONG)            :: csize

    if(present(iostat))then
       call abortonerrorf(int(0, kind=C_INT))
    else
       call abortonerrorf(int(1, kind=C_INT))
    endif
    citype = itype
    csize  = size(buf, kind=kind(csize))
    call readdatasetf(cnp, snap%ptr, citype, int(1,C_INT), buf, csize, trim(name)//achar(0))
    np = cnp
    if(present(iostat))then
       if(np.lt.0)then
          iostat = -1
       else
          iostat = 0
       endif
    endif

    return
  end function read_dataset_int8_1d


  function read_dataset_real4_1d(snap, itype, name, buf, iostat) result(np)

    implicit none
    integer(kind=int8byte)               :: np
    type (eaglesnapshot)                 :: snap
    integer                              :: itype
    character(len=*)                     :: name
    real(kind=real4byte),   dimension(:) :: buf
    integer,                    optional :: iostat
    integer(kind=C_LONG_LONG)            :: cnp
    integer(kind=C_INT)                  :: citype
    integer(kind=C_LONG_LONG)            :: csize

    if(present(iostat))then
       call abortonerrorf(int(0, kind=C_INT))
    else
       call abortonerrorf(int(1, kind=C_INT))
    endif
    citype = itype
    csize  = size(buf, kind=kind(csize))
    call readdatasetf(cnp, snap%ptr, citype, int(2,C_INT), buf, csize, trim(name)//achar(0))
    np = cnp
    if(present(iostat))then
       if(np.lt.0)then
          iostat = -1
       else
          iostat = 0
       endif
    endif

    return
  end function read_dataset_real4_1d


  function read_dataset_real8_1d(snap, itype, name, buf, iostat) result(np)

    implicit none
    integer(kind=int8byte)               :: np
    type (eaglesnapshot)                 :: snap
    integer                              :: itype
    character(len=*)                     :: name
    real(kind=real8byte),   dimension(:) :: buf
    integer,                    optional :: iostat
    integer(kind=C_LONG_LONG)            :: cnp
    integer(kind=C_INT)                  :: citype
    integer(kind=C_LONG_LONG)            :: csize

    if(present(iostat))then
       call abortonerrorf(int(0, kind=C_INT))
    else
       call abortonerrorf(int(1, kind=C_INT))
    endif
    citype = itype
    csize  = size(buf, kind=kind(csize))
    call readdatasetf(cnp, snap%ptr, citype, int(3,C_INT), buf, csize, trim(name)//achar(0))
    np = cnp
    if(present(iostat))then
       if(np.lt.0)then
          iostat = -1
       else
          iostat = 0
       endif
    endif

    return
  end function read_dataset_real8_1d


  function read_dataset_int4_2d(snap, itype, name, buf, iostat) result(np)

    implicit none
    integer(kind=int8byte)               :: np
    type (eaglesnapshot)                 :: snap
    integer                              :: itype
    character(len=*)                     :: name
    integer(kind=int4byte), dimension(:,:) :: buf
    integer,                    optional :: iostat
    integer(kind=C_LONG_LONG)            :: cnp
    integer(kind=C_INT)                  :: citype
    integer(kind=C_LONG_LONG)            :: csize

    if(present(iostat))then
       call abortonerrorf(int(0, kind=C_INT))
    else
       call abortonerrorf(int(1, kind=C_INT))
    endif
    citype = itype
    csize  = size(buf, kind=kind(csize))
    call readdatasetf(cnp, snap%ptr, citype, int(0,C_INT), buf, csize, trim(name)//achar(0))
    np = cnp
    if(present(iostat))then
       if(np.lt.0)then
          iostat = -1
       else
          iostat = 0
       endif
    endif

    return
  end function read_dataset_int4_2d


  function read_dataset_int8_2d(snap, itype, name, buf, iostat) result(np)

    implicit none
    integer(kind=int8byte)               :: np
    type (eaglesnapshot)                 :: snap
    integer                              :: itype
    character(len=*)                     :: name
    integer(kind=int8byte), dimension(:,:) :: buf
    integer,                    optional :: iostat
    integer(kind=C_LONG_LONG)            :: cnp
    integer(kind=C_INT)                  :: citype
    integer(kind=C_LONG_LONG)            :: csize

    if(present(iostat))then
       call abortonerrorf(int(0, kind=C_INT))
    else
       call abortonerrorf(int(1, kind=C_INT))
    endif
    citype = itype
    csize  = size(buf, kind=kind(csize))
    call readdatasetf(cnp, snap%ptr, citype, int(1,C_INT), buf, csize, trim(name)//achar(0))
    np = cnp
    if(present(iostat))then
       if(np.lt.0)then
          iostat = -1
       else
          iostat = 0
       endif
    endif

    return
  end function read_dataset_int8_2d


  function read_dataset_real4_2d(snap, itype, name, buf, iostat) result(np)

    implicit none
    integer(kind=int8byte)               :: np
    type (eaglesnapshot)                 :: snap
    integer                              :: itype
    character(len=*)                     :: name
    real(kind=real4byte),   dimension(:,:) :: buf
    integer,                    optional :: iostat
    integer(kind=C_LONG_LONG)            :: cnp
    integer(kind=C_INT)                  :: citype
    integer(kind=C_LONG_LONG)            :: csize

    if(present(iostat))then
       call abortonerrorf(int(0, kind=C_INT))
    else
       call abortonerrorf(int(1, kind=C_INT))
    endif
    citype = itype
    csize  = size(buf, kind=kind(csize))
    call readdatasetf(cnp, snap%ptr, citype, int(2,C_INT), buf, csize, trim(name)//achar(0))
    np = cnp
    if(present(iostat))then
       if(np.lt.0)then
          iostat = -1
       else
          iostat = 0
       endif
    endif

    return
  end function read_dataset_real4_2d


  function read_dataset_real8_2d(snap, itype, name, buf, iostat) result(np)

    implicit none
    integer(kind=int8byte)               :: np
    type (eaglesnapshot)                 :: snap
    integer                              :: itype
    character(len=*)                     :: name
    real(kind=real8byte),   dimension(:,:) :: buf
    integer,                    optional :: iostat
    integer(kind=C_LONG_LONG)            :: cnp
    integer(kind=C_INT)                  :: citype
    integer(kind=C_LONG_LONG)            :: csize

    if(present(iostat))then
       call abortonerrorf(int(0, kind=C_INT))
    else
       call abortonerrorf(int(1, kind=C_INT))
    endif
    citype = itype
    csize  = size(buf, kind=kind(csize))
    call readdatasetf(cnp, snap%ptr, citype, int(3,C_INT), buf, csize, trim(name)//achar(0))
    np = cnp
    if(present(iostat))then
       if(np.lt.0)then
          iostat = -1
       else
          iostat = 0
       endif
    endif

    return
  end function read_dataset_real8_2d


end module read_eagle
