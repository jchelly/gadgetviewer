module data_types
!
! This defines the 'kind' parameters used for various
! quantities. It also defines equivalent HDF5 data types.
!
#include "../../config.h"

  use f90_util

  implicit none

  ! Numbers of bytes to use for particle data are provided by config.h
  ! Need to convert these to kind numbers.
  integer, parameter, dimension(2) :: imap = (/ int4byte,  int8byte  /)
  integer, parameter, dimension(2) :: rmap = (/ real4byte, real8byte /)
  integer, parameter :: pos_kind    = rmap( POSKIND   /4 )
  integer, parameter :: vel_kind    = rmap( VELKIND   /4 )
  integer, parameter :: r_prop_kind = rmap( RPROPKIND /4 )
  integer, parameter :: i_prop_kind = imap( IPROPKIND /4 )
  integer, parameter :: index_kind  = imap( INDEXKIND /4 )

  ! Types to allow arrays of pointers to property arrays
  type r_prop_ptr_type
     real(kind=r_prop_kind),    dimension(:), pointer :: ptr
  end type r_prop_ptr_type
  type i_prop_ptr_type
     integer(kind=i_prop_kind), dimension(:), pointer :: ptr
  end type i_prop_ptr_type

end module data_types
