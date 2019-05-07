module c_types

#include "../../config.h"

  ! Determine kind number for an integer which can store a C pointer
#if ( SIZEOF_VOID_P == 4 )
  integer, parameter :: C_PTR = selected_int_kind(9)
#elif ( SIZEOF_VOID_P == 8 )
  integer, parameter :: C_PTR = selected_int_kind(18)
#else
#error Size of C (void *) must be 4 or 8 bytes
#endif

! Determine kind number for C integers
#if ( SIZEOF_INT == 4 )
  integer, parameter :: C_INT = selected_int_kind(9)
#elif ( SIZEOF_INT == 8 )
  integer, parameter :: C_INT = selected_int_kind(18)
#else
#error Size of C int must be 4 or 8 bytes
#endif

! Determine kind number for C long long
#if ( SIZEOF_LONG_LONG == 8 )
  integer, parameter :: C_LONG_LONG = selected_int_kind(18)
#else
#error Size of C long long must be 8 bytes
#endif

! Determine kind number for C doubles
#if ( SIZEOF_DOUBLE == 8 )
  integer, parameter :: C_DOUBLE = selected_real_kind(12,300)
#else
#error Size of C double must be 8 bytes
#endif

end module c_types
