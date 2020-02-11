module threads
  !
  ! Module to store number of OpenMP threads to use
  !
  implicit none
  private
  save

  public :: threads_init
  public :: threads_set_keys
  public :: threads_get_keys
  public :: threads_set_number

  public :: nparmax
  public :: openmp_status
  public :: nthreads

  ! Maximum number of OpenMP threads to use by default
  integer, parameter :: MAX_DEFAULT_THREADS = 8

  ! Allowed numbers of threads (max=2**nparmax)
  integer, parameter :: nparmax = 7

#ifdef _OPENMP
  integer, external :: OMP_GET_MAX_THREADS
  integer, external :: OMP_GET_NUM_PROCS
#endif

  integer :: nthreads, nthreadmax
  character(len=500) :: openmp_status

contains

  subroutine threads_init()

    use string_module
    implicit none
    
#ifdef _OPENMP
    nthreadmax = OMP_GET_NUM_PROCS()
    nthreads   = min(MAX_DEFAULT_THREADS, nthreadmax)
    call omp_set_num_threads(nthreads)
    openmp_status = "OpenMP: enabled, initial threads: "//trim(string(nthreads))//&
         ", number of cores available: "//trim(string(nthreadmax))
#else
    nthreadmax = 1
    nthreads   = 1
    openmp_status = "OpenMP: disabled"
#endif

    return
  end subroutine threads_init


  subroutine threads_set_number(num)

    implicit none
    integer, intent(in) :: num
    
    nthreads = num
#ifdef _OPENMP
    call omp_set_num_threads(nthreads)
#endif

    return
  end subroutine threads_set_number


  subroutine threads_set_keys()

    use key_file
    implicit none

    call set_key("Threads", "Nthreads",       nthreads)

    return
  end subroutine threads_set_keys


  subroutine threads_get_keys()

    use key_file
    implicit none

    call get_key("Threads", "Nthreads", nthreads)
    call threads_set_number(nthreads)

    return
  end subroutine threads_get_keys

end module threads
