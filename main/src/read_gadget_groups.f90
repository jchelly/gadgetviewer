module gadget_groups
!
! Read group catalogues output by Gadget or various versions of
! SubFind.
!
! Parameters which define the format:
! -----------------------------------
!
! Header information
!
! * have_subhalos - true if the catalogue includes subgroups
! * nfiles_offset - byte offset to no. of files in the tab file 
! * nfof_offset   - byte offset to no. of FoF groups in the tab file
!                   or -1 if this quantity isn't in the file
! * nsub_offset   - byte offset to no. of subgroups in the tab file
!                   or -1 if this quantity isn't in the file
! * nids_offset   - byte offset to no. of IDs in the ids file
!
! Group IDs / subhalo IDs
!
! * ids_offset    - byte offset to the ID list in the ids file
! 
! Lengths and offsets
!
! * len_offset
! * len_nsub_factor
! * len_nfof_factor
!
! * offset_offset
! * offset_nsub_factor
! * offset_nfof_factor
!
! Offsets in the file are in bytes. The number of files is used to 
! determine the endian-ness of the group catalogue.
!
  use data_types
  use return_status
  use gadget_path
  use byteswapper
  use f90_util

  implicit none
  private
  save

  public :: gadget_groups_read
  public :: gadget_groups_format_list

!
! Group format definitions
!
  type group_format_type
     character(len=41) :: name
     logical :: have_subfind
     logical :: have_fof
     integer :: nfiles_offset
     integer :: nfof_offset
     integer :: nsub_offset
     integer :: nids_offset
     integer :: ids_offset
     integer :: flen_offset
     integer :: flen_nsub_factor
     integer :: flen_nfof_factor
     integer :: foffset_offset
     integer :: foffset_nsub_factor
     integer :: foffset_nfof_factor
     integer :: slen_offset
     integer :: slen_nsub_factor
     integer :: slen_nfof_factor
     integer :: soffset_offset
     integer :: soffset_nsub_factor
     integer :: soffset_nfof_factor
     logical :: file_relative_offsets
     logical :: have_hashkeys
  end type group_format_type


  type(group_format_type), parameter :: FORMAT1 = group_format_type( &
       "L-Gadget group_tab                        "         , & ! Name
       .false.    , & ! have_subfind
       .true.     , & ! have_fof
       12         , & ! nfiles_offset
       0          , & ! nfof_offset
       -1         , & ! nsub_offset
       4          , & ! nids_offset
       16         , & ! ids_offset
       16         , & ! flen_offset
       0          , & ! flen_nsub_factor
       0          , & ! flen_nfof_factor
       16         , & ! foffset_offset
       0          , & ! foffset_nsub_factor
       4          , & ! foffset_nfof_factor
       -1         , & ! slen_offset
       -1         , & ! slen_nsub_factor
       -1         , & ! slen_nfof_factor
       -1         , & ! soffset_offset
       -1         , & ! soffset_nsub_factor
       -1         , & ! soffset_nfof_factor
       .true.     , & ! file_relative_offsets
       .true.       & ! have_hashkeys
       )

  type(group_format_type), parameter :: FORMAT2 = group_format_type( &
       "L-SubFind sub_tab                         "         , & ! Name
       .true.     , & ! have_subfind
       .false.    , & ! have_fof
       12         , & ! nfiles_offset
       0          , & ! nfof_offset
       16         , & ! nsub_offset
       4          , & ! nids_offset
       16         , & ! ids_offset
       -1         , & ! flen_offset
       -1         , & ! flen_nsub_factor
       -1         , & ! flen_nfof_factor
       -1         , & ! foffset_offset
       -1         , & ! foffset_nsub_factor
       -1         , & ! foffset_nfof_factor
       20         , & ! slen_offset
       0          , & ! slen_nsub_factor
       8          , & ! slen_nfof_factor
       20         , & ! soffset_offset
       4          , & ! soffset_nsub_factor
       8          , & ! soffset_nfof_factor
       .true.     , & ! file_relative_offsets
       .true.       & ! have_hashkeys
       )

  type(group_format_type), parameter :: FORMAT3 = group_format_type( &
       "P-Gadget3 group_tab                       "         , & ! Name
       .false.    , & ! have_subfind
       .true.     , & ! have_fof
       20         , & ! nfiles_offset
       0          , & ! nfof_offset
       -1         , & ! nsub_offset
       8          , & ! nids_offset
       28         , & ! ids_offset
       24         , & ! flen_offset
       0          , & ! flen_nsub_factor
       0          , & ! flen_nfof_factor
       24         , & ! foffset_offset
       0          , & ! foffset_nsub_factor
       4          , & ! foffset_nfof_factor
       -1         , & ! slen_offset
       -1         , & ! slen_nsub_factor
       -1         , & ! slen_nfof_factor
       -1         , & ! soffset_offset
       -1         , & ! soffset_nsub_factor
       -1         , & ! soffset_nfof_factor
       .false.    , & ! file_relative_offsets
       .false.      & ! have_hashkeys
       )

  type(group_format_type), parameter :: FORMAT4 = group_format_type( &
       "P-Gadget3 subhalo_tab                     "         , & ! Name
       .true.     , & ! have_subfind
       .true.     , & ! have_fof
       20         , & ! nfiles_offset
       0          , & ! nfof_offset
       24         , & ! nsub_offset
       8          , & ! nids_offset
       28         , & ! ids_offset
       32         , & ! flen_offset
       0          , & ! flen_nsub_factor
       0          , & ! flen_nfof_factor
       32         , & ! foffset_offset
       0          , & ! foffset_nsub_factor
       1*4        , & ! foffset_nfof_factor
       32         , & ! slen_offset
       0          , & ! slen_nsub_factor
       16*4       , & ! slen_nfof_factor
       32         , & ! soffset_offset
       4          , & ! soffset_nsub_factor
       16*4       , & ! soffset_nfof_factor
       .false.    , & ! file_relative_offsets
       .false.      & ! have_hashkeys
       )

 type(group_format_type), parameter :: FORMAT5 = group_format_type( &
       "P-Gadget3 subhalo_tab (with -DSO_VELDISP)"         , & ! Name
       .true.     , & ! have_subfind
       .true.     , & ! have_fof
       20         , & ! nfiles_offset
       0          , & ! nfof_offset
       24         , & ! nsub_offset
       8          , & ! nids_offset
       28         , & ! ids_offset
       32         , & ! flen_offset
       0          , & ! flen_nsub_factor
       0          , & ! flen_nfof_factor
       32         , & ! foffset_offset
       0          , & ! foffset_nsub_factor
       4          , & ! foffset_nfof_factor
       32         , & ! slen_offset
       0          , & ! slen_nsub_factor
       22*4       , & ! slen_nfof_factor
       32         , & ! soffset_offset
       4          , & ! soffset_nsub_factor
       22*4       , & ! soffset_nfof_factor
       .false.    , & ! file_relative_offsets
       .false.      & ! have_hashkeys
       )

  integer, parameter :: nformat = 5
  type(group_format_type), dimension(nformat), target :: group_format = &
       (/ FORMAT1, FORMAT2, FORMAT3, FORMAT4, FORMAT5 /)

contains

  type (result_type) function gadget_groups_read(iformat, isnap, &
       path_data, id_size, nfof, nsub, nids, foflen, foffset, &
       sublen, suboffset, groupids)
!
! Read the specified group files
!
    implicit none
    ! Parameters
    integer, intent(in) :: iformat, id_size, isnap
    integer, intent(out) :: nfof, nsub, nids
    integer, dimension(:), pointer :: foflen, foffset, sublen, suboffset
    integer(kind=i_prop_kind), dimension(:), pointer :: groupids
    type (path_data_type) :: path_data
    ! Internal
    integer :: istat
    integer :: ifile
    integer :: ios
    character(len=500) :: tab_file, ids_file
    type (group_format_type), pointer :: gf
    integer :: nsubread, nfofread, nidsread
    integer :: filepos
    logical :: need_byteswap
    ! Quantities read from the file(s)
    integer, parameter :: nfilemax = 1024
    integer(kind=int4byte), dimension(0:nfilemax-1) :: nsubfile, nfoffile, &
                                                       nidsfile
    integer(kind=int4byte) :: nfiles
    ! Read buffers
    integer(kind=int4byte), dimension(:), allocatable :: i4buf
    integer(kind=int8byte), dimension(:), allocatable :: i8buf
    integer :: ifof, isub
    integer :: ifirst, ilast, i 
    logical :: fexist

    ! Nullify input pointers
    nullify(foflen, foffset, sublen, suboffset, groupids)
    nfoffile = 0
    nsubfile = 0
    nidsfile = 0
    gf => group_format(iformat)

    ! Read the file headers to determine total groups and
    ! number of files
    ifile = 0
    nfiles = 1
    do while(ifile.lt.nfiles)

       ! Generate the path to the tab file
       call gadget_path_generate(isnap, ifile, tab_file, path_data)

       ! Special case: if we can't find a second file it may be that all of the groups
       ! are in one file (TODO - add a check on the number of groups/ids etc)
       inquire(file=tab_file, exist=fexist, iostat=ios)
       if(ios.eq.0)then
          if(.not.fexist.and.ifile.eq.1)then
             nfiles = 1
             exit
          endif
       else
          gadget_groups_read%string  = "Unable to access file: "//trim(tab_file)
          gadget_groups_read%success = .false.
          return
       endif

       call open_binary(tab_file, ios)
       if(ios.ne.0)then
          gadget_groups_read%string  = "Unable to open file: "//trim(tab_file)
          gadget_groups_read%success = .false.
          return
       endif

       call read_binary(nfiles, ios, pos=gf%nfiles_offset)
       if(ios.ne.0)then
          gadget_groups_read%string  = "Unable to read from file: "//&
               trim(tab_file)
          gadget_groups_read%success = .false.
          return
       endif
       need_byteswap = (nfiles.lt.0.or.nfiles.ge.65536)
       if(need_byteswap)call byteswap(nfiles)

       ! Read numbers of groups per file
       if(gf%nfof_offset.ge.0)then
          call read_binary(nfoffile(ifile), ios, pos=gf%nfof_offset)
          if(ios.ne.0)then
             gadget_groups_read%string  = "Unable to read from file: "//&
                  trim(tab_file)
             gadget_groups_read%success = .false.
             return
          endif
       else
          ! Negative offset means nfof isn't stored in the file
          nfoffile(ifile) = 0
       endif
       if(gf%nsub_offset.ge.0)then
          call read_binary(nsubfile(ifile), ios, pos=gf%nsub_offset)
          if(ios.ne.0)then
             gadget_groups_read%string  = "Unable to read from file: "//&
                  trim(tab_file)
             gadget_groups_read%success = .false.
             return
          endif
       else
          ! Negative offset means nsub isn't stored in the file
          nsubfile(ifile) = 0
       endif

       call close_binary()

       ! Guess ids file name by replacing "tab" with "ids" in tab_file
       i = index(tab_file, "tab", back=.true.)
       if(i.gt.0)then
          ids_file = trim(tab_file)
          ids_file(i:i+2) = "ids"
       else
          gadget_groups_read%success = .false.
          gadget_groups_read%string  = "Can't guess ids file name from "//trim(tab_file)
          return
       endif

       ! Read the subhalo_ids file
       call open_binary(ids_file, ios)
       if(ios.ne.0)then
          gadget_groups_read%string  = "Unable to open file: "//trim(ids_file)
          gadget_groups_read%success = .false.
          return
       endif
       call read_binary(nidsfile(ifile), ios, pos=gf%nids_offset)
       if(ios.ne.0)then
          gadget_groups_read%string  = "Unable to read from file: "//&
               trim(ids_file)
          gadget_groups_read%success = .false.
          return
       endif
       call close_binary()

       ifile = ifile + 1
    end do

    if(need_byteswap)then
       call byteswap(nfoffile)
       call byteswap(nsubfile)
       call byteswap(nidsfile)
    endif

    !
    ! Allocate memory
    !
    nfof = sum(nfoffile)
    nsub = sum(nsubfile)
    nids = sum(nidsfile)

    if(gf%have_fof)then
       allocate(foflen(nfof), foffset(nfof), stat=istat)
       if(istat.ne.0)then
          gadget_groups_read%string  = "Unable to allocate enough memory"
          gadget_groups_read%success = .false.
          call cleanup()
          return
       endif
    endif

    if(gf%have_subfind)then
       allocate(sublen(nsub), suboffset(nsub), stat=istat)
       if(istat.ne.0)then
          gadget_groups_read%string  = "Unable to allocate enough memory"
          gadget_groups_read%success = .false.
          call cleanup()
          return
       endif
    endif

    allocate(groupids(nids), stat=istat)
    if(istat.ne.0)then
       gadget_groups_read%string  = "Unable to allocate enough memory"
       gadget_groups_read%success = .false.
       call cleanup()
       return
    endif

    !
    ! Read the tab files
    !
    nsubread = 0
    nfofread = 0

    do ifile = 0, nfiles-1, 1

       call gadget_path_generate(isnap, ifile, tab_file, path_data)
       call open_binary(tab_file, ios)
       if(ios.ne.0)then
          gadget_groups_read%string  = "Unable to open file: "//trim(tab_file)
          gadget_groups_read%success = .false.
          call cleanup()
          return
       endif
       
       if(gf%have_fof)then

          allocate(i4buf(nfoffile(ifile)), stat=istat)
          if(istat.ne.0)then
             gadget_groups_read%string  = "Unable to allocate enough memory"
             gadget_groups_read%success = .false.
             call cleanup()
             return
          endif

          ! Read the FoF lengths
          filepos = &
               gf%flen_offset + &
               gf%flen_nsub_factor * nsubfile(ifile) + &
               gf%flen_nfof_factor * nfoffile(ifile)
          call read_binary(i4buf, ios, pos=filepos)
          if(ios.ne.0)then
             gadget_groups_read%string  = "Unable to read from file: "//&
                  trim(tab_file)
             gadget_groups_read%success = .false.
             call cleanup()
             return
          endif
          foflen(nfofread+1:nfofread+nfoffile(ifile)) = i4buf

          ! Read the FoF offsets
          filepos = &
               gf%foffset_offset + &
               gf%foffset_nsub_factor * nsubfile(ifile) + &
               gf%foffset_nfof_factor * nfoffile(ifile)
          call read_binary(i4buf, ios, pos=filepos)
          if(ios.ne.0)then
             gadget_groups_read%string  = "Unable to read from file: "//&
                  trim(tab_file)
             gadget_groups_read%success = .false.
             call cleanup()
             return
          endif
          foffset(nfofread+1:nfofread+nfoffile(ifile)) = i4buf
          
          deallocate(i4buf)

       endif

       if(gf%have_subfind)then

          allocate(i4buf(nsubfile(ifile)), stat=istat)
          if(istat.ne.0)then
             gadget_groups_read%string  = "Unable to allocate enough memory"
             gadget_groups_read%success = .false.
             call cleanup()
             return
          endif

          ! Read the Sub lengths
          filepos = &
               gf%slen_offset + &
               gf%slen_nsub_factor * nsubfile(ifile) + &
               gf%slen_nfof_factor * nfoffile(ifile)
          call read_binary(i4buf, ios, pos=filepos)
          if(ios.ne.0)then
             gadget_groups_read%string  = "Unable to read from file: "//&
                  trim(tab_file)
             gadget_groups_read%success = .false.
             call cleanup()
             return
          endif
          sublen(nsubread+1:nsubread+nsubfile(ifile)) = i4buf

          ! Read the Sub offsets
          filepos = &
               gf%soffset_offset + &
               gf%soffset_nsub_factor * nsubfile(ifile) + &
               gf%soffset_nfof_factor * nfoffile(ifile)
          call read_binary(i4buf, ios, pos=filepos)
          if(ios.ne.0)then
             gadget_groups_read%string  = "Unable to read from file: "//&
                  trim(tab_file)
             gadget_groups_read%success = .false.
             call cleanup()
             return
          endif
          suboffset(nsubread+1:nsubread+nsubfile(ifile)) = i4buf

          deallocate(i4buf)

       endif

       nsubread = nsubread + nsubfile(ifile)
       nfofread = nfofread + nfoffile(ifile)

       ! Next file
       call close_binary()
    end do
    
    ! 
    ! Read the IDs files
    !
    nidsread = 0

    do ifile = 0, nfiles-1, 1

       if(id_size.eq.4)then
          allocate(i4buf(nidsfile(ifile)), stat=istat)
       else
          allocate(i8buf(nidsfile(ifile)), stat=istat)
       endif
       if(istat.ne.0)then
          gadget_groups_read%string  = "Unable to allocate enough memory"
          gadget_groups_read%success = .false.
          call cleanup()
          return
       endif

       ! Guess ids file name by replacing "tab" with "ids" in tab_file
       call gadget_path_generate(isnap, ifile, tab_file, path_data)
       i = index(tab_file, "tab", back=.true.)
       if(i.gt.0)then
          ids_file = trim(tab_file)
          ids_file(i:i+2) = "ids"
       else
          gadget_groups_read%success = .false.
          gadget_groups_read%string  = "Can't guess ids file name from "//trim(tab_file)
          return
       endif

       ! Open the file
       call open_binary(ids_file, ios)
       if(ios.ne.0)then
          gadget_groups_read%string  = "Unable to open file: "//trim(ids_file)
          gadget_groups_read%success = .false.
          call cleanup()
          return
       endif

       ! Read the IDs
       filepos = gf%ids_offset
       if(id_size.eq.4)then
          ! Assume 4 byte IDs never have hash keys
          call read_binary(i4buf, ios, pos=filepos)
          if(need_byteswap)call byteswap(i4buf)
       else
          call read_binary(i8buf, ios, pos=filepos)
          if(need_byteswap)call byteswap(i8buf)
          ! Remove hash keys if necessary. Have to do this BEFORE
          ! copying into groupids, which may be integer*4
          if(gf%have_hashkeys)i8buf = ishft(ishft(i8buf,30),-30)
       endif
       if(ios.ne.0)then
          gadget_groups_read%string  = "Unable to read from file: "//&
               trim(ids_file)
          gadget_groups_read%success = .false.
          call cleanup()
          return
       endif
       
       if(id_size.eq.4)then
          groupids(nidsread+1:nidsread+nidsfile(ifile)) = i4buf
          deallocate(i4buf)
       else
          groupids(nidsread+1:nidsread+nidsfile(ifile)) = i8buf
          deallocate(i8buf)
       endif

       nidsread = nidsread + nidsfile(ifile)

       call close_binary()
    
       ! Next IDs file
    end do

    ! Byteswap the data arrays
    ! GroupIDs has already been done.
    if(need_byteswap)then
       if(gf%have_fof)then
          call byteswap(foffset)
          call byteswap(foflen)
       endif
       if(gf%have_subfind)then
          call byteswap(suboffset)
          call byteswap(sublen)
       endif
    endif

    ! Make all offsets relative to the start of the first file
    if(gf%file_relative_offsets)then
       do ifile = 1, nfiles-1, 1

          ! FoF groups
          if(gf%have_fof)then
             ifirst = sum(nfoffile(0:ifile-1)) + 1
             ilast  = sum(nfoffile(0:ifile))
             foffset(ifirst:ilast) = foffset(ifirst:ilast) + &
                  sum(nidsfile(0:ifile-1))
          endif

          ! Subgroups
          if(gf%have_subfind)then
             ifirst = sum(nsubfile(0:ifile-1)) + 1
             ilast  = sum(nsubfile(0:ifile))
             suboffset(ifirst:ilast) = suboffset(ifirst:ilast) + &
                  sum(nidsfile(0:ifile-1))
          endif

       end do
    endif

    ! Make offsets start at one rather than zero
    if(gf%have_fof)     foffset   = foffset + 1
    if(gf%have_subfind) suboffset = suboffset + 1

    ! Sanity check the resulting arrays
    if(gf%have_fof)then
       do ifof = 1, nfof, 1
          if(foffset(ifof).lt.1.or.foffset(ifof).gt.nids)then
             gadget_groups_read%string  = &
                  "FoF offset(s) inconsistent with no. of IDs"
             gadget_groups_read%success = .false.
             call cleanup()
             return
          endif
          if(foffset(ifof)+foflen(ifof).lt.1.or.&
               foflen(ifof)+foffset(ifof)-1.gt.nids)then
             gadget_groups_read%string  = &
                  "FoF group length(s) inconsistent with no. of IDs"
             gadget_groups_read%success = .false.
             call cleanup()
             return
          endif
       end do
    endif
    if(gf%have_subfind)then
       do isub = 1, nsub, 1
          if(suboffset(isub).lt.1.or.suboffset(isub).gt.nids)then
             gadget_groups_read%string  = &
                  "Subhalo offset(s) inconsistent with no. of IDs"
             gadget_groups_read%success = .false.
             call cleanup()
             return
          endif
       end do
       do isub = 1, nsub, 1
          if(suboffset(isub)+sublen(isub).lt.1.or. &
               suboffset(isub)+sublen(isub)-1.gt.nids)then
             gadget_groups_read%string  = &
                  "Subhalo length(s) inconsistent with no. of IDs"
             gadget_groups_read%success = .false.
             call cleanup()
             return
          endif
       end do
    endif

    gadget_groups_read%success = .true.

    return

  contains

    subroutine cleanup()

      implicit none

      if(associated(foflen))    deallocate(foflen)
      if(associated(foffset))   deallocate(foffset)
      if(associated(sublen))    deallocate(sublen)
      if(associated(suboffset)) deallocate(suboffset)
      if(associated(groupids))  deallocate(groupids)

      if(allocated(i4buf))deallocate(i4buf)
      if(allocated(i8buf))deallocate(i8buf)

      return
    end subroutine cleanup

  end function gadget_groups_read


  subroutine gadget_groups_format_list(n, format_names)
!
! Return the names of the supported formats
!
    implicit none
    integer :: n
    character(len=*), dimension(:), optional :: format_names
    integer :: i

    if(present(format_names))then
       if(size(format_names).lt.nformat) &
            call terminate('format_names array too small in gadget_groups')
       do i = 1, nformat, 1
          format_names(i) = trim(group_format(i)%name)
       end do
    endif

    n = nformat

    return
  end subroutine gadget_groups_format_list


end module gadget_groups
