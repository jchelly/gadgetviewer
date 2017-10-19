module gadget_path
!
! Module to generate path to a given snapshot
!
! This is intended for Gadget simulations. Will need an equivalent of this
! module for any other formats that are to be supported.
!
! Any instances of the snapshot number of the form "_XXX" in the path
! will be substituted when reading a different snapshot. This should be
! enough to deal with most variations of the Gadget directory structure.
!
  use f90_util
  use string_module
  use return_status

  implicit none
  private
  save

  public :: gadget_path_extract
  public :: gadget_path_generate
  public :: path_data_type

  ! Internal data
  ! -------------

  integer, parameter :: nsubmax = 10    ! Max possible substitutions

  type path_data_type
     ! The original filename
     character(len=fname_maxlen) :: input_fname
     ! Absolute path to the directory containing snapshot files
     character(len=fname_maxlen) :: path
     ! Filename, minus extensions, snapnum etc
     character(len=fname_maxlen) :: basename
     ! List of locations where we need to substitute in the snapshot number
     integer :: nsub                       ! Number of substitutions required
     integer :: sublength                  ! Length to pad snapnums to
     integer, dimension(nsubmax) :: isub   ! Locations of substitutions
     logical :: have_fileno                ! Whether we have a file number
     integer :: fileno                     ! The file number
     logical :: have_snapno                ! Whether we have a snapshot number
     logical :: is_hdf5                    ! Whether the filename has a ".hdf5"
     ! Redshift label in eagle snapshots
     logical :: have_zlabel
     character(len=fname_maxlen) :: zlabel
     integer :: dir_label
  end type path_data_type

contains

  type (result_type) function gadget_path_extract(fname_in, isnap, path_data,&
       eagle_snapshot)
!
! Given a file name, extract the path and figure out if we need to
! substitute in the snapshot number anywhere
!
    implicit none
    ! Parameters
    character(len=*), intent(in)  :: fname_in
    integer, intent(out)          :: isnap
    type(path_data_type) :: path_data
    logical, optional :: eagle_snapshot
    ! Internal
    character(len=len(fname_in)) :: buf, buf2
    integer                   :: n, i
    integer                   :: ipos
    character(len=20)         :: snapstr
    logical :: before_ok, after_ok
    character(len=len(fname_in)+fname_maxlen) :: fname
    logical :: is_eagle

    ! Return snapshot -1 if can't determine snapnum
    isnap = -1

    ! Check if this is an eagle snapshot
    if(present(eagle_snapshot))then
       is_eagle = eagle_snapshot
    else
       is_eagle = .false.
    endif

    ! Expand full path if possible
    if(.not.get_full_path(fname_in, fname))then
       fname = trim(fname_in)
    endif

    ! Check for trailing ".hdf5" extension and remove it
    path_data%is_hdf5 = .false.
    buf = trim(fname)
    if(len_trim(buf).gt.5)then
       n = len_trim(buf)
       if(buf(n-4:n).eq.".hdf5")then
          path_data%is_hdf5 = .true.
          buf = buf(1:n-5)
       endif
    endif
    buf2 = buf

    ! Assume no redshift label until we find one
    path_data%have_zlabel = .false.
    path_data%dir_label   = -1

    !
    ! Try to interpret as a multi file snapshot filename
    !
    ! Check for trailing file number and remove it
    path_data%have_fileno = .false.
    ipos = scan(buf, ".", back=.true.)
    if(ipos.gt.1.and.ipos.lt.len_trim(buf))then
       if(verify(buf(ipos+1:len_trim(buf)), &
            "0123456789", back=.true.).eq.0)then
          path_data%have_fileno = .true.
          read(buf(ipos+1:), *)path_data%fileno
          buf = buf(1:ipos-1)
       endif
    endif
    ! Check for snapshot number
    path_data%have_snapno = .false.
    ipos = scan(buf, "_", back=.true.)
    if(ipos.gt.1.and.ipos.lt.len_trim(buf))then
       if(verify(buf(ipos+1:len_trim(buf)), "0123456789", back=.true.).eq.0)then
          path_data%have_snapno = .true.
          read(buf(ipos+1:),*)isnap
          snapstr = buf(ipos+1:)
          path_data%sublength = len_trim(buf) - ipos
       else if(is_eagle.and.ipos.eq.len_trim(buf)-8)then
          if(buf(ipos+1:ipos+1).eq."z".and.buf(ipos+5:ipos+5).eq."p")then
             if(verify(buf(ipos+2:ipos+4), "0123456789", back=.true.).eq.0.and. &
                  verify(buf(ipos+6:ipos+8), "0123456789", back=.true.).eq.0)then
                ! Looks like an Eagle redshift label, so remove it
                path_data%zlabel = buf(ipos+1:ipos+8)
                buf = buf(1:ipos-1)
                path_data%have_zlabel = .true.
                ! Check if directory is labelled too
                path_data%dir_label = index(buf, trim(path_data%zlabel), back=.true.)
                if(path_data%dir_label.lt.1)path_data%dir_label = -1
                ! Extract snapshot number from what's left
                ipos = scan(buf, "_", back=.true.)
                if(ipos.gt.1.and.ipos.lt.len_trim(buf))then
                   if(verify(buf(ipos+1:len_trim(buf)), "0123456789", back=.true.).eq.0)then
                      path_data%have_snapno = .true.
                      read(buf(ipos+1:),*)isnap
                      snapstr = buf(ipos+1:)
                      path_data%sublength = len_trim(buf) - ipos
                   endif
                endif
            endif
          endif
       endif
    endif
    path_data%basename = trim(buf)

    ! Determine where we need to substitute the snapshot number
    path_data%nsub = 0
    if(path_data%have_snapno)then
       i = len_trim(fname)-path_data%sublength+1
       do while(i.gt.0)
          if(fname(i:i+path_data%sublength-1).eq.trim(snapstr))then
             ! We have a match. Check characters before and after it.
             ! Must have a leading underscore if not first character.
             before_ok = .true.
             if(i.gt.1)before_ok = (fname(i-1:i-1).eq."_")
             ! Next character must not be a number
             after_ok = .true.
             if(i+path_data%sublength.le.len_trim(fname)) &
                  after_ok = &
                  (verify(fname(path_data%sublength:path_data%sublength), &
                  "0123456789", back=.false.).eq.1)
             if(before_ok.and.after_ok)then
                ! Record this substitution
                path_data%nsub = path_data%nsub + 1
                path_data%isub(path_data%nsub) = i
                i = i - path_data%sublength
             else
                i = i - 1
             endif
          else
             i = i - 1
          endif
          if(path_data%nsub.eq.nsubmax)exit
       end do
    endif

    ! Should now be able to locate other snapshots, so will return success
    ! status
    gadget_path_extract%success = .true.

    return
  end function gadget_path_extract


  subroutine gadget_path_generate(isnap, ifile, fname, path_data)
!
! Generate the full path to a snapshot file
!
    implicit none
    ! Parameters
    integer, intent(in)           :: isnap, ifile
    character(len=*), intent(out) :: fname
    type (path_data_type)         :: path_data
    ! Internal
    character(len=500) :: buf, buf2
    character(len=50)  :: snapstr, snapfmt, str
    integer            :: snaplen
    integer            :: i, ifirst, ilast, nsub

    ! Get base name
    buf = trim(path_data%basename)
    nsub = path_data%nsub

    ! Substitute snapshot number if necessary
    if(path_data%have_snapno)then
       if(path_data%nsub.gt.0)then
          ! Determine how many digits we need for snapshot numbers
          i = 3
          do while(isnap.gt.(10**i)-1)
             i = i + 1
          end do
          snaplen = i
          ! Make string for snapshot numbers
          write(snapfmt,'(a2,i8,a1,i8,1a2)')"(I",i,".",i,")"
          write(snapstr,snapfmt)isnap
          str = trim(adjustl(snapstr))
          ! Carry out substitutions
          if(path_data%isub(nsub)-1.ge.1)then
             buf2 = buf(1:path_data%isub(nsub)-1)
          else
             buf2 = ""
          endif
          do i = 1, path_data%nsub, 1
             ! Add snapnum
             buf2 = trim(buf2) // trim(snapstr)
             ! Add any characters between this substitution and the next
             ifirst = path_data%isub(nsub-i+1) + path_data%sublength
             if(i.lt.path_data%nsub)then
                ilast = path_data%isub(nsub-i) - 1
             else
                ilast = len_trim(buf)
             endif
             if(ifirst.ge.1.and.ilast.le.len_trim(buf).and.ilast.ge.ifirst)then
                buf2 = trim(buf2) // buf(ifirst:ilast)
             endif
          end do
          buf = buf2
       endif
    endif

    ! Add redshift label if necessary
    if(path_data%have_zlabel)then
       buf = trim(buf) // "_" // "z???p???"
       if(path_data%dir_label.gt.0)then
          buf(path_data%dir_label:path_data%dir_label+7) = "z???p???"
       endif
    endif

    ! Add the file number if original filename had one
    if(path_data%have_fileno)then
       write(str,'(1i8)')ifile
       buf = trim(buf) // "." // trim(adjustl(str))
    endif

    ! Add .hdf5 if necessary
    if(path_data%is_hdf5)then
       buf = trim(buf) // ".hdf5"
    endif

    ! Thanks to annoying naming conventions have to use
    ! glob() call to find Eagle snapshot names - can't
    ! construct redshift label without reading from the file!
    if(path_data%have_zlabel)call glob(buf)

    fname = trim(buf)

    return
  end subroutine gadget_path_generate


end module gadget_path
