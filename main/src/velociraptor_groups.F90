module velociraptor_groups
!
! Read group catalogues output by Velociraptor halo finder
!
  use data_types
  use return_status
  use gadget_path
  use byteswapper
  use f90_util

  implicit none
  private
  save

  public :: velociraptor_groups_read

contains

  type (result_type) function velociraptor_groups_read(isnap, &
       path_data, nfof, nsub, nids, foflen, foffset, &
       sublen, suboffset, groupids)
!
! Read the specified group files
!
    implicit none
    ! Parameters
    integer, intent(in) :: isnap
    integer, intent(out) :: nfof, nsub, nids
    integer, dimension(:), pointer :: foflen, foffset, sublen, suboffset
    integer(kind=i_prop_kind), dimension(:), pointer :: groupids
    type (path_data_type) :: path_data

    ! Nullify input pointers
    nullify(foflen, foffset, sublen, suboffset, groupids)

    ! ... TODO: do something ...

    velociraptor_groups_read%success = .false.
    velociraptor_groups_read%string  = "Not implemented yet!"

    return

  contains

    subroutine cleanup()

      implicit none

      if(associated(foflen))    deallocate(foflen)
      if(associated(foffset))   deallocate(foffset)
      if(associated(sublen))    deallocate(sublen)
      if(associated(suboffset)) deallocate(suboffset)
      if(associated(groupids))  deallocate(groupids)

      return
    end subroutine cleanup

  end function velociraptor_groups_read

end module velociraptor_groups
