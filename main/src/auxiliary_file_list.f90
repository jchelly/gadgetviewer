module auxiliary_file_list
!
! List of files with additional data to read on snapshot change
!
  use gadget_path
  use particle_store
  use data_types

  implicit none
  public
  save
  
  integer :: naux
  integer, parameter :: nauxmax = 20
  type auxiliary_file
     ! File location and format
     integer                        :: nfiles
     type(path_data_type)           :: path_data
     character(len=maxlen)          :: format
     ! Information about the array to load
     character(len=maxlen)          :: prop_name
     integer                        :: ispecies
     character(len=maxlen)          :: type
     ! Information for binary files
     integer                        :: record_no
     integer                        :: nbytes
     ! Information for HDF5 files
     character(len=maxlen)          :: dataset_name
     ! Information for ascii files
     integer                        :: header_lines
     integer                        :: column_no
     ! Whether the file could be read
     logical                        :: ok
     logical                        :: allocated
     character(len=500)             :: error
     integer(kind=index_kind)       :: nread
  end type auxiliary_file
  type(auxiliary_file), dimension(nauxmax) :: aux_file

end module auxiliary_file_list
