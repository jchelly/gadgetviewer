module partial_read_info

  implicit none
  private
  save

  public :: read_info, have_full_snapshot
  type read_info
     logical            :: just_this_file
     logical            :: do_sampling
     real               :: sample_rate
     logical            :: do_sphere
     real               :: radius
     real, dimension(3) :: pos
     logical            :: ignore_missing_mass
     character(len=1000) :: extra_dataset_names
  end type read_info

contains

  logical function have_full_snapshot(rinfo)

    type (read_info) :: rinfo

    have_full_snapshot = &
         (.not.rinfo%just_this_file).and.&
         (.not.rinfo%do_sampling).and.&
         (.not.rinfo%do_sphere)

    return
  end function have_full_snapshot

end module partial_read_info
