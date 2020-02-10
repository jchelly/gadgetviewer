program gadgetviewer
!
! Program to visualise Gadget simulation outputs
!
#include "../../config.h"
  use f90_gui
  use main_window
  use particle_store
  use plotter
  use colour_table
  use snapshot_reader
  use return_status
  use view_parameters
  use selection
  use f90_util
  use data_types
  use configuration
  use command_line
  use settings
  use movie
  use read_partial
  use partial_read_info
  use sampling

  implicit none
  type(result_type)           :: res
  character(len=fname_maxlen) :: snapshot_file, config_file, selection_file
  integer :: isnap, hdferr
  logical :: batch_mode
  logical :: partial
  type(read_info) :: rinfo

  ! Initialise modules
  call particle_store_init(pdata)
  call particle_store_init(psample)
  call colour_table_init()
  call selection_init()

  ! Initialise random number generator
  call random_seed()

  ! Read configuration info that's stored in separate files
  ! (colour tables, I/O settings and mouse buttons)
  call configuration_read()

  ! Get command line arguments
  call command_line_options(snapshot_file, config_file, batch_mode, &
       selection_file, partial, rinfo)

  ! Create the main window if necessary
  if(.not.batch_mode)then
     call gui_init()
     call main_window_create()
  endif

  ! Try to read the snapshot file
  if(len_trim(snapshot_file).gt.0.and.(.not.partial))then
     res = snapshot_open_unknown(snapshot_file,isnap,rinfo)
     if(res%success)then
        res = snapshot_read(isnap)
        if(.not.res%success)then
           write(0,*)'Failed to read file '//trim(snapshot_file)//":"
           write(0,*)trim(res%string)
           call terminate()
        endif
     else
        write(0,*)'Failed to read file '//trim(snapshot_file)//":"
        write(0,*)trim(res%string)
        call terminate()
     endif
  endif
  if(.not.batch_mode)call read_partial_set_filename(snapshot_file,&
       rinfo)

  ! Initialise plotting routines
  call plotter_init(new_snapshot=.false.)

  ! Any saved default settings override plotter defaults
  res = load_settings(default_file)

  ! Set default view parameters
  call view_parameters_initialise()

  ! Try to read any configuration file specified on the command line
  if(len_trim(config_file).gt.0)then
     res = load_settings(config_file)
     if(.not.res%success)then
        write(0,*)"Unable to read configuration file ",trim(config_file)
        call terminate()
     else
        ! Resample to apply new parameters
        res = sample_region(keep_coords=.true.)
        if(.not.res%success)then
           write(0,*)"Unable to sample particles"
           call terminate()
        endif
     endif
  endif
  
  ! Try to read selected particles specified on the command line
  if(len_trim(selection_file).gt.0)then
     res = selection_restore_state(selection_file)
     if(.not.res%success)then
        write(0,*)"Unable to read selected particles: ",trim(selection_file)
        call terminate()
     else
        res = selection_apply_all(psample)
        if(.not.res%success)then
           write(0,*)"Unable to apply selection: ",trim(res%string)
           call terminate()
        endif
     endif
  endif

  if(.not.batch_mode)then
     ! Interactive mode
     call main_window_update_controls()
     ! Set the routine to be called when an event occurs
     call gui_set_event_handler(main_window_process_events)
     ! Redraw in case we just loaded a snapshot
     call main_window_redraw()
     ! Open partial snapshot read GUI if option was used
     if(partial)call read_partial_open(mainwin)
     ! Main loop processes events until the program is terminated
     call gui_main_loop()
  else
     ! Batch mode for movie making
     res = movie_make()
     if(.not.res%success)then
        write(0,*)"Error while making movie: "
        write(0,*)trim(res%string)
        call terminate()
     endif
  endif

  ! Deallocate memory
  call particle_store_empty(pdata)
  call particle_store_empty(psample)

  if(.not.batch_mode)call main_window_finalise()

end program gadgetviewer
