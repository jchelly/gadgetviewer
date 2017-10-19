module graph
!
! Module for drawing histograms and scatterplots of the particle
! properties. Uses PLPlot or Cairo depending on what's available.
!
#include "../../config.h"
  use f90_gui
  use particle_store
  use string_module
  use data_types
  use selection
  use f90_util
  use drawing
  use math

  implicit none
  private
  save

  public :: graph_open
  public :: graph_close
  public :: graph_process_events
  public :: graph_update
  public :: graph_plot
  public :: graph_set_keys
  public :: graph_get_keys

  ! Format string for range entry boxes
  character(len=9)   :: rfmt = "(1es10.3)"
  integer, parameter :: box_size = 10

  ! Widgets
  logical                 :: window_open = .false.
  type (gui_window)       :: window
  type (gui_drawing_area) :: drawing_area
  type (gui_radio_button) :: hist_button, scatter_button
  type (gui_combo_box)    :: xprop_box, yprop_box
  type (gui_entrybox)     :: xmin_box, xmax_box
  type (gui_entrybox)     :: ymin_box, ymax_box
  type (gui_checkbox)     :: xlog_box, ylog_box
  type (gui_combo_box)    :: species_box
  type (gui_box)          :: nbins_hbox
  type (gui_entrybox)     :: nbins_box
  type (gui_label)        :: nbins_label
  type (gui_menu)         :: file_menu
  type (gui_menu_item)    :: file_postscript
  type (gui_menu_item)    :: file_png
  type (gui_button)       :: autoscale_button

  ! Image array
  character(len=1), dimension(:), pointer :: image => null()
  integer :: width, height

  ! Plot settings
  integer            :: ispecies = 1
  logical            :: is_hist  = .true.
  integer            :: ipropx = 1, ipropy = 1
  real, dimension(2) :: xrange, yrange
  real, dimension(2) :: frequency_range
  logical            :: xlog = .true., ylog = .true.
  integer            :: nbins = 20

  ! Maximum value from last histogram plotted
  integer :: hist_ymax

  ! Colour map
  integer, dimension(:,:), allocatable :: cmap

contains

  subroutine graph_open(mainwin)
!
! Open the graph window
!    
    implicit none
    type (gui_window) :: mainwin
    type (gui_box)    :: vbox, hbox, xbox, ybox
    type (gui_label)  :: label
    integer           :: nspecies
    integer(kind=index_kind), dimension(MAXSPECIES) :: np
    integer :: istat

    if(window_open.or.(.not.particle_store_loaded(psample)))return

    ! Set default packing mode
    call gui_packing_mode(expand=.false., fill=.false., spacing=3, &
         position=gui_start)

    ! Create the window
    call gui_create_window(window, "Plot window", parent=mainwin, &
         menubar=.true.)

    ! Add File menu
    call gui_create_menu(file_menu, window, "File")
    call gui_create_menu_item(file_postscript, file_menu, &
         "Save plot as postscript...")
    call gui_create_menu_item(file_png, file_menu, &
         "Save plot as PNG...")

    call gui_packing_mode(expand=.true., fill=.true.)
    call gui_create_box(vbox, window, gui_vertical)
    call gui_packing_mode(expand=.false., fill=.false.)
    ! Top hbox with plot type selector
    call gui_create_box(hbox, vbox, gui_horizontal)
    call gui_create_radio_button(hist_button, hbox, "Histogram")
    call gui_create_radio_button(scatter_button, hbox, "Scatterplot", &
         previous=hist_button)
    ! Particle type
    call gui_packing_mode(position=gui_end)
    call gui_create_button(autoscale_button, hbox, "Rescale")
    call gui_create_combo_box(species_box, hbox, (/"<particle_type>"/))
    call gui_create_label(label, hbox, "Particle type:")
    call gui_packing_mode(position=gui_start)

   ! Graphics window
    call gui_packing_mode(expand=.true., fill=.true.)
    call gui_create_drawing_area(drawing_area, vbox, width=400, height=300)
    call gui_packing_mode(expand=.false., fill=.false.)
    ! X axis controls
    call gui_create_box(xbox, vbox, gui_horizontal)
    call gui_create_label(label, xbox, "x axis: ")
    call gui_packing_mode(expand=.true., fill=.true.)
    call gui_create_combo_box(xprop_box, xbox, (/"<property>"/))
    call gui_packing_mode(expand=.false., fill=.false.)
    call gui_create_label(label, xbox, " Range: ")
    call gui_create_entrybox(xmin_box, xbox, box_size)
    call gui_create_label(label, xbox, " to ")
    call gui_create_entrybox(xmax_box, xbox, box_size)
    call gui_create_checkbox(xlog_box, xbox, "Log")
    ! Y axis controls
    call gui_create_box(ybox, vbox, gui_horizontal)
    call gui_create_label(label, ybox, "y axis: ")
    call gui_packing_mode(expand=.true., fill=.true.)
    call gui_create_box(nbins_hbox, ybox, gui_horizontal)
    call gui_packing_mode(expand=.false., fill=.false.)
    call gui_create_label(nbins_label, nbins_hbox, "Bins:")
    call gui_create_entrybox(nbins_box, nbins_hbox, 3)
    call gui_packing_mode(expand=.true., fill=.true.)
    call gui_create_combo_box(yprop_box, ybox, (/"<property>"/))
    call gui_packing_mode(expand=.false., fill=.false.)
    call gui_create_label(label, ybox, " Range: ")
    call gui_create_entrybox(ymin_box, ybox, box_size)
    call gui_create_label(label, ybox, " to ")
    call gui_create_entrybox(ymax_box, ybox, box_size)
    call gui_create_checkbox(ylog_box, ybox, "Log")

    call gui_show_window(window)
    window_open = .true.

    if(associated(image))then
       deallocate(image)
    endif
    call gui_drawing_area_get_size(drawing_area,width,height)
    allocate(image(0:3*width*height-1), stat=istat)

    ! Try to ensure that graph window displays something
    ! when first opened. Pick a particle type with np>0.
    call particle_store_contents(psample, get_np=np, get_nspecies=nspecies)
    ispecies = min(max(1,ispecies),nspecies)
    if(np(ispecies).eq.0)then
       do while(np(ispecies).eq.0.and.ispecies.lt.nspecies)
          ispecies = ispecies + 1
       end do
       if(np(ispecies).eq.0)ispecies = 1
    endif

    call graph_default_range()
    call graph_update()

    return
  end subroutine graph_open


  subroutine graph_close()
!
! Open the graph window
!    
    implicit none

    if(.not.window_open)return

    call gui_destroy_window(window)
    window_open = .false.

    return
  end subroutine graph_close


  subroutine graph_update()
!
! Update the graph
!
    implicit none

    if(.not.window_open)return

    ! Can't do anything if there's no particle data
    if(.not.particle_store_loaded(psample))then
       call graph_close()
       return
    endif

    ! Make sure widgets with ranges etc are up to date
    call graph_update_controls()
    ! Draw the graph on the image array
    if(associated(image))then
       call graph_plot(image,width,height)
       ! Copy the image to the drawing area's backing store
       call gui_draw_image(drawing_area,image,width,height,0,0)
       ! Update the drawing area on the screen
       call gui_drawing_area_redraw(drawing_area)
    endif

    return
  end subroutine graph_update


  subroutine graph_process_events()
!
! Process events in the graph window
!
    implicit none
    logical :: need_redraw
    character(len=500) :: str, fname
    integer :: ios, i
    logical :: ok
    character(len=20) :: bt
    integer :: istat

    need_redraw = .false.

    if(.not.window_open)return

    ! Redraw if the window is resized. Also ensure image array is the
    ! right size
    if(gui_drawing_area_resized(drawing_area))then
       call gui_drawing_area_get_size(drawing_area,width,height)
       if(.not.associated(image))then
          allocate(image(0:3*width*height-1), stat=istat)
       else
          if(size(image).lt.3*width*height)then
             deallocate(image)
             allocate(image(0:3*width*height-1), stat=istat)
          endif
       endif
       need_redraw = .true.
    else
       if(.not.associated(image))then
          allocate(image(0:3*width*height-1), stat=istat)
       endif
    endif

    ! Can't do anything if there's no particle data
    if(.not.particle_store_loaded(psample))then
       call graph_close()
       return
    endif

    ! Close window if close box clicked
    if(gui_window_closed(window))then
       call graph_close()
       return
    endif

    ! Reset plot limits if rescale button is clicked
    if(gui_button_clicked(autoscale_button))then
       call graph_default_range()
       need_redraw = .true.
       if(is_hist)then
          yrange(2) = hist_ymax*1.1
       endif
    endif

    ! Check if plot type is changed
    if(gui_radio_button_changed(hist_button))then
       call gui_radio_button_get_state(hist_button, is_hist)
       call graph_default_range()
       need_redraw = .true.
    endif

    ! Check if particle type is changed
    if(gui_combo_box_changed(species_box))then
       call gui_combo_box_get_index(species_box,ispecies)
       call graph_default_range()
       need_redraw = .true.
    endif

    ! Check if properties to plot have changed
    if(gui_combo_box_changed(xprop_box))then
       call gui_combo_box_get_index(xprop_box, ipropx)
       call graph_default_range()
       need_redraw = .true.
    endif
    if(gui_combo_box_changed(yprop_box))then
       call gui_combo_box_get_index(yprop_box, ipropy)
       call graph_default_range()
       need_redraw = .true.
    endif

    ! Check if x/y axis ranges are changed
    if(gui_entrybox_changed(xmin_box))then
       call gui_entrybox_get_value(xmin_box, xrange(1), ios)
       need_redraw = .true.
    endif
    if(gui_entrybox_changed(xmax_box))then
       call gui_entrybox_get_value(xmax_box, xrange(2), ios)
       need_redraw = .true.
    endif
    if(gui_entrybox_changed(ymin_box))then
       if(is_hist)then
          call gui_entrybox_get_value(ymin_box, frequency_range(1), ios)
       else
          call gui_entrybox_get_value(ymin_box, yrange(1), ios)
       endif
       need_redraw = .true.
    endif
    if(gui_entrybox_changed(ymax_box))then
       if(is_hist)then
          call gui_entrybox_get_value(ymax_box, frequency_range(2), ios)
       else
          call gui_entrybox_get_value(ymax_box, yrange(2), ios)
       endif
       need_redraw = .true.
    endif

    ! Check if log boxes change
    if(gui_checkbox_changed(xlog_box))then
       call gui_checkbox_get_state(xlog_box, xlog)
       need_redraw = .true.
    endif
    if(gui_checkbox_changed(ylog_box))then
       call gui_checkbox_get_state(ylog_box, ylog)
       need_redraw = .true.
    endif

    ! Check if number of bins changes
    if(gui_entrybox_changed(nbins_box))then
       call gui_entrybox_get_text(nbins_box, str)
       read(str,*,iostat=ios)i
       if(ios.eq.0.and.i.gt.0.and.i.lt.1000)nbins=i
       need_redraw = .true.
    endif

    ! Redraw if necessary
    if(need_redraw)call graph_update()

    ! Screenshot option
    if(gui_menu_item_clicked(file_png))then
       if(associated(image))then
          call gui_select_file(window, &
               "Save plot as PNG file", &
               gui_file_save, ok, fname)
          if(ok)then
             ! Image should already have been generated
             if(.not.associated(image))stop &
                  'Image not allocated when writing out screenshot'
             if(write_png(fname,width,height,image))then
                ! Success
                bt=gui_display_dialog(window,"info", &
                     "Wrote file "//trim(fname))
             else
                ! Failure
                bt=gui_display_dialog(window,"error", &
                     "Unable to write file "//trim(fname))
             endif
          endif
       else
          bt=gui_display_dialog(window,"error", &
               "Unable to allocate memory") 
       endif
    endif

    ! Postscript option
    if(gui_menu_item_clicked(file_postscript))then
       call gui_select_file(window, &
            "Save plot as postscript file", &
            gui_file_save, ok, fname)
       if(ok)then
          ! Check location is writable first because PlPlot doesn't
          ! provide any return status
          open(unit=1,file=fname,status='unknown',form='unformatted',&
               iostat=ios)
          if(ios.eq.0)then
             close(1)
             call graph_plot(image,width,height,psfile=fname)
             bt=gui_display_dialog(window,"info", &
                  "Wrote file "//trim(fname))
          else
             bt=gui_display_dialog(window,"error", &
                  "Unable to write file "//trim(fname))
          endif
       endif
    endif

    return
  end subroutine graph_process_events


  subroutine graph_update_controls()
!
! Update the widgets in the graph window using the module variables
!
    implicit none
    character(len=maxlen), dimension(maxprops) :: propnames
    integer :: nprops, nspecies
    character(len=maxlen), dimension(maxspecies) :: species_names

    call gui_radio_button_set_state(hist_button, is_hist)
    call gui_radio_button_set_state(scatter_button, .not.is_hist)

    if(is_hist)then
       ! Histogram, so only need one property
       call gui_set_visible(nbins_hbox, .true.)
       call gui_set_visible(yprop_box, .false.)
    else
       ! Scatterplot, so need two properties
       call gui_set_visible(nbins_hbox, .false.)
       call gui_set_visible(yprop_box, .true.)      
    endif
    
    ! Set species names box text
    call particle_store_contents(psample, get_nspecies=nspecies, &
         get_species_names=species_names)
    call gui_combo_box_set_text(species_box, species_names(1:nspecies))

    ! Set selected species
    if(ispecies.gt.nspecies)ispecies = nspecies
    if(ispecies.lt.1)ispecies = 1
    call gui_combo_box_set_index(species_box, ispecies)

    ! Put available property names in the x/y axis combo boxes
    call particle_store_species(psample, ispecies, get_propnames=propnames, &
         get_nprops=nprops)
    call gui_combo_box_set_text(xprop_box, propnames(1:nprops))
    call gui_combo_box_set_text(yprop_box, propnames(1:nprops))

    ! Set selected properties
    if(ipropx.le.nprops) &
         call gui_combo_box_set_index(xprop_box, ipropx)
    if(ipropy.le.nprops) &
         call gui_combo_box_set_index(yprop_box, ipropy)

    ! Set log tick boxes
    call gui_checkbox_set_state(xlog_box, xlog)
    call gui_checkbox_set_state(ylog_box, ylog)

    ! Set axis ranges
    call gui_entrybox_set_text(xmin_box, string(xrange(1),fmt=rfmt))
    call gui_entrybox_set_text(xmax_box, string(xrange(2),fmt=rfmt))
    if(is_hist)then
       call gui_entrybox_set_text(ymin_box,string(frequency_range(1),fmt=rfmt))
       call gui_entrybox_set_text(ymax_box,string(frequency_range(2),fmt=rfmt))
    else
       call gui_entrybox_set_text(ymin_box, string(yrange(1),fmt=rfmt))
       call gui_entrybox_set_text(ymax_box, string(yrange(2),fmt=rfmt))
    endif

    ! Set number of bins
    call gui_entrybox_set_text(nbins_box, string(nbins,fmt='(1i3)'))

    return
  end subroutine graph_update_controls


  subroutine graph_default_range() 
!
! Set default values for the range
!
    implicit none
    integer :: nspecies, nprops
    integer(kind=index_kind) :: np

    call particle_store_contents(pdata, get_nspecies=nspecies)
    if(ispecies.le.nspecies.and.ispecies.gt.0)then
       call particle_store_species(psample, ispecies, get_nprops=nprops)
       if(ipropx.le.nprops.and.ipropx.gt.0)then
          call particle_store_property(pdata, ispecies, ipropx, &
               get_range=xrange)
       endif
       if(is_hist)then
          call particle_store_contents(pdata, get_nspecies=nspecies)
          if(ispecies.gt.nspecies)ispecies=nspecies
          if(ispecies.lt.1)ispecies=1
          call particle_store_species(psample, ispecies, get_np=np)
          if(np.gt.0)then
             frequency_range = (/ 0.1, (5.0*real(np)/real(nbins)) /)
          else
             frequency_range = (/ 0.0, 0.0 /)
          endif
       else
          if(ipropy.le.nprops.and.ipropy.gt.0)then
             call particle_store_property(pdata, ispecies, ipropy, &
                  get_range=yrange)
          endif
       endif
    endif

    ! Make sure range in x is not zero
    if(xrange(1).eq.xrange(2))then
       xrange(1) = xrange(1) * 0.9
       xrange(2) = xrange(2) * 1.1
    endif

    ! Make sure range in y is not zero
    if(yrange(1).eq.yrange(2))then
       yrange(1) = yrange(1) * 0.9
       yrange(2) = yrange(2) * 1.1
    endif

    return
  end subroutine graph_default_range


  subroutine graph_plot(image,width,height,psfile)
    !
    ! Plot a graph
    !
    implicit none
    integer :: width, height
    character(len=1), dimension(:), pointer :: image
    character(len=*), optional :: psfile
    integer :: nspecies, nprops
    character(len=maxlen), dimension(maxspecies) :: species_names
    character(len=maxlen) :: xlabel, ylabel,title
    character(len=maxlen), dimension(maxprops) :: propnames
    real, dimension(2) :: xlimit, ylimit
    ! Data arrays
    integer(kind=index_kind) :: np
    real, dimension(:), allocatable :: xdata, ydata
    integer :: stat
    real(kind=r_prop_kind),    dimension(:), pointer :: rdata
    integer(kind=i_prop_kind), dimension(:), pointer :: idata
    character(len=maxlen) :: type
    integer, dimension(:), pointer :: selected
    integer :: i, j
    ! Histogram data
    real, dimension(nbins) :: xhist, yhist
    ! Selections
    integer :: isel

    ! Selected data
    real, dimension(:), allocatable :: xsel, ysel
    integer :: npsel
    real    :: ypos
    character(len=maxlen), dimension(0:nselmax-1) :: names    
    logical :: axis_ok

    ! Clear the image
    if(associated(image)) &
         image(0:3*width*height-1) = char(0)

    ! Get particle type names
    call particle_store_contents(psample, get_species_names=species_names, &
         get_nspecies=nspecies)

    if(ispecies.gt.nspecies)return

    ! Get property names
    call particle_store_species(psample, ispecies, get_propnames=propnames, &
         get_nprops=nprops)

    ! If necessary data is not loaded, return
    if(ipropx.gt.nprops.or.(ipropy.gt.nprops.and.is_hist))then
       if(.not.present(psfile))call graph_plot_error("Data not loaded")
       return
    endif
       
    ! Check that axis limits are sensible
    axis_ok = .true.
    if(xlog.and.any(xrange.le.0.0))   axis_ok = .false.
    if(abs(xrange(1)-xrange(2)).eq.0) axis_ok = .false.
    if(xrange(2).lt.xrange(1)) axis_ok = .false.
    if(is_hist)then
       if(abs(frequency_range(1)-frequency_range(2)).eq.0) axis_ok = .false.
       if(ylog.and.any(frequency_range.le.0.0))            axis_ok = .false. 
       if(frequency_range(2).lt.frequency_range(1)) axis_ok = .false.
    else
       if(abs(yrange(1)-yrange(2)).eq.0) axis_ok = .false.
       if(ylog.and.any(yrange.le.0.0))   axis_ok = .false.
       if(yrange(2).lt.yrange(1)) axis_ok = .false.
    endif

    if(.not.axis_ok)then
       if(.not.present(psfile))call graph_plot_error("Invalid axis range")
       return
    endif

    ! Get axis labels
    if(xlog)then
       xlabel = "Log("//trim(propnames(ipropx))//")"
    else
       xlabel = trim(propnames(ipropx))
    endif
    if(is_hist)then
       if(ylog)then
          ylabel = "Log(Frequency)"
       else
          ylabel = "Frequency"
       endif
    else
       if(ylog)then
          ylabel = "Log("//trim(propnames(ipropy))//")"
       else
          ylabel = trim(propnames(ipropy))
       endif
    endif

    ! Determine axis limits
    if(xlog)then
       xlimit = real(mylog10(xrange))
    else
       xlimit = real(xrange)
    endif
    if(ylog)then
       if(is_hist)then
          ylimit = real(mylog10(frequency_range))
       else
          ylimit = real(mylog10(yrange))
       endif
    else
       if(is_hist)then
          ylimit = real(frequency_range)
       else
          ylimit = real(yrange)
       endif
    endif

    ! Get number of data points
    call particle_store_species(psample, ispecies, get_np=np)

    ! Allocate storage for data
    allocate(xdata(np), ydata(np), xsel(np), ysel(np), cmap(3,0:nselmax+2), stat=stat)
    if(stat.ne.0)return

    ! Get x data array
    call particle_store_property(psample, ispecies, ipropx, get_type=type)
    select case(type)
    case("REAL")
       call particle_store_property(psample, ispecies, ipropx, get_rdata=rdata)
       xdata = rdata(1:np)
    case("INTEGER")
       call particle_store_property(psample, ispecies, ipropx, get_idata=idata)
       xdata = idata(1:np)
    end select
    if(xlog)then
       where(xdata.gt.0.0)
          xdata = mylog10(xdata)
       elsewhere
          xdata = -1000.0
       end where
    endif

    if(.not.is_hist)then
       ! Get y data array
       call particle_store_property(psample, ispecies, ipropy, get_type=type)
       select case(type)
       case("REAL")
          call particle_store_property(psample, ispecies, ipropy, &
               get_rdata=rdata)
          ydata = rdata(1:np)
       case("INTEGER")
          call particle_store_property(psample, ispecies, ipropy, &
               get_idata=idata)
          ydata = idata(1:np)
       end select
       if(ylog)then
          where(ydata.gt.0.0)
             ydata = mylog10(ydata)
          elsewhere
             ydata = -1000.0
          end where
       endif
    endif

    ! Get selection status
    call particle_store_species(psample, ispecies, get_selected=selected)

    ! Set up colour map
    if(.not.present(psfile))then
       cmap(1:3,0) = (/ 0,   0,   0   /)
       cmap(1:3,1) = (/ 255, 255, 255 /)
    else
       cmap(1:3,1) = (/ 0,   0,   0   /)
       cmap(1:3,0) = (/ 255, 255, 255 /)  
    endif
    cmap(1:3,2) = (/ 255, 0,   0   /)
    do isel = 0, nselmax-1, 1
       cmap(1:3,isel+3) = selection_get_colour(isel)
    end do
    ! Set output device - postscript or memory
    if(.not.present(psfile))then
       if(associated(image))then
          call draw_init_mem(image, width, height, cmap)
       else
          deallocate(cmap)
          return
       endif
    else
       call draw_init_ps(psfile, cmap)
    endif
    deallocate(cmap)

    ! Title for graph
    if(np.gt.0)then
       title = trim(species_names(ispecies))
    else
       title = trim(species_names(ispecies))// &
            " - no particles of this type in the sample"
    endif

    ! Draw axes
    call draw_box(x=0.1, y=0.2, &
         width=0.7, height=0.7, &
         xmin=xlimit(1), xmax=xlimit(2), ymin=ylimit(1), ymax=ylimit(2), &
         title=title, xlabel=xlabel, ylabel=ylabel, &
         colour=1)
    
    if(np.gt.0)then
       if(is_hist)then
          ! Make histogram
          call histogram(nbins, xdata, xlimit, xhist, yhist)
          if(ylog)then
             where(yhist.gt.0.0)
                yhist = mylog10(yhist)
             elsewhere
                yhist = -1000.0
             end where
          endif
          call draw_histogram(xhist, yhist, colour=2)
       else
          ! Make scatterplot
          call draw_scatterplot(xdata, ydata, colour=2)
       endif

       ! Repeat for selected particles only
       do isel = 0, nselmax-1, 1
          if(selection_is_empty(isel))cycle
          if(.not.display_selection(isel))cycle

          j = 0
          do i = 1, np, 1
             if(btest(selected(i),isel))then
                j = j + 1
                xsel(j) = xdata(i)
                ysel(j) = ydata(i)
             endif
          end do
          npsel = j

          if(is_hist)then
             ! Make histogram
             if(npsel.gt.0)then
                call histogram(nbins, xsel(1:npsel), xlimit, xhist, yhist)
                if(ylog)then
                   where(yhist.gt.0.0)
                      yhist = mylog10(yhist)
                   elsewhere
                      yhist = -1000.0
                   end where
                endif
                call draw_histogram(xhist, yhist, &
                     colour=3+isel)
             endif
          else
             ! Make scatterplot
             call draw_scatterplot(xsel(1:npsel), ysel(1:npsel), &
                  colour=3+isel)
          endif
          ! Next selection
       end do
    endif

    ! Add legend
    call selection_get_names(names)
    call draw_text("Legend:", x=0.83, y=0.9, &
         colour=1)
    call draw_text("All particles", x=0.83, y=0.85, &
         colour=2)
    ypos = 0.85
    do isel = 0, nselmax-1, 1
       if(.not.selection_is_empty(isel).and.display_selection(isel))then
          ypos = ypos - 0.05
          call draw_text(trim(names(isel)), &
               x=0.83, y=ypos, &
               colour=3+isel)
       endif
    end do

    ! Finished plotting
    call draw_end()

    deallocate(xdata,ydata,xsel,ysel)

    return

  contains
    
    subroutine graph_plot_error(message)
!
! Report an error in the graph window
!
      implicit none
      character(len=*) :: message
      
      if(associated(image))then
         call draw_init_mem(image, width, height)
         call draw_text(message, 0.1, 0.1)
         call draw_end()
      endif

      return
    end subroutine graph_plot_error

  end subroutine graph_plot


  subroutine histogram(nbins, xdata, xrange, xhist, yhist)
!
! Generate a histogram and return it in ncount
!
    implicit none
    integer :: nbins
    real, dimension(:) :: xdata
    real, dimension(2) :: xrange
    real, dimension(1:nbins) :: xhist, yhist
    integer :: i, j

    ! Return zeros if bins have zero width
    if(xrange(1).eq.xrange(2))then
       xhist = 0.0
       yhist = 0.0
    endif

    yhist = 0
    do i = 1, size(xdata), 1
       j = floor((xdata(i)-xrange(1))/(xrange(2)-xrange(1))*nbins)+1
       if(j.ge.1.and.j.le.nbins)then
          yhist(j) = yhist(j) + 1
       endif
    end do
    do i = 0, nbins-1, 1
       xhist(i+1) = xrange(1) + i*((xrange(2)-xrange(1))/nbins)
    end do

    hist_ymax = maxval(yhist)

    return
  end subroutine histogram


  subroutine graph_set_keys()

    use key_file
    implicit none

    call set_key("Graph","Particle Type", ispecies)
    call set_key("Graph","Histogram",     is_hist)
    call set_key("Graph","X Property",    ipropx)
    call set_key("Graph","Y Property",    ipropy)
    call set_key("Graph","X Range",       xrange)
    call set_key("Graph","Y Range",       yrange)
    call set_key("Graph","Frequency Range", frequency_range)
    call set_key("Graph","Log X",         xlog)
    call set_key("Graph","Log Y",         ylog)
    call set_key("Graph","Bins",          nbins)

    return
  end subroutine graph_set_keys


  subroutine graph_get_keys()

    use key_file
    implicit none

    call get_key("Graph","Particle Type", ispecies)
    call get_key("Graph","Histogram",     is_hist)
    call get_key("Graph","X Property",    ipropx)
    call get_key("Graph","Y Property",    ipropy)
    call get_key("Graph","X Range",       xrange)
    call get_key("Graph","Y Range",       yrange)
    call get_key("Graph","Frequency Range", frequency_range)
    call get_key("Graph","Log X",         xlog)
    call get_key("Graph","Log Y",         ylog)
    call get_key("Graph","Bins",          nbins)

    return
  end subroutine graph_get_keys


end module graph
