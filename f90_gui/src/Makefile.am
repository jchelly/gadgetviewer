AUTOMAKE_OPTIONS = foreign
AM_CFLAGS        = @GTK3_CFLAGS@ -I$(srcdir)/../../f90_util/src/
AM_FCFLAGS       = $(ax_cv_f90_modflag)../../f90_util/src/

noinst_LTLIBRARIES      = libf90_gui.la

@ax_mod_deps_f90_gui@

libf90_gui_la_SOURCES = set_event_handler.c startgtk.c iterategtk.c setsensitive.c destroywindow.c createwindow.c showwindow.c createbox.c createbutton.c hidewindow.c createlabel.c createentrybox.c getentryboxtext.c setentryboxtext.c packingmode.c pack_box.c createcheckbox.c checkboxstate.c setcheckboxstate.c createslider.c getslidervalue.c setslidervalue.c setstatusbar.c createmenu.c createmenuitem.c radiobutton.c radiobuttonstate.c createspinbutton.c getspinbuttonvalue.c menuitemstate.c setmenuitemstate.c setradiobuttonstate.c setspinbuttonvalue.c createseparator.c createdrawingarea.c drawimage.c createdialog.c selectfile.c createimage.c drawtext.c createcombobox.c createcolourbutton.c createprogress.c setvisible.c fullscreen.c createnotebook.c addaccel.c createtextview.c f90_gui.F90
libf90_gui_la_LIBADD = @GTK3_LIBS@ ../../f90_util/src/libf90_util.la

noinst_HEADERS = set_event_handler.h pack_box.h  packingmode.h

#nodist_include_HEADERS = f90_gui.$(ax_cv_f90_modext)

