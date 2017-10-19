#include "../../config.h"
#define CLEARALLEVENTS_F90 FC_FUNC (clearallevents, CLEARALLEVENTS)
#define ITERATEGTK_F90 FC_FUNC (iterategtk, ITERATEGTK)
#define GTKMAINLOOP_F90 FC_FUNC (gtkmainloop, GTKMAINLOOP)
#define GTKQUIT_F90 FC_FUNC (gtkquit, GTKQUIT)
#include "gtk/gtk.h"

void ITERATEGTK_F90(void)
{
  /* gtk_main_iteration(); */
  while(gtk_events_pending())
    gtk_main_iteration();
}

void CLEARALLEVENTS_F90(void)
{
  while(gtk_events_pending())
    gtk_main_iteration();
}

void GTKMAINLOOP_F90(void)
{
  gtk_main();
}

void GTKQUIT_F90(void)
{
  gtk_main_quit();
}
