#include "../../config.h"
#define FULLSCREEN_F90 FC_FUNC (fullscreen, FULLSCREEN)
#include "gtk/gtk.h"
#include "gdk/gdk.h"

void FULLSCREEN_F90(GtkWidget **window, int *flag)
{
  if(*flag != 0)
    {
      gtk_window_fullscreen(GTK_WINDOW(*window));
    }
  else
    {
      gtk_window_unfullscreen(GTK_WINDOW(*window));
    }
}
  
