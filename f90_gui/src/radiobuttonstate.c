#include "../../config.h"
#define RADIOBUTTONSTATE_F90 FC_FUNC (radiobuttonstate, RADIOBUTTONSTATE)
#include "gtk/gtk.h"

void RADIOBUTTONSTATE_F90(GtkWidget **radiobutton, int *state)
{
  if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(*radiobutton)))
    {
      *state = 1;
    }
  else
    {
      *state = 0;
    }
}
