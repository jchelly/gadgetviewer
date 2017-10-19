#include "../../config.h"
#define SETRADIOBUTTONSTATE_F90 FC_FUNC (setradiobuttonstate, SETRADIOBUTTONSTATE)
#include "gtk/gtk.h"
#include "set_event_handler.h"


void SETRADIOBUTTONSTATE_F90(GtkWidget **radiobutton, int *state)
{
  ignore_events = 1;
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(*radiobutton), (*state)==1);
  ignore_events = 0;
}
