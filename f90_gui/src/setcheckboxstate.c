#include "../../config.h"
#define SETCHECKBOXSTATE_F90 FC_FUNC (setcheckboxstate, SETCHECKBOXSTATE)
#include "gtk/gtk.h"
#include "set_event_handler.h"

void SETCHECKBOXSTATE_F90(GtkWidget **checkbox, int *ichecked)
{
  ignore_events = 1;
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(*checkbox), (*ichecked==1));
  ignore_events = 0;
}
