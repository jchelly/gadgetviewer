#include "../../config.h"
#define CHECKBOXSTATE_F90 FC_FUNC (checkboxstate, CHECKBOXSTATE)
#include "gtk/gtk.h"

void CHECKBOXSTATE_F90(GtkWidget **checkbox, int *ichecked)
{
  if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(*checkbox)))
    {
      *ichecked = 1;
    }
  else
    {
      *ichecked = 0;
    }
}
