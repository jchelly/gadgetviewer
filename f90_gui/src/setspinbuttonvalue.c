#include "../../config.h"
#define SETSPINBUTTONVALUE_F90 FC_FUNC (setspinbuttonvalue, SETSPINBUTTONVALUE)
#include "gtk/gtk.h"
#include "set_event_handler.h"

/* Set the current value of a spinbutton widget */
void SETSPINBUTTONVALUE_F90(GtkWidget **spinner, double *value)
{
  gdouble val = (gdouble) (*value);

  ignore_events = 1;
  gtk_spin_button_set_value(GTK_SPIN_BUTTON(*spinner), val);
  ignore_events = 0;

}
