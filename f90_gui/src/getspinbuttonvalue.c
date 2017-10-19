#include "../../config.h"
#define GETSPINBUTTONVALUE_F90 FC_FUNC (getspinbuttonvalue, GETSPINBUTTONVALUE)
#include "gtk/gtk.h"

/* Get the current value of a spinbutton widget */

void GETSPINBUTTONVALUE_F90(GtkWidget **spinner, double *value)
{
  gdouble val;

  val = gtk_spin_button_get_value(GTK_SPIN_BUTTON(*spinner));
  *value = (double) val;
}
