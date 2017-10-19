#include "../../config.h"
#define GETSLIDERVALUE_F90 FC_FUNC (getslidervalue, GETSLIDERVALUE)
#include "gtk/gtk.h"

/* Get the current value of a slider widget */

void GETSLIDERVALUE_F90(GtkWidget **slider, double *value)
{
  gdouble val;

  val = gtk_range_get_value(GTK_RANGE(*slider));
  *value = (double) val;
}
