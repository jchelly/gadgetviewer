#include "../../config.h"
#define SETSLIDERVALUE_F90 FC_FUNC (setslidervalue, SETSLIDERVALUE)
#include "gtk/gtk.h"
#include "set_event_handler.h"

/* Set the current value of a slider widget */

void SETSLIDERVALUE_F90(GtkWidget **slider, double *value)
{
  gdouble val = (gdouble) (*value);
  ignore_events = 1;
  gtk_range_set_value(GTK_RANGE(*slider), val);
  ignore_events = 0;
}
