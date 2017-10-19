#include "../../config.h"
#define CREATESPINBUTTON_F90 FC_FUNC (createspinbutton, CREATESPINBUTTON)
#include "gtk/gtk.h"
#include "pack_box.h"
#include "set_event_handler.h"

gboolean spinbutton_event( GtkWidget *widget, gpointer data)
{
  int *iptr;

  if(ignore_events != 0)
    {
      iptr = (int *) data;
      *iptr = 0;
      return TRUE;
    }
  else
    {
      iptr = (int *) data;
      *iptr = 1;
      if(event_handler != NULL) (*event_handler)();
    }

  return TRUE;
}

void CREATESPINBUTTON_F90(GtkWidget **spinbutton, GtkWidget **box,
		       double *min, double *max, double *step, int *changed)
{
  GtkAdjustment *adjustment;
  *spinbutton = gtk_spin_button_new_with_range(*min, *max, *step);
  pack_box(*box, *spinbutton);

  adjustment =
    gtk_spin_button_get_adjustment(GTK_SPIN_BUTTON(*spinbutton));
  
  gtk_signal_connect (GTK_OBJECT (adjustment), "value_changed",
		      GTK_SIGNAL_FUNC (spinbutton_event), (gpointer) changed);
}
