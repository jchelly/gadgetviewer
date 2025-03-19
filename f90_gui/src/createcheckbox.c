#include "../../config.h"
#define CREATECHECKBOX_F90 FC_FUNC (createcheckbox, CREATECHECKBOX)
#include "gtk/gtk.h"
#include "pack_box.h"
#include "set_event_handler.h"

void checkbox_event( GtkWidget *widget, gpointer data)
{
  int *iptr;
  if(ignore_events != 0)return;
  iptr = (int *) data;
  *iptr = 1;
  if(event_handler != NULL) (*event_handler)();
  return;
}

void CREATECHECKBOX_F90(GtkWidget **checkbox, GtkWidget **box, int *changed,
		     char *label)
{
  *checkbox = gtk_check_button_new_with_label((const gchar *) label);
  pack_box(*box, *checkbox);

  g_signal_connect (GTK_WIDGET (*checkbox), "toggled",
                    G_CALLBACK(checkbox_event), (gpointer) changed);
}
