#include "../../config.h"
#define ADDRADIOBUTTON_F90 FC_FUNC (addradiobutton, ADDRADIOBUTTON)
#include "../../config.h"
#define NEWRADIOBUTTONGROUP_F90 FC_FUNC (newradiobuttongroup, NEWRADIOBUTTONGROUP)
#include "gtk/gtk.h"
#include "pack_box.h"
#include "set_event_handler.h"

void radiobutton_event( GtkWidget *widget, gpointer data)
{
  int *iptr;

  if(ignore_events != 0)return;

  iptr = (int *) data;
  *iptr = 1;

  if(event_handler != NULL) (*event_handler)();
  return;

}

void NEWRADIOBUTTONGROUP_F90(GtkWidget **radiobutton, GtkWidget **box,
			  char *label, int *changed)
{
  *radiobutton = gtk_radio_button_new_with_label( NULL, (gchar *) label);
  pack_box(*box, *radiobutton);

  gtk_signal_connect (GTK_OBJECT (*radiobutton), "toggled",
		      GTK_SIGNAL_FUNC (radiobutton_event), (gpointer) changed);
}


void ADDRADIOBUTTON_F90(GtkWidget **radiobutton, GtkWidget **box, char *label,
		     GtkWidget **prevbutton, int *changed)
{
  *radiobutton = gtk_radio_button_new_with_label(
		     gtk_radio_button_get_group(GTK_RADIO_BUTTON(*prevbutton)),
		     (gchar *) label);
  pack_box(*box, *radiobutton);

  gtk_signal_connect (GTK_OBJECT (*radiobutton), "toggled",
		      GTK_SIGNAL_FUNC (radiobutton_event), (gpointer) changed);
}
