#include "../../config.h"
#define CREATEENTRYBOX_F90 FC_FUNC (createentrybox, CREATEENTRYBOX)
#include "gtk/gtk.h"
#include "pack_box.h"
#include "set_event_handler.h"

void entrybox_event( GtkWidget *widget, gpointer data)
{
  int *iptr;

  if(ignore_events != 0)return;

  iptr = (int *) data;
  *iptr = 1;

  if(event_handler != NULL) (*event_handler)();

  return;
}

void CREATEENTRYBOX_F90(GtkWidget **entrybox, GtkWidget **box, int *length,
		     int *changed, int *editable, int *max_chars)
{
  *entrybox = gtk_entry_new();
  if((*length) > 0)
    {
      gtk_entry_set_width_chars(GTK_ENTRY(*entrybox), (gint) (*length));
    }
  if((*max_chars) > 0)
    {
      gtk_entry_set_max_length(GTK_ENTRY(*entrybox),(gint) (*max_chars));
    }

  gtk_editable_set_editable(GTK_EDITABLE(*entrybox), (gboolean) *editable );
  
  pack_box(*box, *entrybox);

  g_signal_connect (GTK_WIDGET (*entrybox), "activate",
                    G_CALLBACK(entrybox_event), (gpointer) changed);
}
