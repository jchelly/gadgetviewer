#include "../../config.h"
#define REMOVELINE_F90 FC_FUNC (removeline, REMOVELINE)
#include "../../config.h"
#define COMBOBOXSETINDEX_F90 FC_FUNC (comboboxsetindex, COMBOBOXSETINDEX)
#include "../../config.h"
#define COMBOBOXGETINDEX_F90 FC_FUNC (comboboxgetindex, COMBOBOXGETINDEX)
#include "../../config.h"
#define COMBOBOXADDTEXT_F90 FC_FUNC (comboboxaddtext, COMBOBOXADDTEXT)
#include "../../config.h"
#define CREATECOMBOBOX_F90 FC_FUNC (createcombobox, CREATECOMBOBOX)
#include "gtk/gtk.h"
#include "pack_box.h"
#include "set_event_handler.h"

void combobox_event( GtkWidget *widget, gpointer data)
{
  int *iptr;

  if(ignore_events != 0)return;

  iptr = (int *) data;
  *iptr = 1;
  if(event_handler != NULL) (*event_handler)();
  return;
}


void CREATECOMBOBOX_F90(GtkWidget **combobox, GtkWidget **box, int *clicked)
{
  *combobox = gtk_combo_box_new_text();
  pack_box(*box, *combobox);

  gtk_signal_connect (GTK_OBJECT (*combobox), "changed",
		      GTK_SIGNAL_FUNC (combobox_event), (gpointer) clicked);

}

void COMBOBOXADDTEXT_F90(GtkWidget **combobox, char *str)
{
  ignore_events = 1;
  gtk_combo_box_append_text(GTK_COMBO_BOX(*combobox), (gchar *) str);
  gtk_combo_box_set_active(GTK_COMBO_BOX(*combobox), 0);
  ignore_events = 0;
}

void COMBOBOXGETINDEX_F90(GtkWidget **combobox, int *i)
{
  *i = (int) gtk_combo_box_get_active(GTK_COMBO_BOX(*combobox));
}

void COMBOBOXSETINDEX_F90(GtkWidget **combobox, int *i)
{
  ignore_events = 1;
  gtk_combo_box_set_active(GTK_COMBO_BOX(*combobox), (gint) (*i));
  ignore_events = 0;
}

void REMOVELINE_F90(GtkWidget **combobox, int *i)
{
  ignore_events = 1;
  gtk_combo_box_remove_text( GTK_COMBO_BOX(*combobox), (gint) (*i));
  ignore_events = 0;
}

