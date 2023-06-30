#include "../../config.h"
#define CREATENOTEBOOK_F90 FC_FUNC (createnotebook, CREATENOTEBOOK)
#define CREATENOTEBOOKPAGE_F90 FC_FUNC (createnotebookpage, CREATENOTEBOOKPAGE)
#define SETNOTEBOOKPAGE_F90 FC_FUNC (setnotebookpage, SETNOTEBOOKPAGE)
#include "gtk/gtk.h"
#include "pack_box.h"

/*
  Create a notebook widget and pack it in the specified box
*/

void CREATENOTEBOOK_F90(GtkWidget **notebook, GtkWidget **inbox)
{
  *notebook = gtk_notebook_new();
  pack_box(*inbox, *notebook);
}

void CREATENOTEBOOKPAGE_F90(GtkWidget **notebook, GtkWidget **box, char *label)
{
  GtkWidget *tab_label = gtk_label_new((gchar *) label);
  *box = gtk_box_new(GTK_ORIENTATION_VERTICAL,0);
  gtk_notebook_append_page(GTK_NOTEBOOK(*notebook), *box, tab_label);
}

void SETNOTEBOOKPAGE_F90(GtkWidget **notebook, int *page)
{
  gtk_notebook_set_current_page(GTK_NOTEBOOK(*notebook), (gint) (*page));
}
