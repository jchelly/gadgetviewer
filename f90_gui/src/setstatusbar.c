#include "../../config.h"
#define SETSTATUSBAR_F90 FC_FUNC (setstatusbar, SETSTATUSBAR)
#include "gtk/gtk.h"

void SETSTATUSBAR_F90(GtkWidget **statusbar, char *text, int *contextid)
{
  gtk_statusbar_pop(GTK_STATUSBAR(*statusbar), (guint) *contextid);

  gtk_statusbar_push(GTK_STATUSBAR(*statusbar), (guint) *contextid,
		     (gchar *) text);
}
