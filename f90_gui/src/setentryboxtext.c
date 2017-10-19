#include "../../config.h"
#define SETENTRYBOXTEXT_F90 FC_FUNC (setentryboxtext, SETENTRYBOXTEXT)
#include "gtk/gtk.h"
#include "set_event_handler.h"

void SETENTRYBOXTEXT_F90(GtkWidget **entrybox, char *text)
{
  ignore_events = 1;
  gtk_entry_set_text(GTK_ENTRY(*entrybox), (const gchar *) text);
  ignore_events = 0;
}
