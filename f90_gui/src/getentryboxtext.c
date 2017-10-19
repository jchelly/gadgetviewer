#include "../../config.h"
#define GETENTRYBOXTEXT_F90 FC_FUNC (getentryboxtext, GETENTRYBOXTEXT)
#include "gtk/gtk.h"
#include "c_to_fortran.h"

void GETENTRYBOXTEXT_F90(GtkWidget **entrybox, int *string_length, char *text)
{
  gchar *widget_text;
  
  widget_text = (gchar *) gtk_entry_get_text(GTK_ENTRY(*entrybox));
  c_to_fortran((char *) widget_text, text, *string_length);
}
