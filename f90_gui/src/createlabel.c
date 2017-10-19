#include "../../config.h"
#define CREATELABEL_F90 FC_FUNC (createlabel, CREATELABEL)
#include "gtk/gtk.h"
#include "pack_box.h"

void CREATELABEL_F90(GtkWidget **label, GtkWidget **box, char *text)
{
  *label = gtk_label_new(text);
  pack_box(*box, *label);
}
