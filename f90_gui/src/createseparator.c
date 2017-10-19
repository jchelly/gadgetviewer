#include "../../config.h"
#define CREATESEPARATOR_F90 FC_FUNC (createseparator, CREATESEPARATOR)
#include "gtk/gtk.h"
#include "pack_box.h"

void CREATESEPARATOR_F90(GtkWidget **separator, GtkWidget **box, int *vertical)
{
  if(*vertical==0)
    *separator = gtk_hseparator_new();
  else
    *separator = gtk_vseparator_new();      
 
  pack_box(*box, *separator);
}
