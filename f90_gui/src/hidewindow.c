#include "../../config.h"
#define HIDEWINDOW_F90 FC_FUNC (hidewindow, HIDEWINDOW)
#include <gtk/gtk.h>

void HIDEWINDOW_F90(void **win)
{
  gtk_widget_hide((GtkWidget *) (*win));
}
