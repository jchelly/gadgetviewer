#include "../../config.h"
#define SHOWWINDOW_F90 FC_FUNC (showwindow, SHOWWINDOW)
#include <gtk/gtk.h>

void SHOWWINDOW_F90(void **win)
{
  gtk_widget_show_all((GtkWidget *) (*win));
}

