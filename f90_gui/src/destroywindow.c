#include "../../config.h"
#define DESTROYWINDOW_F90 FC_FUNC (destroywindow, DESTROYWINDOW)
#include "gtk/gtk.h"

void DESTROYWINDOW_F90(GtkWidget **widget)
{
  gtk_widget_destroy(*widget);
}
