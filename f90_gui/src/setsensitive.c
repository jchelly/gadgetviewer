#include "../../config.h"
#define SETSENSITIVE_F90 FC_FUNC (setsensitive, SETSENSITIVE)
#include "gtk/gtk.h"

void SETSENSITIVE_F90(GtkWidget **widget, int *flag)
{
  if(*flag == 0)
    {
      gtk_widget_set_sensitive(*widget, FALSE);     
    }
  else
    {
      gtk_widget_set_sensitive(*widget, TRUE);
    }
}
