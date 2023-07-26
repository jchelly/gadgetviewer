#include "../../config.h"
#define SETVISIBLE_F90 FC_FUNC (setvisible, SETVISIBLE)
#include "gtk/gtk.h"

void SETVISIBLE_F90(GtkWidget **widget, int *flag)
{
  if(*flag == 0)
    {
      gtk_widget_hide(*widget);     
    }
  else
    {
      gtk_widget_show_all(*widget);     
    }
}
