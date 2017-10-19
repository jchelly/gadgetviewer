#include "gtk/gtk.h"
#include "packingmode.h"

void pack_box(GtkWidget *box, GtkWidget *widget)
{
  if(gui_position == 0)
    {
      gtk_box_pack_start(GTK_BOX(box), widget, gui_expand, gui_fill, gui_spacing);
    }
  else
    {
      gtk_box_pack_end(GTK_BOX(box), widget, gui_expand, gui_fill, gui_spacing);
    }
}
