#include "../../config.h"
#define MENUITEMSTATE_F90 FC_FUNC (menuitemstate, MENUITEMSTATE)
#include "gtk/gtk.h"

void MENUITEMSTATE_F90(GtkWidget **menuitem, int *checked)
{
  if(gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM(*menuitem)))
    *checked = 1;
  else
    *checked = 0;
}
