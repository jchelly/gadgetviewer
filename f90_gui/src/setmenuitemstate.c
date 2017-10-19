#include "../../config.h"
#define SETMENUITEMSTATE_F90 FC_FUNC (setmenuitemstate, SETMENUITEMSTATE)
#include "gtk/gtk.h"
#include "set_event_handler.h"

void SETMENUITEMSTATE_F90(GtkWidget **menuitem, int *checked)
{
  ignore_events = 1;
  gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(*menuitem),(*checked)==1);
  ignore_events = 0;
}
