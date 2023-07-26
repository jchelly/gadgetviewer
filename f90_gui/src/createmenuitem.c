#include "../../config.h"
#define CREATEMENUITEM_F90 FC_FUNC (createmenuitem, CREATEMENUITEM)
#include "gtk/gtk.h"
#include "set_event_handler.h"

void menuitem_event(gpointer data)
{
  int *iptr;

  if(ignore_events != 0)return;

  iptr = (int *) data;
  *iptr = 1;
  if(event_handler != NULL) (*event_handler)();

  return;
}

void menuitem_toggled(GtkWidget *widget, gpointer data)
{
  int *iptr;

  if(ignore_events != 0)return;

  iptr = (int *) data;
  *iptr = 1;
  if(event_handler != NULL) (*event_handler)();

}

void CREATEMENUITEM_F90(GtkWidget **menuitem, int *clicked,
		     GtkWidget **menu, char *name, int *separator,
		     int *type, GtkWidget **previous, int *changed)
{
  if(*separator>0)
    {
      GtkWidget *sep;
      sep = gtk_separator_menu_item_new();
      gtk_menu_shell_append (GTK_MENU_SHELL(*menu), sep);	
    }

  if(*type == 0)
    *menuitem = gtk_menu_item_new_with_label ((gchar *) name);

  if(*type == 1)
    *menuitem = gtk_check_menu_item_new_with_label ((gchar *) name);

  if(*type == 2)
    *menuitem = gtk_radio_menu_item_new_with_label (NULL, (gchar *) name);

  if(*type == 3)
    *menuitem = gtk_radio_menu_item_new_with_label_from_widget
      (GTK_RADIO_MENU_ITEM(*previous), (gchar *) name);

  gtk_menu_shell_append(GTK_MENU_SHELL(*menu), *menuitem);

  g_signal_connect_swapped(GTK_WIDGET (*menuitem), "activate",
                           G_CALLBACK(menuitem_event),
                           (gpointer) clicked);
  
  /* Checkbox and radiobutton items can emit the 'toggled' signal */
  if(*type > 0)
    g_signal_connect (GTK_WIDGET (*menuitem), "toggled",
                      G_CALLBACK(menuitem_toggled),
                      (gpointer) changed);

}
