#include "../../config.h"
#define CREATESUBMENU_F90 FC_FUNC (createsubmenu, CREATESUBMENU)
#include "../../config.h"
#define CREATEMENU_F90 FC_FUNC (createmenu, CREATEMENU)
#include "gtk/gtk.h"

void CREATEMENU_F90(GtkWidget **menu, GtkWidget **menubar, char *name, int *right)
{
  GtkWidget *menu_item;

  /* Create the menu widget (this is an invisible container) */
  *menu = gtk_menu_new();
  
  /* Create the visible, clickable menu item that goes in the menubar */
  menu_item = gtk_menu_item_new_with_label ((gchar *) name);
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(menu_item), *menu );

  /* Right justify if necessary */
  if(*right > 0)
    gtk_menu_item_set_right_justified(GTK_MENU_ITEM(menu_item),TRUE);

  /* Add the menu item to the menu bar */
  gtk_menu_bar_append (GTK_MENU_BAR(*menubar), menu_item);

}

void CREATESUBMENU_F90(GtkWidget **menu, GtkWidget **inmenu, char *name,
		       int *separator)
{
  GtkWidget *menu_item;
  
  /* Add a separator before the new menu if necessary */
  if(*separator>0)
    {
      GtkWidget *sep;
      sep = gtk_separator_menu_item_new();
      gtk_menu_append (GTK_MENU (*inmenu), sep);	
    }

  /* Create the menu widget (this is an invisible container) */
  *menu = gtk_menu_new();
  
  /* Create the visible, clickable menu item that goes in the parent
     menu */
  menu_item = gtk_menu_item_new_with_label ((gchar *) name);
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(menu_item), *menu );

  /* Add the menu item to the parent menu */
  gtk_menu_append (GTK_MENU (*inmenu), menu_item);

}
