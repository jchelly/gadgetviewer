#include "../../config.h"
#define REDRAWWINDOW_F90 FC_FUNC (redrawwindow, REDRAWWINDOW)
#define SETPARENT_F90 FC_FUNC (setparent, SETPARENT)
#define CREATEWINDOW_F90 FC_FUNC (createwindow, CREATEWINDOW)
#define MOVEWINDOW_F90 FC_FUNC (movewindow, MOVEWINDOW)
#include <gtk/gtk.h>
#include "set_event_handler.h"

gint delete_event( GtkWidget *widget, GdkEvent  *event, gpointer data)
{
  int *iptr;

  /*
    If the user tries to close the window, set window%close=1
    in the Fortran calling program
  */

  iptr = (int *) data;
  *iptr = 1;
  if(event_handler != NULL) (*event_handler)();
  
  return TRUE;
}

void CREATEWINDOW_F90(void **win, int *close, GtkWidget **box,
		   int *xsize, int *ysize,
		   int *min_xsize, int *min_ysize, char *title,
		   int *has_statusbar, GtkWidget **statusbar,
		   int *contextid, int *has_menubar, GtkWidget **menubar,
		   int *decorated, int *resize)
{

  GtkWidget *outer_box;

  /* Create the window */
  *win = gtk_window_new(GTK_WINDOW_TOPLEVEL);

  /* Set the initial size */
  if((*xsize > 0) && (*ysize > 0))
    {
      gint width, height;
      width  = (gint) (*xsize);
      height = (gint) (*ysize);
      gtk_window_set_default_size(*win, width, height);
    }

  /* Set the minimum size */
  if((*min_xsize > 0) && (*min_ysize > 0))
    {
      gint width, height;
      width  = (gint) (*min_xsize);
      height = (gint) (*min_ysize);
      gtk_widget_set_size_request(*win, width, height);
    }

  /* Set the title */ 
  gtk_window_set_title(*win, (gchar *) title); 
  if(*decorated == 0) 
    { 
      gtk_window_set_decorated(GTK_WINDOW(*win),FALSE); 
    } 

  /* Make window fixed size if resize=0 */
  if(*resize == 0)
    {
      gtk_window_set_resizable(GTK_WINDOW(*win),FALSE);
    }

  /*
    Register a callback that will be activated if the user tries
    to close the window
  */

  gtk_signal_connect (GTK_OBJECT (*win), "delete_event",
		      GTK_SIGNAL_FUNC (delete_event), (gpointer) close);

  /* Put a vbox in the window to contain the menu bar and status bar */
  outer_box = gtk_vbox_new(FALSE,0);
  gtk_container_add (GTK_CONTAINER (*win), outer_box);

  /* Add the menubar at the top of the outer vbox */
  if(*has_menubar == 1)
    {
      *menubar = gtk_menu_bar_new();
      gtk_box_pack_start(GTK_BOX(outer_box), *menubar, FALSE, FALSE, 0);
    }

  /* Put another vbox in the outer vbox to contain any widgets
     added by the calling program */
  *box = gtk_vbox_new(FALSE,0);

  /*
    Note we deliberately ignore the settings from gui_packing_mode here
    because this box always has to fill all the available space.
  */
  gtk_box_pack_start(GTK_BOX(outer_box), *box, TRUE, TRUE, 0);

  /* Put a statusbar at the bottom of the outer vbox if requested */
  if(*has_statusbar == 1)
    {
      *statusbar = (GtkWidget *) gtk_statusbar_new();
      gtk_box_pack_end(GTK_BOX(outer_box), *statusbar, FALSE, FALSE, 0);
      *contextid = (int) 
	gtk_statusbar_get_context_id(GTK_STATUSBAR(*statusbar),"Status bar");
      gtk_statusbar_push(GTK_STATUSBAR(*statusbar), (guint) *contextid, " ");
    }
}

void SETPARENT_F90(GtkWidget **win, GtkWidget **par)
{
  gtk_window_set_transient_for(GTK_WINDOW(*win),GTK_WINDOW(*par));
  gtk_window_set_position(GTK_WINDOW(*win),GTK_WIN_POS_CENTER_ON_PARENT);
  gtk_window_set_type_hint(GTK_WINDOW(*win),GDK_WINDOW_TYPE_HINT_DIALOG);
}

void REDRAWWINDOW_F90(GtkWidget **win)
{
  gtk_widget_queue_draw(*win);
}

void MOVEWINDOW_F90(GtkWidget **win, double *x, double *y)
{
  int ix = (*x)*gdk_screen_width();
  int iy = (*y)*gdk_screen_height();
  /* gtk_window_set_gravity(GTK_WINDOW(*win), GDK_GRAVITY_CENTER); */
  gtk_window_move(GTK_WINDOW(*win), (gint) ix, (gint) iy);
}
