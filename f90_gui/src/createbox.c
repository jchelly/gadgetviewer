#include "../../config.h"
#define CREATEVBOX_F90 FC_FUNC (createvbox, CREATEVBOX)
#include "../../config.h"
#define CREATEHBOX_F90 FC_FUNC (createhbox, CREATEHBOX)
#include "gtk/gtk.h"
#include "pack_box.h"

/*
  Add a hbox or vbox to another box.
  (the first box in a window is created automatically when
  the window is created, so we don't need a routine to
  create a box in a window)
*/


void CREATEHBOX_F90(GtkWidget **box, GtkWidget **inbox, int *has_frame,
		    int *has_label, char *label, int *has_scrollbars, 
		    int *expand)
{
  GtkWidget *parent;
  if(*expand != 0)
    {
      GtkWidget *expander = gtk_expander_new((gchar *) label);
      pack_box(*inbox, expander);
      parent = gtk_vbox_new(FALSE,0);
      gtk_container_add(GTK_CONTAINER(expander), parent);  
      gtk_expander_set_expanded(GTK_EXPANDER(expander),FALSE);
    }
  else
    {
      parent = *inbox;
    }

  if(*has_scrollbars == 0)
    {
      if(*has_frame==0)
	{
	  *box = gtk_hbox_new(FALSE,0);
	  pack_box(parent, *box);
	}
      else
	{
	  GtkWidget *frame;
	  if(*has_label == 0 || *expand != 0)
	    {
	      frame = gtk_frame_new(NULL);
	    }
	  else
	    {
	      frame = gtk_frame_new((gchar *) label);
	    }
	  pack_box(parent, frame);
	  *box = gtk_hbox_new(FALSE,0);
	  gtk_container_add(GTK_CONTAINER(frame), *box);
	}
    }
  else
    {
      /* Put a scrollable window in the parent box */
      GtkWidget *scrollwin = gtk_scrolled_window_new(NULL, NULL);
      pack_box(parent, scrollwin);

      /* Set the scrollbars to disappear if not needed */
      gtk_scrolled_window_set_policy  (GTK_SCROLLED_WINDOW(scrollwin),
				       GTK_POLICY_AUTOMATIC,
				       GTK_POLICY_AUTOMATIC);

      /* Create the new box and put it in the scroll window */
      *box = gtk_hbox_new(FALSE,0);
      gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(scrollwin),
					    *box);
    }
}


void CREATEVBOX_F90(GtkWidget **box, GtkWidget **inbox, int *has_frame,
		    int *has_label, char *label, int *has_scrollbars, 
		    int *expand)
{
  GtkWidget *parent;
  if(*expand != 0)
    {
      GtkWidget *expander = gtk_expander_new((gchar *) label);
      pack_box(*inbox, expander);
      parent = gtk_vbox_new(FALSE,0);
      gtk_container_add(GTK_CONTAINER(expander), parent); 
      gtk_expander_set_expanded(GTK_EXPANDER(expander),FALSE);
    }
  else
    {
      parent = *inbox;
    }

  if(*has_scrollbars == 0)
    {
      if(*has_frame==0)
	{
	  *box = gtk_vbox_new(FALSE,0);
	  pack_box(parent, *box);
	}
      else
	{
	  GtkWidget *frame;
	  if(*has_label == 0 || *expand != 0)
	    {
	      frame = gtk_frame_new(NULL);
	    }
	  else
	    {
	      frame = gtk_frame_new((gchar *) label);
	    }
	  pack_box(parent, frame);
	  *box = gtk_vbox_new(FALSE,0);
	  gtk_container_add(GTK_CONTAINER(frame), *box);
	}
    }
  else
    {
      /* Put a scrollable window in the parent box */
      GtkWidget *scrollwin = gtk_scrolled_window_new(NULL, NULL);
      pack_box(parent, scrollwin);

      /* Set the scrollbars to disappear if not needed */
      gtk_scrolled_window_set_policy  (GTK_SCROLLED_WINDOW(scrollwin),
				       GTK_POLICY_AUTOMATIC,
				       GTK_POLICY_AUTOMATIC);

      /* Create the new box and put it in the scroll window */
      *box = gtk_vbox_new(FALSE,0);
      gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(scrollwin),
					    *box);
    }
}

