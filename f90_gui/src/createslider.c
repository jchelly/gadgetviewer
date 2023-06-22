#include "../../config.h"
#define CREATEHSLIDER_F90 FC_FUNC (createhslider, CREATEHSLIDER)
#include "../../config.h"
#define CREATEVSLIDER_F90 FC_FUNC (createvslider, CREATEVSLIDER)
#include "gtk/gtk.h"
#include "pack_box.h"
#include "set_event_handler.h"

void slider_event( GtkWidget *widget, gpointer data)
{
  int *iptr;

  if(ignore_events != 0)return;

  iptr = (int *) data;
  *iptr = 1;
  if(event_handler != NULL) (*event_handler)();
  return;
}

void CREATEVSLIDER_F90(GtkWidget **slider, GtkWidget **box, int *changed,
		    double *min, double *max, double *step, int *c_draw,
		    int *c_size)
{
  *slider = gtk_vscale_new_with_range((gdouble) (*min), (gdouble) (*max),
				      (gdouble) (*step));

  if(*c_size > 0)gtk_widget_set_size_request(*slider, 0, (gint) (*c_size));

  pack_box(*box, *slider);

  g_signal_connect (GTK_WIDGET (*slider), "value_changed",
                    G_CALLBACK(slider_event), (gpointer) changed);

  if(*c_draw > 0)
    gtk_scale_set_draw_value(GTK_SCALE(*slider), TRUE);
  else
    gtk_scale_set_draw_value(GTK_SCALE(*slider), FALSE);
    
}

void CREATEHSLIDER_F90(GtkWidget **slider, GtkWidget **box, int *changed,
		    double *min, double *max, double *step, int *c_draw,
		    int *c_size)
{
  *slider = gtk_hscale_new_with_range((gdouble) (*min), (gdouble) (*max),
				      (gdouble) (*step));

  if(*c_size > 0)gtk_widget_set_size_request(*slider, (gint) (*c_size), 0);

  pack_box(*box, *slider);

  g_signal_connect (GTK_WIDGET (*slider), "value_changed",
                    G_CALLBACK(slider_event), (gpointer) changed);

  if(*c_draw > 0)
    gtk_scale_set_draw_value(GTK_SCALE(*slider), TRUE);
  else
    gtk_scale_set_draw_value(GTK_SCALE(*slider), FALSE);
    
}
