#include "../../config.h"
#define COLOURBUTTONSET_F90 FC_FUNC (colourbuttonset, COLOURBUTTONSET)
#include "../../config.h"
#define COLOURBUTTONGET_F90 FC_FUNC (colourbuttonget, COLOURBUTTONGET)
#include "../../config.h"
#define CREATECOLOURBUTTON_F90 FC_FUNC (createcolourbutton, CREATECOLOURBUTTON)
#include "gtk/gtk.h"
#include "pack_box.h"
#include "set_event_handler.h"

void colourbutton_event( GtkWidget *widget, gpointer data)
{
  int *iptr;
  iptr = (int *) data;
  *iptr = 1;
  if(event_handler != NULL) (*event_handler)();
  return;
}

void CREATECOLOURBUTTON_F90(GtkWidget **button, GtkWidget **box, int *clicked)
{
  *button = gtk_color_button_new();
  g_object_set(G_OBJECT(*button), "show-editor", TRUE, NULL);
  pack_box(*box, *button);

  g_signal_connect (GTK_WIDGET (*button), "color-set",
                    G_CALLBACK(colourbutton_event),(gpointer) clicked);
}

void COLOURBUTTONGET_F90(GtkWidget **button, int *r, int *g, int *b)
{
  GdkRGBA color;
  gtk_color_chooser_get_rgba(GTK_COLOR_CHOOSER(*button), &color);
  (*r) = color.red   * 255;
  (*g) = color.green * 255;
  (*b) = color.blue  * 255;
}

void COLOURBUTTONSET_F90(GtkWidget **button, int *r, int *g, int *b)
{
  GdkRGBA color;
  color.red   = ((double) (*r)) / 255;
  color.green = ((double) (*g)) / 255;
  color.blue  = ((double) (*b)) / 255;
  color.alpha = 1.0;
  gtk_color_chooser_set_rgba(GTK_COLOR_CHOOSER(*button), &color);
}
