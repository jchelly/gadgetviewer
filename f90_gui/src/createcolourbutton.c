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
  pack_box(*box, *button);

  g_signal_connect (GTK_OBJECT (*button), "color-set",
                    G_CALLBACK(colourbutton_event),(gpointer) clicked);
}

void COLOURBUTTONGET_F90(GtkWidget **button, int *r, int *g, int *b)
{
  GdkColor color;
  gtk_color_button_get_color(GTK_COLOR_BUTTON(*button),&color);

  (*r) = color.red   / 256;
  (*g) = color.green / 256;
  (*b) = color.blue  / 256;
}

void COLOURBUTTONSET_F90(GtkWidget **button, int *r, int *g, int *b)
{
  GdkColor color;

  color.red   = (*r) * 256;  
  color.green = (*g) * 256;  
  color.blue  = (*b) * 256;  
  gtk_color_button_set_color(GTK_COLOR_BUTTON(*button),&color);   

}
