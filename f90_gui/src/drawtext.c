#include "../../config.h"
#define DRAWTEXT_F90 FC_FUNC (drawtext, DRAWTEXT)
#include "gtk/gtk.h"

static GdkFont *font = NULL;

void DRAWTEXT_F90(GtkWidget **drawingarea, GdkPixmap **pixmap,
	      int *x, int *y, char *text)
{
  if(!font)font=gdk_font_load
    ("-*-helvetica-medium-r-*--12-*-*-*-p-*-iso8859-1");

  if(!font)font=gdk_font_load
    ("-*-*-medium-r-*--12-*-*-*-p-*-iso8859-1");

  if(!font)font=gdk_font_load
    ("-*-*-*-r-*--12-*-*-*-p-*-iso8859-1");

  gdk_draw_string(*pixmap,
		  font,
		  (*drawingarea)->style->white_gc,
		  (gint) *x,
		  (gint) *y,
		  (gchar *) text);
}
