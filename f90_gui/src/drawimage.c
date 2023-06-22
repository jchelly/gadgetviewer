#include "../../config.h"
#define DRAWIMAGE_F90 FC_FUNC (drawimage, DRAWIMAGE)

#include "gtk/gtk.h"
#include "gdk/gdk.h"

void DRAWIMAGE_F90(GtkWidget **drawingarea, cairo_surface_t **surface,
		char *image, int *width, int *height, int *x, int *y)
{
  /*
    gdk_draw_rgb_image(*pixmap, (*drawingarea)->style->black_gc,
    (gint) *x, (gint) *y, (gint) *width, (gint) *height,
    GDK_RGB_DITHER_NONE, (guchar *) image,
    (gint) (3*(*width)));
  */

  cairo_surface_t *source = cairo_image_surface_create_for_data(image, CAIRO_FORMAT_ARGB32,
                                                                width, height, 4*width);

  cairo_surface_destroy(source);



}
  
