#include "../../config.h"
#define DRAWIMAGE_F90 FC_FUNC (drawimage, DRAWIMAGE)

#include "gtk/gtk.h"
#include "gdk/gdk.h"

void DRAWIMAGE_F90(GtkWidget **drawingarea, cairo_surface_t **surface,
		unsigned char *image, int *width, int *height, int *x, int *y)
{

  /* Flush any updates to the surface before we modify it */
  cairo_surface_flush(*surface);

  /* Get parameters of the Cairo surface to update */
  int s_width  = cairo_image_surface_get_width(*surface);
  int s_height = cairo_image_surface_get_height(*surface);
  int s_stride = cairo_image_surface_get_stride(*surface);
  unsigned char *s_data = cairo_image_surface_get_data(*surface);

  /* Loop over input pixels */
  int i, j;
  for (j=0; j<*height; j+=1) {
    for (i=0; i<*width; i+=1) {

      /* Look up the input pixel value */
      unsigned char input_r = image[3*(*width)*j+3*i+0];
      unsigned char input_g = image[3*(*width)*j+3*i+1];
      unsigned char input_b = image[3*(*width)*j+3*i+2];

      /* Compute location in Cairo data */
      int s_x = (*x) + i; /* x coordinate in Cairo surface */
      int s_y = (*y) + j; /* y coordinate in Cairo surface */
      unsigned int offset = (s_stride*s_y) + 4*s_x;

      /* Update the Cairo surface (note: probably wrong for big endian systems?)*/
      if(s_x >= 0 && s_x < s_width && s_y >=0 && s_y < s_height) {
        s_data[offset+0] = input_b;
        s_data[offset+1] = input_g;
        s_data[offset+2] = input_r;
        s_data[offset+3] = 255;
      }

    }
  }

  /* Mark surface as dirty */
  cairo_surface_mark_dirty(*surface);

}
  
