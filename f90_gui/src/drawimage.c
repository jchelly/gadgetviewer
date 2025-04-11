#include "../../config.h"
#define DRAWIMAGE_F90 FC_FUNC (drawimage, DRAWIMAGE)

#include <stdint.h>
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
      int offset = (s_stride*s_y) + 4*s_x;

      /* Compute pixel value: Cairo ARGB32 format uses 4 bytes per pixel with
         alpha in the 8 most significant bits and then r,g,b in descending order of
         significance. So the ordering in memory depends on endian-ness. */
      union {
        uint32_t uint;
        unsigned char usc[4]; 
      } pixel;
      pixel.uint =
        (((unsigned int) 255) << 24)
        + (((unsigned int) input_r) << 16)
        + (((unsigned int) input_g) << 8)
        + ((unsigned int) input_b);

      /* Update the Cairo surface */
      if(s_x >= 0 && s_x < s_width && s_y >=0 && s_y < s_height)
        memcpy(s_data+offset, pixel.usc, 4);
    }
  }

  /* Mark surface as dirty */
  cairo_surface_mark_dirty(*surface);
}
  
