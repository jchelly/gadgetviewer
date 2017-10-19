#include "../../config.h"
#include <stdlib.h>
#include <stdio.h>
#ifdef HAVE_LIBPNG
#include <png.h>
#endif

#define WRITEPNG_F90 FC_FUNC (writepng, WRITEPNG)

/*
  Write out an image stored as a 3*width*height character array. Intended to be
  called from Fortran, in which case the calling program needs to append a zero
  character to the filename. E.g. fname = trim(fname)//achar(0)

  Returns *ires = 0 if successful.

*/

void WRITEPNG_F90(char *fname, int *ix, int *iy, unsigned char *img, int *ires)
{
#ifdef HAVE_LIBPNG
  png_byte **row_array = malloc((*iy)*sizeof(png_byte*));
  int rowstride        = 3*(*ix);
  int i;
  png_struct *png;
  FILE *fd;
  png_info *info;

  /* Return failure if memory can't be allocated */
  if(row_array == NULL)
    {
      *ires = 1;
      return;
    }

  for (i = 0; i < (*iy); i++)
    row_array[i] = img + i * rowstride;
  
  png  = png_create_write_struct(PNG_LIBPNG_VER_STRING,
				 NULL, NULL, NULL);
  info = png_create_info_struct(png);
  fd = fopen(fname,"wb");

  if(fd == NULL)
    {
      *ires = 1;
      return;
    }

  if((fd != NULL) && (png != NULL) && (info != NULL))
    {
      png_color_16 black;
      png_init_io (png, fd);
      png_set_IHDR (png, info, (*ix), (*iy), 8, PNG_COLOR_TYPE_RGB,
		    PNG_INTERLACE_NONE,
		    PNG_COMPRESSION_TYPE_DEFAULT,
		    PNG_FILTER_TYPE_DEFAULT);
      black.red   = 0x00;
      black.blue  = 0x00;
      black.green = 0x00;
      png_set_bKGD(png, info, &black);
      png_write_info (png, info);
      png_write_image(png, row_array);
      png_write_end  (png, info);
      png_destroy_write_struct (&png, &info);
      fclose(fd);
    }
  else
    {
      /* Something went wrong */
      *ires = 2;
      fclose(fd);
      png_destroy_write_struct (&png, &info);    
      return;
    }
  /* Completed successfully */
  *ires = 0;
  free (row_array);
#else
  /* No PNG library, so can't do anything */
  *ires = 3;
  return;
#endif
}
