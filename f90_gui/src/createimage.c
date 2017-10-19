#include "../../config.h"
#define CREATEIMAGE_F90 FC_FUNC (createimage, CREATEIMAGE)
#include "gtk/gtk.h"
#include "pack_box.h"

void CREATEIMAGE_F90(GtkWidget **image, GtkWidget **box, char *fname)
{
  *image = gtk_image_new_from_file (fname);
  pack_box(*box, *image);
}
