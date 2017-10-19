#include "../../config.h"
#define PACKINGMODE_F90 FC_FUNC (packingmode, PACKINGMODE)
#include "gtk/gtk.h"
#include "packingmode.h"

gboolean gui_expand   = FALSE;
gboolean gui_fill     = FALSE;
gint  gui_spacing     = 0;
gint  gui_position    = 0;

void PACKINGMODE_F90(int *expand, int *fill, int *spacing, int *position)
{
  if(*expand == 0) gui_expand  = FALSE;
  if(*expand == 1) gui_expand  = TRUE;
  if(*fill == 0)   gui_fill    = FALSE;
  if(*fill == 1)   gui_fill    = TRUE;
  if(*spacing >=0) gui_spacing = *spacing;
  if(*position>=0) gui_position= *position;
}

