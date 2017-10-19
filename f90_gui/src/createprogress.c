#include "../../config.h"
#define SETPROGRESS_F90 FC_FUNC (setprogress, SETPROGRESS)
#include "../../config.h"
#define CREATEPBAR_F90 FC_FUNC (createpbar, CREATEPBAR)
#include "gtk/gtk.h"
#include "pack_box.h"

void CREATEPBAR_F90(GtkWidget **pbar, GtkWidget **box)
{
  *pbar = gtk_progress_bar_new();
  pack_box(*box, *pbar);
}

void SETPROGRESS_F90(GtkWidget **pbar, double *f)
{
  gtk_progress_bar_set_fraction( GTK_PROGRESS_BAR(*pbar), (gdouble) *f);
}
