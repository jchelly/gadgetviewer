#include "../../config.h"
#define SELECTFILE_F90 FC_FUNC (selectfile, SELECTFILE)
#include <string.h>
#include "gtk/gtk.h"
#include "c_to_fortran.h"



void SELECTFILE_F90(GtkWidget **window, char *message, int *c_mode, int *c_ok,
		char *filename, int *string_length)
{
  
  GtkWidget *dialog;

  /*
    New GTK file selector chokes on anything but the smallest
    directories and makes pasting in paths highly annoying,
    so offer option to use old version in configure script
  */
#ifndef OLD_FILE_SELECTOR
  if(*c_mode == 0)
    {

      dialog = gtk_file_chooser_dialog_new ((gchar *) message,
					    GTK_WINDOW(*window),
					    GTK_FILE_CHOOSER_ACTION_OPEN,
					    GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
					    GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,
					    NULL);

    }
  else
    {
      dialog = gtk_file_chooser_dialog_new ((gchar *) message,
					    GTK_WINDOW(*window),
					    GTK_FILE_CHOOSER_ACTION_SAVE,
					    GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
					    GTK_STOCK_SAVE, GTK_RESPONSE_ACCEPT,
					    NULL);
    }
#else
  dialog = gtk_file_selection_new((gchar *) message);
#endif


#ifndef OLD_FILE_SELECTOR
  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
      char *f;
      f = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
      c_to_fortran(f,filename,*string_length);
      g_free (f);
      *c_ok = 1;
    }
  else
    {
      *c_ok = 0;
    }
#else
  if (gtk_dialog_run (GTK_DIALOG (dialog)) != GTK_RESPONSE_CANCEL)
    {
      char *f = (char *) gtk_file_selection_get_filename(GTK_FILE_SELECTION(dialog));
      c_to_fortran(f,filename,*string_length);
      *c_ok = 1;
    }
  else
    *c_ok = 0;
#endif

  gtk_widget_destroy (dialog);	

}
