#include "../../config.h"
#define CREATEDIALOG_F90 FC_FUNC (createdialog, CREATEDIALOG)
#include "gtk/gtk.h"

void CREATEDIALOG_F90(void **window, int *itype, char *text,
		   int *result)
{
  GtkWidget *dialog;
  GtkMessageType gt;
  GtkButtonsType bt;
  gint res;

  if(*itype == 0)
    {
      gt = GTK_MESSAGE_INFO;
      bt = GTK_BUTTONS_OK;
    }
  if(*itype == 1)
    {
      gt = GTK_MESSAGE_WARNING;
      bt = GTK_BUTTONS_OK_CANCEL;
    }
  if(*itype == 2)
    {
      gt = GTK_MESSAGE_QUESTION;
      bt = GTK_BUTTONS_YES_NO;
    }
  if(*itype == 3)
    {
      gt = GTK_MESSAGE_ERROR;
      bt = GTK_BUTTONS_OK;
    }

  dialog = gtk_message_dialog_new(*window, 
				   GTK_DIALOG_DESTROY_WITH_PARENT, gt,
				   bt, "%s\n", (gchar *) text);
  res = gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy (dialog);	

  /* Return a value indicating which button was pressed */
  switch (res)
    {
    case GTK_RESPONSE_OK:
      *result = 1;
      break;
    case GTK_RESPONSE_CANCEL:
      *result = 2;
      break;
    case GTK_RESPONSE_YES:
      *result = 3;
      break;
    case GTK_RESPONSE_NO:
      *result = 4;
      break;
    default:
      /* Return 0 if dialog was closed without a button being pressed */
      *result = 0;
    }
}
