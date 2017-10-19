#include "../../config.h"
#define STARTGTK_F90 FC_FUNC (startgtk, STARTGTK)
#include <gtk/gtk.h>
#include <locale.h>

void STARTGTK_F90(void)
{
  setlocale(LC_ALL,"C");
  gtk_disable_setlocale();

  gtk_init(NULL,NULL);
}
