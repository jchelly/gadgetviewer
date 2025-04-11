#include "../../config.h"
#define STARTGTK_F90 FC_FUNC (startgtk, STARTGTK)
#include <gtk/gtk.h>
#include <locale.h>

void STARTGTK_F90(void)
{
  setlocale(LC_ALL,"C");
  gtk_disable_setlocale();

  gtk_init(NULL,NULL);

  // Animations make the UI dramatically slower on a remote display
  GtkSettings *settings = gtk_settings_get_default();
  g_object_set(settings, "gtk-enable-animations", FALSE, NULL);
}
