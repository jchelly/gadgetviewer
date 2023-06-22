#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "../../config.h"
#define ADDACCEL_F90 FC_FUNC (addaccel, ADDACCEL)
#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkkeysyms.h>

void ADDACCEL_F90(GtkWidget **window, GtkAccelGroup **accel_group,
		  GtkWidget **menuitem, char *key, char *mod, 
		  int *make_group)
{
  guint ikey;
  guint imod;

  if(*make_group != 0)
    {
      (*accel_group) = gtk_accel_group_new ();
      gtk_window_add_accel_group(GTK_WINDOW(*window), (*accel_group));
    }

  ikey = 0;

  /* Letters */
  if(strcmp(key,"A")==0)ikey=GDK_KEY_A;
  if(strcmp(key,"B")==0)ikey=GDK_KEY_B;
  if(strcmp(key,"C")==0)ikey=GDK_KEY_C;
  if(strcmp(key,"D")==0)ikey=GDK_KEY_D;
  if(strcmp(key,"E")==0)ikey=GDK_KEY_E;
  if(strcmp(key,"F")==0)ikey=GDK_KEY_F;
  if(strcmp(key,"G")==0)ikey=GDK_KEY_G;
  if(strcmp(key,"H")==0)ikey=GDK_KEY_H;
  if(strcmp(key,"I")==0)ikey=GDK_KEY_I;
  if(strcmp(key,"J")==0)ikey=GDK_KEY_J;
  if(strcmp(key,"K")==0)ikey=GDK_KEY_K;
  if(strcmp(key,"L")==0)ikey=GDK_KEY_L;
  if(strcmp(key,"M")==0)ikey=GDK_KEY_M;
  if(strcmp(key,"N")==0)ikey=GDK_KEY_N;
  if(strcmp(key,"O")==0)ikey=GDK_KEY_O;
  if(strcmp(key,"P")==0)ikey=GDK_KEY_P;
  if(strcmp(key,"Q")==0)ikey=GDK_KEY_Q;
  if(strcmp(key,"R")==0)ikey=GDK_KEY_R;
  if(strcmp(key,"S")==0)ikey=GDK_KEY_S;
  if(strcmp(key,"T")==0)ikey=GDK_KEY_T;
  if(strcmp(key,"U")==0)ikey=GDK_KEY_U;
  if(strcmp(key,"V")==0)ikey=GDK_KEY_V;
  if(strcmp(key,"W")==0)ikey=GDK_KEY_W;
  if(strcmp(key,"X")==0)ikey=GDK_KEY_X;
  if(strcmp(key,"Y")==0)ikey=GDK_KEY_Y;
  if(strcmp(key,"Z")==0)ikey=GDK_KEY_Z;

  /* Numbers */
  if(strcmp(key,"1")==0)ikey=GDK_KEY_1;
  if(strcmp(key,"2")==0)ikey=GDK_KEY_2;
  if(strcmp(key,"3")==0)ikey=GDK_KEY_3;
  if(strcmp(key,"4")==0)ikey=GDK_KEY_4;
  if(strcmp(key,"5")==0)ikey=GDK_KEY_5;
  if(strcmp(key,"6")==0)ikey=GDK_KEY_6;
  if(strcmp(key,"7")==0)ikey=GDK_KEY_7;
  if(strcmp(key,"8")==0)ikey=GDK_KEY_8;
  if(strcmp(key,"9")==0)ikey=GDK_KEY_9;
  if(strcmp(key,"0")==0)ikey=GDK_KEY_0;

  /* Symbols */
  if(strcmp(key,".")==0)ikey=GDK_KEY_period;
  if(strcmp(key,",")==0)ikey=GDK_KEY_comma;
  if(strcmp(key,"<")==0)ikey=GDK_KEY_less;
  if(strcmp(key,">")==0)ikey=GDK_KEY_greater;
  if(strcmp(key,"=")==0)ikey=GDK_KEY_equal;
  if(strcmp(key,"-")==0)ikey=GDK_KEY_minus;

  /* Function keys */
  if(strcmp(key,"F1")==0)ikey=GDK_KEY_F1;
  if(strcmp(key,"F2")==0)ikey=GDK_KEY_F2;
  if(strcmp(key,"F3")==0)ikey=GDK_KEY_F3;
  if(strcmp(key,"F4")==0)ikey=GDK_KEY_F4;
  if(strcmp(key,"F5")==0)ikey=GDK_KEY_F5;
  if(strcmp(key,"F6")==0)ikey=GDK_KEY_F6;
  if(strcmp(key,"F7")==0)ikey=GDK_KEY_F7;
  if(strcmp(key,"F8")==0)ikey=GDK_KEY_F8;
  if(strcmp(key,"F9")==0)ikey=GDK_KEY_F9;
  if(strcmp(key,"F10")==0)ikey=GDK_KEY_F10;

  if(ikey==0)
    {
      printf("Unrecognised accelerator key in addaccel()\n");
      abort();
    }

  imod = 0;

  if(strcmp(mod,"CTRL")==0)  imod=GDK_CONTROL_MASK;
  if(strcmp(mod,"SHIFT")==0) imod=GDK_SHIFT_MASK;
  if(strcmp(mod,"ALT")==0)   imod=GDK_MOD1_MASK;

  gtk_widget_add_accelerator((*menuitem), "activate", (*accel_group),
			     ikey, (GdkModifierType) imod, GTK_ACCEL_VISIBLE);
}

