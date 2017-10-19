#include "../../config.h"
#define CREATETEXTVIEW_F90 FC_FUNC (createtextview, CREATETEXTVIEW)
#define ADDLINE_F90 FC_FUNC (addline, ADDLINE)
#define ADDLINETAG_F90 FC_FUNC (addlinetag, ADDLINETAG)
#define CLEARTEXT_F90 FC_FUNC (cleartext, CLEARTEXT)

#include "gtk/gtk.h"
#include "pack_box.h"

void CREATETEXTVIEW_F90(GtkWidget **textview, GtkWidget **box, 
			GtkTextBuffer **textbuffer)
{
  *textview = gtk_text_view_new();
  pack_box(*box, *textview);
  
  *textbuffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(*textview));

  g_object_set(G_OBJECT(*textview), "editable",       FALSE, NULL);
  g_object_set(G_OBJECT(*textview), "cursor_visible", FALSE, NULL);

  gtk_text_view_set_left_margin(GTK_TEXT_VIEW(*textview), (gint) 3);
  gtk_text_view_set_right_margin(GTK_TEXT_VIEW(*textview), (gint) 3);
}


void ADDLINE_F90(GtkTextBuffer **textbuffer, char *str)
{
  GtkTextIter iter; 
  gtk_text_buffer_get_end_iter(*textbuffer, &iter);  

  gtk_text_buffer_insert(*textbuffer, &iter, (gchar *) str, (gint) -1);

  gtk_text_buffer_insert_at_cursor(*textbuffer, "\n", 1);

}

void ADDLINETAG_F90(GtkTextBuffer **textbuffer, char *str, char *tagname)
{
  GtkTextIter iter;
  GtkTextTag *tag;
  gtk_text_buffer_get_end_iter(*textbuffer, &iter);  
  tag = gtk_text_buffer_create_tag(*textbuffer,
				   (gchar *) tagname,
				   NULL);

  gtk_text_buffer_insert_with_tags(*textbuffer, &iter, (gchar *) str, 
				   (gint) -1, tag, NULL);

  gtk_text_buffer_insert_at_cursor(*textbuffer, "\n", 1);

}

void CLEARTEXT_F90(GtkTextBuffer **textbuffer)
{
  GtkTextIter iter1, iter2; 
  gtk_text_buffer_get_start_iter(*textbuffer, &iter1);  
  gtk_text_buffer_get_end_iter(*textbuffer, &iter2); 
  gtk_text_buffer_delete(*textbuffer, &iter1, &iter2);
}
