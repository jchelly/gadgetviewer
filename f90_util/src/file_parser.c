#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <glib.h>
#include "../../config.h"
#include "c_to_fortran.h"

#define READKEYFILE_F90    FC_FUNC (readkeyfile,  READKEYFILE)
#define WRITEKEYFILE_F90   FC_FUNC (writekeyfile, WRITEKEYFILE)
#define ADDINTKEY_F90      FC_FUNC (addintkey,    ADDINTKEY)
#define ADDDOUBLEKEY_F90   FC_FUNC (adddoublekey, ADDDOUBLEKEY)
#define ADDSTRINGKEY_F90   FC_FUNC (addstringkey, ADDSTRINGKEY)
#define GETINTKEY_F90      FC_FUNC (getintkey,    GETINTKEY)
#define GETDOUBLEKEY_F90   FC_FUNC (getdoublekey, GETDOUBLEKEY)
#define GETSTRINGKEY_F90   FC_FUNC (getstringkey, GETSTRINGKEY)
#define CLOSEKEYFILE_F90   FC_FUNC (closekeyfile, CLOSEKEYFILE)
#define KEYGROUPS_F90      FC_FUNC (keygroups,    KEYGROUPS)
#define GETGROUPNAMES_F90  FC_FUNC (getgroupnames, GETGROUPNAMES)
#define HASGROUP_F90       FC_FUNC (hasgroup,     HASGROUP)
#define NEWKEYFILE_F90     FC_FUNC (newkeyfile,   NEWKEYFILE)
#define GETNGROUPS_F90     FC_FUNC (getngroups,   GETNGROUPS)

#ifdef HAVE_KEYFILE
static GKeyFile *keyfile;
#endif

/*
  Make a new key file in memory
*/
void NEWKEYFILE_F90()
{
#ifdef HAVE_KEYFILE
  keyfile = g_key_file_new();
#endif 
  return;
}

/*
  Read in a key file or make a new one if it doesn't exist
*/
void READKEYFILE_F90(char *fname, int *iostat)
{
#ifdef HAVE_KEYFILE
  GError *error = NULL;
  keyfile = g_key_file_new(); 
  g_key_file_load_from_file(keyfile, (const gchar *) fname,
			    G_KEY_FILE_NONE, &error);
  if(error == NULL)
    *iostat = 0;
  else
    {
      g_error_free(error);
      *iostat = 1;
    }
  return;
#else
  *iostat = 1;
#endif
}

/*
  Write a key file to disk
*/
void WRITEKEYFILE_F90(char *fname, int *iostat)
{
#ifdef HAVE_KEYFILE
  FILE *fd;
  gsize len;

  gchar *data = g_key_file_to_data(keyfile, &len, NULL);

  *iostat = 1;
  if((fd = fopen(fname,"w")) == NULL)
    {
      g_free(data);
      return;
    }
  if(fprintf(fd, "%s\n", data) < 0)
    {
      g_free(data);
      fclose(fd);
      return;
    }
  if(fclose(fd) != 0)
    {
      g_free(data);
      return;
    }

  g_free(data);
  *iostat = 0;
  return;
#else
  *iostat = 1;
#endif
}

/*
  Close a key file
*/
void CLOSEKEYFILE_F90()
{
#ifdef HAVE_KEYFILE
  g_key_file_free(keyfile);
#endif
}

/*
  Set values in a key file
*/
void ADDINTKEY_F90(char *group, char *name, int *value, int *n)
{
#ifdef HAVE_KEYFILE
  g_key_file_set_integer_list(keyfile, 
			      (const gchar *) group,
			      (const gchar *) name,
			      (gint *) value,
			      (gsize) *n);
#endif
}

void ADDDOUBLEKEY_F90(char *group, char *name, double *value, int *n)
{
#ifdef HAVE_KEYFILE
  g_key_file_set_double_list(keyfile, 
			     (const gchar *) group,
			     (const gchar *) name,
			     (gdouble *) value,
			     (gsize) *n);
#endif
}

/*
  It is assumed that the input "array" of strings is a char array of length
  n*len where n is the number of strings. The strings should be null
  terminated (where there is space for a terminator).
*/
void ADDSTRINGKEY_F90(char *group, char *name, char *value, int *len, int *n)
{
#ifdef HAVE_KEYFILE
  int i;
  gchar **arr = malloc(sizeof(gchar *)*(*n));
  for(i=0;i<(*n);i++)
    {
      arr[i] = malloc(sizeof(gchar)*(*len));
      strncpy(arr[i], &(value[i*(*len)]), (size_t) *len);
    }
  g_key_file_set_string_list(keyfile, 
			     (const gchar *) group,
			     (const gchar *) name,
			     (const gchar **) arr, (gsize) *n);
  for(i=0;i<(*n);i++)
    free(arr[i]);
  free(arr);
#endif
}


/*
  Get values from a key file. Leave variables unchanged if key is not
  present.
*/
void GETINTKEY_F90(char *group, char *name, int *value, int *n, int *nmax)
{
#ifdef HAVE_KEYFILE
  GError *error = NULL;
  gsize len;
  gint *v = g_key_file_get_integer_list(keyfile, 
					(const gchar *) group,
					(const gchar *) name,
					&len,
					&error);
  if ( error == NULL )
    {
      int i;
      *n = (int) len;
      if(*n > *nmax)*n = *nmax;
      for(i=0;i<(*n);i++)
	value[i] = (int) v[i];
    }
  else
    {
      g_error_free(error);
      *n = 0;
    }
  if(v != NULL)g_free(v);    
#else
  *n = 0;
#endif
}

void GETDOUBLEKEY_F90(char *group, char *name, double *value, int *n, int *nmax)
{
#ifdef HAVE_KEYFILE
  GError *error = NULL;
  gsize len;
  gdouble *v = g_key_file_get_double_list(keyfile, 
					  (const gchar *) group,
					  (const gchar *) name,
					  &len,
					  &error);
  if ( error == NULL )
    {
      int i;
      *n = (int) len;
      if(*n > *nmax)*n = *nmax;
      for(i=0;i<(*n);i++)
	value[i] = (double) v[i];
    }
  else
    {
      *n = 0;
      g_error_free(error);
    }
  if(v != NULL)g_free(v);
#else
  *n = 0;
#endif
}

void GETSTRINGKEY_F90(char *group, char *name, char *value, int *len, int *n, int *nmax)
{
#ifdef HAVE_KEYFILE
  int i;
  GError *error = NULL;
  gsize nlist;
  gchar **arr = g_key_file_get_string_list(keyfile, 
					   (const gchar *) group,
					   (const gchar *) name,
					   &nlist, &error);
  if ( error == NULL )
    {
      *n = nlist;
      if(*n > *nmax)*n = *nmax;
      for(i=0;i<(*n);i++)
	c_to_fortran((char *) arr[i], &(value[i*(*len)]), *len);
    }
  else
    {
      g_error_free(error);
      *n = 0;
    }
  if(arr != NULL) g_strfreev(arr);
#else
  *n = 0;
#endif
}


void GETGROUPNAMES_F90(char *value, int *len, int *n, int *nmax)
{
#ifdef HAVE_KEYFILE
  int i;
  gsize nlist;
  gchar **arr = g_key_file_get_groups(keyfile, &nlist);

  *n = nlist;
  if(*n > *nmax)*n = *nmax;
  for(i=0;i<(*n);i++)
    c_to_fortran((char *) arr[i], &(value[i*(*len)]), *len);
  if(arr != NULL) g_strfreev(arr);
#else
  *n = 0;
#endif
}

void GETNGROUPS_F90(int *n)
{
#ifdef HAVE_KEYFILE
  int i;
  gsize nlist;
  gchar **arr = g_key_file_get_groups(keyfile, &nlist);

  *n = nlist;
  if(arr != NULL) g_strfreev(arr);
#else
  *n = 0;
#endif
}


void HASGROUP_F90(char *name, int *flag)
{
#ifdef HAVE_KEYFILE
  if(g_key_file_has_group(keyfile, (gchar *) name))
    *flag = 1;
  else
    *flag = 0;
#else
  *flag = 0;
#endif
}
