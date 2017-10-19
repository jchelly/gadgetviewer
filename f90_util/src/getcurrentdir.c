#include <unistd.h>
#include <stdlib.h>
#include <sys/stat.h>
#include "../../config.h"
#include "c_to_fortran.h"

#define GETCURRENTDIR_F90 FC_FUNC (getcurrentdir, GETCURRENTDIR)
#define GETHOMEDIR_F90 FC_FUNC (gethomedir, GETHOMEDIR)
#define MAKEDIR_F90 FC_FUNC (makedir, MAKEDIR)

void GETCURRENTDIR_F90(char *dir, int *maxlen)
{
  char *str;

  str = malloc((*maxlen)*sizeof(char));
  getcwd(str, *maxlen);
  c_to_fortran(str, dir, *maxlen);
  free(str);
}

void GETHOMEDIR_F90(char *dir, int *maxlen)
{
  char *str = getenv("HOME");
  c_to_fortran(str, dir, *maxlen);
}

void MAKEDIR_F90(char *dir, int *status)
{
  *status = mkdir(dir, S_IRWXU);
}
