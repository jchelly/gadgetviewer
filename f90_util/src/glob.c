#include <unistd.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <glob.h>
#include "../../config.h"
#include "c_to_fortran.h"

#define GLOBFNAME_F90 FC_FUNC (globfname, GLOBFNAME)
void GLOBFNAME_F90(char *pattern, int *maxlen)
{
  /* 
     Convert pattern to a real file name, or leave alone if no match
  */
  glob_t pglob;
  if(glob(pattern, GLOB_TILDE, NULL, &pglob)==0)
    c_to_fortran(pglob.gl_pathv[0], pattern, *maxlen);
  globfree(&pglob);
}

