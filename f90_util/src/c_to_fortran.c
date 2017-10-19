#include "string.h"
#include "c_to_fortran.h"

/*
  Copy up to MAXLEN characters from a C style string into
  a Fortran style string
*/
void c_to_fortran(char *c_str, char *f_str, int maxlen)
{
  int i;
  int found_zero;

  strncpy(f_str, c_str, (size_t) maxlen); 
  
  /* Remove terminating 0 and replace with trailing blanks */
  found_zero = 0;
  for(i = 0; i < maxlen; i++)
    {
      if(f_str[i] == 0)
	found_zero = 1;
      if(found_zero == 1)
	f_str[i] = ' ';
    }
}
