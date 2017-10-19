#include <stdlib.h>
#include "../../config.h"

#define SWAPENDIAN_F90 FC_FUNC (swapendian, SWAPENDIAN)
void SWAPENDIAN_F90(void *data, int *n, int *l)
{
  /* Byteswap the specified data */
  int i, j;
  char tmp[256];
  char *arr;

  arr = (char *) data;
#pragma omp parallel for private(i,j,tmp) shared(arr,l,n,data)
  for(i = 0; i < *n; i++)
    {
      for(j = 0; j < (*l); j++)
	tmp[j] = arr[(*l)*i+j];
      for(j = 0; j < (*l); j++)
	arr[(*l)*i+j] = tmp[(*l)-j-1];
    }
}

