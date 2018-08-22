#include <stdlib.h>
#include "../../config.h"

#define CTERMINATE_F90  FC_FUNC (cterminate,  CTERMINATE)

void CTERMINATE_F90(int *ret)
{
    exit(*ret);
    return;
}

