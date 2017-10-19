#include <stdlib.h>
#include <stdio.h>
#include "../../config.h"
#define OPENBINARYFILE_F90  FC_FUNC (openbinaryfile,  OPENBINARYFILE)
#define CLOSEBINARYFILE_F90 FC_FUNC (closebinaryfile, CLOSEBINARYFILE)
#define READBINARYFILE_F90  FC_FUNC (readbinaryfile,  READBINARYFILE)
#define READBINARYPOS_F90   FC_FUNC (readbinarypos,   READBINARYPOS)
#define SKIPBYTES_F90       FC_FUNC (skipbytes,       SKIPBYTES)

static FILE *fd = NULL;

/* Open a binary file. Return zero in 'ret' on success */
void OPENBINARYFILE_F90(char *fname, int *ret)
{
  fd = fopen(fname,"rb");

  if(fd == NULL)
    *ret = -1;
  else
    *ret = 0;
  
  return;
}

/* Close a binary file */
void CLOSEBINARYFILE_F90()
{
  fclose(fd);
}

/* Read from a binary file. Return zero on success */
void READBINARYFILE_F90(int *nbytes, void *data, int *ret)
{

  if(fread(data, (size_t) 1, (size_t) (*nbytes), fd) != (size_t) (*nbytes))
    {
      *ret = -2;
      return;
    }
  
  *ret = 0;
  return;
}


/* Read from a binary file. Return zero on success */
void READBINARYPOS_F90(int *pos, int *nbytes, void *data, int *ret)
{
  if(fseek(fd, (long int) (*pos), SEEK_SET) != 0)
    {
      *ret = -1;
      return;
    }

  if(fread(data, (size_t) 1, (size_t) (*nbytes), fd) != (size_t) (*nbytes))
    {
      *ret = -2;
      return;
    }

  *ret = 0;
  return;
}

/* Skip the specified number of bytes from the current position */
void SKIPBYTES_F90(int *nbytes, int *ret)
{
  if(fseek(fd, (long int) (*nbytes), SEEK_CUR) != 0)
    {
      *ret = -1;
      return;
    } 
  *ret = 0;
  return;
}

