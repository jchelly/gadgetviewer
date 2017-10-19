#include <stdlib.h>
#include <stdio.h>
#include "../../config.h"
#include "set_event_handler.h"
#define SETEVENTHANDLER_F90 FC_FUNC (seteventhandler, SETEVENTHANDLER)
#define NOEVENTHANDLER_F90  FC_FUNC (noeventhandler,  NOEVENTHANDLER)

/* Set non-zero to ignore events */
int ignore_events    = 0;
int in_event_handler = 0;

/* Pointer to the event handler functions */
void (*event_handler)();
void (*event_handler_f90)();

/* Function called when there's an event */
void process_event()
{
  if(in_event_handler == 0)
    {
      in_event_handler = 1;
      (*event_handler_f90)();
      in_event_handler = 0;
    }
  else
    {
      /* printf("Recursive call to event handler!\n"); */
    }
}

/* Fortran callable function to set the event handler */
void SETEVENTHANDLER_F90(void (*ptr)())
{
  event_handler     = &process_event;
  event_handler_f90 = ptr;
}

/* Fortran callable function to use if no event handler is needed */
void NOEVENTHANDLER_F90()
{
  event_handler = NULL;
}
