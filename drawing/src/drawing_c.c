#include <stdlib.h>
#include <string.h>
#include "../../config.h"
#include "drawing_plplot.h"
#include "drawing_cairo.h"

/*
  Set up for plotting to memory
*/
#define CDINITMEM_F90 FC_FUNC (cdinitmem, CDINITMEM)
void CDINITMEM_F90(char *image, int *width, int *height, int *ncol, int *map)
{
#ifdef HAVE_PLPLOT
  cdinitmem_plplot(image,width,height,ncol, map);
#else
#ifdef HAVE_CAIRO
  cdinitmem_cairo(image,width,height,ncol, map);
#endif
#endif
}

/* 
   Set up for plotting to postscript
*/
#define CDINITPS_F90 FC_FUNC (cdinitps, CDINITPS)
void CDINITPS_F90(char *filename, int *ncol, int *map)
{
#ifdef HAVE_PLPLOT
  cdinitps_plplot(filename,ncol,map);
#else
#ifdef HAVE_CAIRO
  cdinitps_cairo(filename,ncol,map);
#endif
#endif
}

/*
  Draw text at the specified location
*/
#define CDTEXT_F90 FC_FUNC (cdtext, CDTEXT)
void CDTEXT_F90(char *text, double *x, double *y, int *col)
{
#ifdef HAVE_PLPLOT
  cdtext_plplot(text,x,y,col);
#else
#ifdef HAVE_CAIRO
  cdtext_cairo(text,x,y,col);
#endif
#endif
}

/*
  Draw a line
*/
#define CDLINE_F90 FC_FUNC (cdline, CDLINE)
void CDLINE_F90(double *x, double *y, int *col, int *n)
{
#ifdef HAVE_PLPLOT
  cdline_plplot(x,y,col,n);
#else
#ifdef HAVE_CAIRO
  cdline_cairo(x,y,col,n);
#endif
#endif
}

/*
  Draw box with axes.
*/
#define CDBOX_F90 FC_FUNC (cdbox, CDBOX)
void CDBOX_F90(double *x, double *y, double *width, double *height,
	       double *xmin, double *xmax, double *ymin, double *ymax,
	       char *title, char *xlabel, char *ylabel, int *col,
	       int *xtick, int *ytick)
{
#ifdef HAVE_PLPLOT
  cdbox_plplot(x,y,width,height,xmin,xmax,ymin,ymax,title,xlabel,ylabel,
	       col,xtick,ytick);
#else
#ifdef HAVE_CAIRO
  cdbox_cairo(x,y,width,height,xmin,xmax,ymin,ymax,title,xlabel,ylabel,
	      col,xtick,ytick);
#endif
#endif
}

/*
  Make a scatterplot
*/
#define CDPOINTS_F90 FC_FUNC (cdpoints, CDPOINTS)
void CDPOINTS_F90(int *n, double *x, double *y, int *col)
{
#ifdef HAVE_PLPLOT
  cdpoints_plplot(n,x,y,col);
#else
#ifdef HAVE_CAIRO
  cdpoints_cairo(n,x,y,col);
#endif
#endif
}

/*
  Make a histogram
*/
#define CDHIST_F90 FC_FUNC (cdhist, CDHIST)
void CDHIST_F90(int *n, double *x, double *y, int *col)
{
#ifdef HAVE_PLPLOT
  cdhist_plplot(n,x,y,col);
#else
#ifdef HAVE_CAIRO
  cdhist_cairo(n,x,y,col);
#endif
#endif
}

/*
  Finish plotting
*/
#define CDEND_F90 FC_FUNC (cdend, CDEND)
void CDEND_F90(void)
{
#ifdef HAVE_PLPLOT
  cdend_plplot();
#else
#ifdef HAVE_CAIRO
  cdend_cairo();
#endif
#endif
}

/*
  Report drawing library in use
*/
#define CDVERSION_F90 FC_FUNC (cdversion, CDVERSION)
void CDVERSION_F90(char *str, int *len)
{
#ifdef HAVE_PLPLOT
  cdversion_plplot(str,len);
#else
#ifdef HAVE_CAIRO
  cdversion_cairo(str,len);
#else
  strncpy(str, "None", *len);
#endif
#endif
}
