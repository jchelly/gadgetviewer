#include <stdlib.h>
#include <string.h>
#include "../../config.h"
#include "tick_interval.h"
#ifdef HAVE_PLPLOT
#include <plplot.h>
#endif

/*
  Set up for plotting to memory
*/
void cdinitmem_plplot(char *image, int *width, int *height, int *ncol, int *map)
{
#ifdef HAVE_PLPLOT
  PLINT *r;
  PLINT *g;
  PLINT *b;
  int i;

  plsdev("mem");
  plsmem((PLINT) *width,(PLINT) *height,(void *) image);

  /* Set up colour map */
  plscmap0n((PLINT) *ncol);
  r = malloc((*ncol)*sizeof(PLINT));
  g = malloc((*ncol)*sizeof(PLINT));
  b = malloc((*ncol)*sizeof(PLINT));
  for(i=0;i<(*ncol);i++)
    {
      r[i] = (PLINT) map[3*i+0];
      g[i] = (PLINT) map[3*i+1];
      b[i] = (PLINT) map[3*i+2];
    }
  plscmap0 (r, g, b, (PLINT) *ncol);
  plinit();
  pladv(0);
  free(r);
  free(g);
  free(b);
#endif
}

/* 
   Set up for plotting to postscript
*/
void cdinitps_plplot(char *filename, int *ncol, int *map)
{
#ifdef HAVE_PLPLOT
  PLINT *r;
  PLINT *g;
  PLINT *b;
  int i;
  plsdev("psc");
  plsetopt("o", filename);
  /* Set up colour map */
  plscmap0n((PLINT) *ncol);
  r = malloc((*ncol)*sizeof(PLINT));
  g = malloc((*ncol)*sizeof(PLINT));
  b = malloc((*ncol)*sizeof(PLINT));
  for(i=0;i<(*ncol);i++)
    {
      r[i] = (PLINT) map[3*i+0];
      g[i] = (PLINT) map[3*i+1];
      b[i] = (PLINT) map[3*i+2];
    }
  plscmap0 (r, g, b, (PLINT) *ncol);
  plinit();
  pladv(0);
#endif
}

/*
  Draw text at the specified location
*/
void cdtext_plplot(char *text, double *x, double *y, int *col)
{
#ifdef HAVE_PLPLOT
  plschr ((PLFLT) 1.0, (PLFLT) 2.0);
  plcol0(*col);
  plvpor(0.0, 1.0, 0.0, 1.0);
  plwind(0.0, 1.0, 0.0, 1.0);
  plptex(*x, *y, 1.0, 0.0, 0.0, text);
#endif
}

/*
  Draw a line
*/
void cdline_plplot(double *x, double *y, int *col, int *n)
{
#ifdef HAVE_PLPLOT
  PLINT num = (*n);
  PLFLT *x1 = malloc(num*sizeof(PLFLT));
  PLFLT *y1 = malloc(num*sizeof(PLFLT));
  int i;
  plcol0(*col);
  for(i=0;i<num;i++)
    {
      x1[i] = x[i];
      y1[i] = y[i];
    }

  plvpor(0.0, 1.0, 0.0, 1.0);
  plwind(0.0, 1.0, 0.0, 1.0);
  plline(num,x1,y1);
  free(x1);
  free(y1);
#endif
}

/*
  Draw box with axes.
*/
void cdbox_plplot(double *x, double *y, double *width, double *height,
		  double *xmin, double *xmax, double *ymin, double *ymax,
		  char *title, char *xlabel, char *ylabel, int *col,
		  int *xtick1, int *ytick1)
{
#ifdef HAVE_PLPLOT
  char xopt[10];
  char yopt[10];
  int ntick_x, ntick_y;
  PLFLT xtick, ytick;

  plvpor((PLFLT) *x, (PLFLT) ((*x)+(*width)), 
	 (PLFLT) *y, (PLFLT) ((*y)+(*height)));
  plwind((PLFLT) *xmin, (PLFLT) *xmax, (PLFLT) *ymin, (PLFLT) *ymax);
  plcol0(*col);
 
  
  plschr ((PLFLT) 1.0, (PLFLT) 2.0);

  if(*xtick1)
    strcpy(xopt,"bcstn");
  else
    strcpy(xopt,"bc");
  if(*ytick1)
    strcpy(yopt,"bcstnv");
  else
    strcpy(yopt,"bc");


  /* Decide tick intervals */
  if (*width > 0.5)
    ntick_x = 10;
  else
    ntick_x = 2;
  if (*height > 0.5)
    ntick_y = 10;
  else
    ntick_y = 2;
  xtick = tick_interval(*xmin, *xmax, ntick_x);
  ytick = tick_interval(*ymin, *ymax, ntick_y);

  plbox (xopt, xtick, (PLINT) 0, yopt, ytick, (PLINT) 0);
  pllab (xlabel, ylabel, title);
#endif
}

/*
  Make a scatterplot
*/
void cdpoints_plplot(int *n, double *x, double *y, int *col)
{
#ifdef HAVE_PLPLOT
  int i;
  plcol0(*col);
  for(i=0;i<(*n);i++)
    {
      PLFLT xp = x[i];
      PLFLT yp = y[i];
      plpoin(1, &xp, &yp, 1);
    }
#endif
}

/*
  Make a histogram
*/
void cdhist_plplot(int *n, double *x, double *y, int *col)
{
#ifdef HAVE_PLPLOT
  PLINT np = *n;
  PLFLT *xp = malloc(np*sizeof(PLFLT));
  PLFLT *yp = malloc(np*sizeof(PLFLT));
  int i;
  plcol0(*col);
  for(i=0;i<np;i++)
    {
      xp[i] = x[i];
      yp[i] = y[i];
    }
  plbin(np, xp, yp, 0);
  free(xp);
  free(yp);
#endif
}

/*
  Finish plotting
*/
void cdend_plplot(void)
{
#ifdef HAVE_PLPLOT
  plend();
#endif
}

/*
  Report drawing library in use
*/
void cdversion_plplot(char *str, int *len)
{
#ifdef HAVE_PLPLOT
  char ver[80];
  char tmp[500];
  plgver(ver);
  sprintf(tmp, "PLPlot version %s", ver);
  strncpy(str, tmp, *len);
#else
  strncpy(str, "None", *len);
#endif
}
