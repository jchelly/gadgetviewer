#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include "../../config.h"
#include "tick_interval.h"
#ifdef HAVE_CAIRO
#include <cairo.h>
#ifdef CAIRO_HAS_PS_SURFACE
#include <cairo-ps.h>
#endif
#endif

#ifdef HAVE_CAIRO
/* Cairo image surface */
static cairo_surface_t *surface     = NULL;
static cairo_t         *cr          = NULL;
static char            *buf         = NULL;
static int              buffer_size = 0;
/* Gadgetviewer image buffer */
static int image_w, image_h;
static int stride;
static char *image_ptr;
/* Colour map */
static int ncolours;
static int *colour_map = NULL;
/* Font information */
static double textheight;
static double textwidth;
static cairo_matrix_t font_matrix;
/* Whether we're writing to a PS file */
static int postscript = 0;
/* Graph axis range */
static double xaxis_min, xaxis_max;
static double yaxis_min, yaxis_max;
/* Graph location */
static double graph_x, graph_y;
static double graph_w, graph_h;
#endif

/*
  Make format string for writing tickmarks
*/
void tick_format(double ticksize, char *str, int len)
{
  int n = fabs(floor(log10(ticksize)));
  if(n < 0)
    n = 0;
  sprintf(str, "%%-20.%df", n);
}

void trim(char *str)
{
  int i;
  i = strlen(str)-1;
  while(str[i]==' ' && i > 0)
    {
      str[i]=(char) 0;
      i -= 1;
    }
}

/*
  Decide on factor to divide out from axis labels
*/
int axis_factor(double rmin, double rmax)
{
  int ifac;

  if(rmin==0 && rmax==0)
    ifac = 0;
  else if(rmin==0)
    ifac = log10(fabs(rmax));
  else if(rmax==0)
    ifac = log10(fabs(rmin));
  else
    if(fabs(rmin) > fabs(rmax))
      ifac = (int) log10(fabs(rmin));
    else
      ifac = (int) log10(fabs(rmax));

  if(ifac < 2 && ifac > -2)
    ifac = 0;

  return ifac;
}

/*
  Set up for plotting to memory
*/
void cdinitmem_cairo(char *image, int *width, int *height, int *ncol, int *map)
{
#ifdef HAVE_CAIRO

  int req_size;
  int i, j, k;
  double w, h, sx, sy;

  /*
    Determine size of buffer we need. Note: CAIRO_FORMAT_RGB24 actually
    indicates that we have 32 bits per pixel, not 24. It just ignores
    the first 8.
  */

#if CAIRO_VERSION >= CAIRO_VERSION_ENCODE(1,6,0)
  /* In recent versions of Cairo can query for stride */
  stride   = cairo_format_stride_for_width(CAIRO_FORMAT_ARGB32, *width);
#else
  /* If Cairo is older than v1.6 , just have to assume multiple of 4 bytes
     is ok */
  stride   = (*width) * 4;
#endif

  req_size = stride * (*height);

  image_w = *width;
  image_h = *height;
  image_ptr = image;

  /* Make sure the buffer is the right size */
  if(req_size != buffer_size)
    {
      if(buf)
	{
	  free(buf);
	  buf = NULL;
	  cairo_destroy(cr);
	  cairo_surface_destroy(surface);
	}
      buf = malloc(req_size);

      /* Make the new Cairo surface */
      surface = cairo_image_surface_create_for_data ((unsigned char *) buf,
						     CAIRO_FORMAT_ARGB32,
						     *width, *height, stride);
      cr = cairo_create(surface);

      /* Set scale factor so all coordinates are in range 0 to 1 */
      cairo_scale(cr, ((double) (*width)), ((double) (*height)));

    }

  cairo_set_operator (cr, CAIRO_OPERATOR_SOURCE);

  /* Copy the image into the buffer. Order of colour components in
     Cairo depends on endianness */
#ifdef WORDS_BIGENDIAN
  for(i=0;i<(*height);i++)
    for(j=0;j<(*width);j++)
      {
	buf[i*stride+j*4+0] = (char) 0;
	for(k=1;k<4;k++)
	  buf[i*stride+j*4+k] = image[3*(*width)*i + 3*j+k-1];	
      }
#else
  for(i=0;i<(*height);i++)
    for(j=0;j<(*width);j++)
      {
	buf[i*stride+j*4+3-0] = (char) 0;
	for(k=1;k<4;k++)
	  buf[i*stride+j*4+3-k] = image[3*(*width)*i + 3*j+k-1];
      }
#endif

  /* Tell Cairo we've altered the image buffer directly */
  cairo_surface_mark_dirty(surface);

  /* Set line width to 1.2 pixels */
  w = 1.2;
  h = 1.2;
  cairo_device_to_user_distance(cr, &w, &h);
  cairo_set_line_width(cr, w);

  /* Choose text font */
  cairo_select_font_face(cr,"sans-serif",
			 CAIRO_FONT_SLANT_NORMAL,CAIRO_FONT_WEIGHT_NORMAL);

  /* Set size, taking aspect ratio into account */
  sx = 14.0/((double) *width);
  sy = 14.0/((double) *height);
  cairo_matrix_init_scale(&font_matrix, sx, sy);
  cairo_set_font_matrix(cr, &font_matrix);
  textheight = sy;
  textwidth  = sx;

  /* Store colour map info */
  if(colour_map)free(colour_map);
  ncolours = *ncol;
  colour_map = malloc(ncolours*3*sizeof(int));
  for(i=0;i<ncolours;i++)
    for(j=0;j<3;j++)
      colour_map[3*i+j] = map[3*i+j];

  postscript = 0;
#endif
}

/* 
   Set up for plotting to postscript
*/
void cdinitps_cairo(char *filename, int *ncol, int *map)
{
#ifdef HAVE_CAIRO
#ifdef CAIRO_HAS_PS_SURFACE

  double sx, sy;
  int i, j;

  /* Size of page in points */
  double pswidth  = 8.0*72.0;
  double psheight = 6.0*72.0;

  /* Create postscript image surface */
  surface = cairo_ps_surface_create(filename, pswidth, psheight);
  cr = cairo_create(surface);
  cairo_set_operator (cr, CAIRO_OPERATOR_SOURCE);

  /* Set scale factor so all coordinates are in range 0 to 1 */
  cairo_scale(cr, pswidth, psheight);

  /* Set line width */
  cairo_set_line_width(cr, (72.0/300.0) / psheight);

  /* Choose text font */
  cairo_select_font_face(cr,"sans-serif",
			 CAIRO_FONT_SLANT_NORMAL,CAIRO_FONT_WEIGHT_NORMAL);

  /* Set font size, taking aspect ratio into account */
  sx = 10.0/pswidth;
  sy = 10.0/psheight;
  cairo_matrix_init_scale(&font_matrix, sx, sy);
  cairo_set_font_matrix(cr, &font_matrix);
  textheight = sy;
  textwidth  = sx;

  /* Store colour map info */
  if(colour_map)free(colour_map);
  ncolours = *ncol;
  colour_map = malloc(ncolours*3*sizeof(int));
  for(i=0;i<ncolours;i++)
    for(j=0;j<3;j++)
      colour_map[3*i+j] = map[3*i+j];

  postscript = 1;
#endif
#endif
}

/*
  Draw text at the specified location
*/
void cdtext_cairo(char *text, double *x, double *y, int *col)
{
#ifdef HAVE_CAIRO

#ifndef CAIRO_HAS_PS_SURFACE
  if(postscript)return;
#endif

  cairo_set_source_rgb (cr, 
			((double) colour_map[3*(*col)+0]) / 255.0,
			((double) colour_map[3*(*col)+1]) / 255.0,
			((double) colour_map[3*(*col)+2]) / 255.0);
  cairo_move_to(cr, *x, 1.0-(*y));
  cairo_show_text(cr, text);
#endif
}

/*
  Draw a line
*/
void cdline_cairo(double *x, double *y, int *col, int *n)
{
  int i;

#ifdef HAVE_CAIRO
#ifndef CAIRO_HAS_PS_SURFACE
  if(postscript)return;
#endif
  cairo_set_source_rgb (cr, 
			((double) colour_map[3*(*col)+0]) / 255.0,
			((double) colour_map[3*(*col)+1]) / 255.0,
			((double) colour_map[3*(*col)+2]) / 255.0);
  cairo_move_to(cr, x[0], 1.0-y[0]);
  for(i=1;i<(*n);i++)
    cairo_line_to(cr, x[i], 1.0-y[i]);
  cairo_stroke(cr);
#endif
}

/*
  Draw box with axes.
*/
void cdbox_cairo(double *x1, double *y1, double *width, double *height,
		  double *xmin, double *xmax, double *ymin, double *ymax,
		  char *title, char *xlabel, char *ylabel, int *col,
		  int *xtick1, int *ytick1)
{
#ifdef HAVE_CAIRO
  double x, y;
  cairo_matrix_t matrix;
  int ntick_x, ntick_y;
#ifndef CAIRO_HAS_PS_SURFACE
  if(postscript)return;
#endif
  x = *x1;
  y = 1.0 - *y1;

  /* Decide number of tickmarks - use fewer for small boxes */
  if (*width > 0.5)
    ntick_x = 10;
  else
    ntick_x = 2;
  if (*height > 0.5)
    ntick_y = 10;
  else
    ntick_y = 2;

  cairo_set_source_rgb (cr, 
			((double) colour_map[3*(*col)+0]) / 255.0,
			((double) colour_map[3*(*col)+1]) / 255.0,
			((double) colour_map[3*(*col)+2]) / 255.0);

  /* Draw box */
  cairo_move_to(cr, x, y);
  cairo_line_to(cr, x+*width, y);
  cairo_line_to(cr, x+*width, y-*height);
  cairo_line_to(cr, x, y-*height);
  cairo_line_to(cr, x, y);
  cairo_stroke(cr);

  /* Draw title */
  cairo_move_to(cr, x, y-*height-0.5*textheight);
  cairo_show_text(cr, title);

  /* Draw x label */
  cairo_move_to(cr, x+0.5*(*width)-0.5*0.5*textwidth*strlen(xlabel), 
		y+3.0*textheight);
  cairo_show_text(cr, xlabel);

  /* Draw y label */
  cairo_move_to(cr, x-3*0.7*textwidth-0.7*0.5*textheight, 
		y-0.5*(*height)+0.7*0.5*textwidth*strlen(ylabel) +
		0.7*8.0*textwidth);
  matrix = font_matrix;
  cairo_matrix_rotate(&matrix, -0.5*3.14159);
  cairo_set_font_matrix(cr, &matrix);
  cairo_show_text(cr, ylabel);
  cairo_set_font_matrix(cr, &font_matrix);

  /* Set small font for x axis labels */
  matrix = font_matrix;
  cairo_matrix_scale(&matrix, 0.7, 0.7);
  cairo_set_font_matrix(cr, &matrix);

  /* Figure out where to put major x tickmarks */
  if ( *xtick1 && *xmin != *xmax)
    {
      double rmin  = *xmax > *xmin ? *xmin : *xmax;
      double rmax  = *xmax > *xmin ? *xmax : *xmin;
      double xtick = tick_interval(*xmin, *xmax, ntick_x);
      int    ifac  = axis_factor(*xmin, *xmax);
      double xt = (int) (*xmin / xtick) * xtick;
      char   fmt[100];

      tick_format(xtick/pow(10,ifac), fmt, 100);

      while(xt<rmin || xt>rmax)
	xt = xt + xtick;
      while(xt>=rmin && xt<=rmax)
	{
	  double xpos = x + *width * (xt-*xmin)/(*xmax-*xmin);
	  char str[100];
	  cairo_move_to(cr, xpos, y);
	  cairo_line_to(cr, xpos, y-0.5*textheight);
	  cairo_move_to(cr, xpos, y-*height);
	  cairo_line_to(cr, xpos, y-*height+0.5*textheight);

	  /* Label with value */
	  sprintf(str, fmt, xt/pow(10,ifac));
	  trim(str);
	  cairo_move_to(cr, xpos-0.25*strlen(str)*textwidth, y+1.5*textheight);
	  cairo_show_text(cr,str);

	  xt = xt + xtick;
	}
      cairo_stroke(cr);
  
      /* Minor x ticks */
      xt = (int) (*xmin / xtick) * xtick;
      while(xt<rmin || xt>rmax)
	xt = xt + xtick / 5.0;
      while(xt>=rmin && xt<=rmax)
	{
	  double xpos = x + *width * (xt-*xmin)/(*xmax-*xmin);
	  cairo_move_to(cr, xpos, y);
	  cairo_line_to(cr, xpos, y-0.25*textheight);
	  cairo_move_to(cr, xpos, y-*height);
	  cairo_line_to(cr, xpos, y-*height+0.25*textheight);
	  xt = xt + xtick / 5.0;
	}
      cairo_stroke(cr);
      if(ifac != 0)
	{
	  char str[100];
	  cairo_move_to(cr, x+*width-3*textwidth, y+2.5*textheight);
	  sprintf(str,"x %-10.0e",pow(10,ifac));
	  cairo_show_text(cr,str);
	}
    }

  /* Set small font for y axis labels */
  matrix = font_matrix;
  cairo_matrix_scale(&matrix, 0.7, 0.7);
  cairo_set_font_matrix(cr, &matrix);

  /* Major y tickmarks */
  if(*ytick1 && *ymin != *ymax)
    {
      double rmin = *ymax > *ymin ? *ymin : *ymax;
      double rmax = *ymax > *ymin ? *ymax : *ymin;
      double ytick = tick_interval(*ymin, *ymax, ntick_y);
      int    ifac  = axis_factor(*ymin, *ymax);
      double yt = (int) (*ymin / ytick) * ytick; 
      char   fmt[100];

      tick_format(ytick/pow(10,ifac), fmt, 100);

      while(yt<rmin || yt>rmax)
	yt = yt + ytick;
      while(yt>=rmin && yt<=rmax)
	{
	  char str[100];
	  double ypos = y - *height * (yt-*ymin)/(*ymax-*ymin);
	  cairo_move_to(cr, x, ypos);
	  cairo_line_to(cr, x+0.5*textwidth, ypos);
	  cairo_move_to(cr, x+*width,ypos);
	  cairo_line_to(cr, x+*width-0.5*textwidth,ypos);
	  /* Label with value */
	  cairo_move_to(cr, x-3*0.7*textwidth, ypos+0.7*0.5*textheight);
	  sprintf(str, fmt, yt/pow(10,ifac));
	  trim(str);
	  cairo_show_text(cr,str);

	  yt = yt + ytick;
	}
      cairo_stroke(cr);
  
      /* Figure out where to put minor y tickmarks */
      yt = (int) (*ymin / ytick) * ytick; 
      while(yt<rmin || yt>rmax)
	yt = yt + ytick / 5.0;
      while(yt>=rmin && yt<=rmax)
	{
	  double ypos = y - *height * (yt-*ymin)/(*ymax-*ymin);
	  cairo_move_to(cr, x, ypos);
	  cairo_line_to(cr, x+0.25*textwidth, ypos);
	  cairo_move_to(cr, x+*width,ypos);
	  cairo_line_to(cr, x+*width-0.25*textwidth,ypos);
	  yt = yt + ytick / 5.0;
	}
      cairo_stroke(cr);

      if(ifac != 0)
	{
	  char str[100];
	  /* Set small vertical font for scale factor */
	  matrix = font_matrix;
	  cairo_matrix_rotate(&matrix, -0.5*3.14159);
	  cairo_matrix_scale(&matrix, 0.7, 0.7);
	  cairo_set_font_matrix(cr, &matrix);
	  cairo_move_to(cr, x-2.5*textwidth, y-*height+3*textheight);
	  sprintf(str,"x %-10.0e",pow(10,ifac));
	  cairo_show_text(cr,str);
	}

    }

  /* Restore default font */
  cairo_set_font_matrix(cr, &font_matrix);

  /* Record axis range */
  xaxis_min = *xmin;
  xaxis_max = *xmax;
  yaxis_min = *ymin;
  yaxis_max = *ymax;

  /* Record location of graph on image surface */
  graph_x = x;
  graph_y = y;
  graph_w = *width;
  graph_h = *height;

#endif
}

/*
  Make a scatterplot
*/
void cdpoints_cairo(int *n, double *x, double *y, int *col)
{
#ifdef HAVE_CAIRO

#ifndef CAIRO_HAS_PS_SURFACE
  if(postscript)return;
#endif
  if(postscript)
    {
      int i;
      cairo_set_source_rgb (cr, 
			    ((double) colour_map[3*(*col)+0]) / 255.0,
			    ((double) colour_map[3*(*col)+1]) / 255.0,
			    ((double) colour_map[3*(*col)+2]) / 255.0);

      /*
	When doing postscript output draw points as crosses one tenth the
	size of characters.
      */
      /*
      double dx = 0.1*textwidth;
      double dy = 0.1*textheight;
      int i;
      for(i=0;i<*n;i++)
	{
	  double cx = graph_x + 
	    (x[i]-xaxis_min)/(xaxis_max-xaxis_min) * graph_w;
	  double cy = graph_y -
	    (y[i]-yaxis_min)/(yaxis_max-yaxis_min) * graph_h;
	  cairo_move_to(cr, cx, cy-dy);
	  cairo_line_to(cr, cx, cy+dy);
	  cairo_move_to(cr,cx-dx, cy);
	  cairo_line_to(cr,cx+dx, cy);
	}
      */

      /*
	Can draw points by making zero length line segments
	with rounded ends
      */
      cairo_set_line_cap(cr, CAIRO_LINE_CAP_ROUND);
      for(i=0;i<*n;i++)
	{
	  double cx = graph_x + 
	    (x[i]-xaxis_min)/(xaxis_max-xaxis_min) * graph_w;
	  double cy = graph_y -
	    (y[i]-yaxis_min)/(yaxis_max-yaxis_min) * graph_h;
	  cairo_move_to(cr, cx, cy);
	  cairo_line_to(cr, cx, cy);
	}
      cairo_stroke(cr);
      cairo_set_line_cap(cr, CAIRO_LINE_CAP_BUTT);

    }
  else
    {
      /*
	If we're plotting to memory draw points by accessing the
	buffer directly and setting single pixels.
      */
      int c[3];
      int i, k;
      int top    = (graph_y - graph_h) * image_h;
      int bottom = graph_y * image_h;
      int left   = graph_x * image_w;
      int right  = (graph_x + graph_w) * image_w;

      c[0] = colour_map[3*(*col)+0];
      c[1] = colour_map[3*(*col)+1];
      c[2] = colour_map[3*(*col)+2];

      for(i=0;i<*n;i++)
	{
	  double cx = graph_x + 
	    (x[i]-xaxis_min)/(xaxis_max-xaxis_min) * graph_w;
	  double cy = graph_y - 
	    (y[i]-yaxis_min)/(yaxis_max-yaxis_min) * graph_h;
	  int ix = (int) (cx*image_w);
	  int iy = (int) (cy*image_h);
	  if((ix > left) && (ix < right) && (iy < bottom) && (iy > top))
	    {
	      for(k=1;k<4;k++)
#ifdef WORDS_BIGENDIAN
		buf[iy*stride+ix*4+k] = c[k-1];	
#else
	        buf[iy*stride+ix*4+3-k] = c[k-1];	
#endif
	    }
	}
      cairo_surface_mark_dirty(surface);
    }

#endif
}

/*
  Make a histogram
*/
void cdhist_cairo(int *n, double *x, double *y, int *col)
{
#ifdef HAVE_CAIRO
  double dx;
  int i;
#ifndef CAIRO_HAS_PS_SURFACE
  if(postscript)return;
#endif

  cairo_set_source_rgb (cr, 
			((double) colour_map[3*(*col)+0]) / 255.0,
			((double) colour_map[3*(*col)+1]) / 255.0,
			((double) colour_map[3*(*col)+2]) / 255.0);

  /* Get bin size (assume all the same size) */
  if(*n > 1)
    dx = x[1]-x[0];
  else
    dx = xaxis_max-xaxis_min;

  /* Loop over bins */
  for(i=0; i<*n; i++)
    {
      double x1 = x[i];
      double x2 = x[i] + dx;
      double y1 = yaxis_min;
      double y2 = y[i];
      
      /* Convert to normalised coords */
      x1 = graph_x + (x1-xaxis_min)/(xaxis_max-xaxis_min) * graph_w;
      x2 = graph_x + (x2-xaxis_min)/(xaxis_max-xaxis_min) * graph_w;
      y1 = graph_y - (y1-yaxis_min)/(yaxis_max-yaxis_min) * graph_h;
      y2 = graph_y - (y2-yaxis_min)/(yaxis_max-yaxis_min) * graph_h;
      
      if(y2 <= y1 - graph_h)
	{
	  y2 = y1 - graph_h;
	  cairo_move_to(cr, x1, y1);
	  cairo_line_to(cr, x1, y2);
	  cairo_move_to(cr, x2, y2);
	  cairo_line_to(cr, x2, y1);
	}
      else if(y2 <= y1)
	{
	  cairo_move_to(cr, x1, y1);
	  cairo_line_to(cr, x1, y2);
	  cairo_line_to(cr, x2, y2);
	  cairo_line_to(cr, x2, y1);
	}
      
    }
  cairo_stroke(cr);
  
#endif
}

/*
  Finish plotting
*/
void cdend_cairo(void)
{
#ifdef HAVE_CAIRO
#ifndef CAIRO_HAS_PS_SURFACE
  if(postscript)return;
#endif
  cairo_surface_flush(surface);

  if(!postscript)
    {
      /* Copy the buffer back to the image */
#ifdef WORDS_BIGENDIAN
      int i, j, k;
      for(i=0;i<image_h;i++)
	for(j=0;j<image_w;j++)
	  {
	for(k=1;k<4;k++)
	  image_ptr[3*image_w*i + 3*j+k-1] = buf[i*stride+j*4+k];
	  }
#else
      int i, j, k;
      for(i=0;i<image_h;i++)
	for(j=0;j<image_w;j++)
	  {
	    for(k=1;k<4;k++)
	      image_ptr[3*image_w*i + 3*j+k-1] = buf[i*stride+j*4+3-k];
	  }
#endif
    }
  else
    {
#ifdef CAIRO_HAS_PS_SURFACE
      /* Close postscript file */
      cairo_show_page(cr);
      cairo_surface_destroy(surface);
      cairo_destroy(cr);
#endif
    }

#endif
}

/*
  Report drawing library in use
*/
void cdversion_cairo(char *str, int *len)
{
#ifdef HAVE_CAIRO
  char tmp[500];
#ifdef CAIRO_HAS_PS_SURFACE
  sprintf(tmp,"Cairo version %s (postscript enabled)", 
	  cairo_version_string());
#else
    sprintf(tmp,"Cairo version %s (postscript disabled)", 
	    cairo_version_string());
#endif
  strncpy(str, tmp, *len);
#else
  strncpy(str, "None", *len);
#endif
}
