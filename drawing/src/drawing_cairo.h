#ifndef _DRAWING_CAIRO_H_
#define _DRAWING_CAIRO_H_
void cdinitmem_cairo(char *image, int *width, int *height,
		     int *ncol, int *map);
void cdinitps_cairo(char *filename, int *ncol, int *map);
void cdtext_cairo(char *text, double *x, double *y, int *col);
void cdline_cairo(double *x, double *y, int *col, int *n);
void cdbox_cairo(double *x, double *y, double *width, double *height,
		  double *xmin, double *xmax, double *ymin, double *ymax,
		  char *title, char *xlabel, char *ylabel, int *col,
		  int *xtick, int *ytick);
void cdpoints_cairo(int *n, double *x, double *y, int *col);
void cdhist_cairo(int *n, double *x, double *y, int *col);
void cdend_cairo(void);
void cdversion_cairo(char *str, int *len);
#endif
