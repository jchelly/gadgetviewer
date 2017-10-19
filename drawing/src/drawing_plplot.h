#ifndef _DRAWING_PLPLOT_H_
#define _DRAWING_PLPLOT_H_
void cdinitmem_plplot(char *image, int *width, int *height,
		      int *ncol, int *map);
void cdinitps_plplot(char *filename, int *ncol, int *map);
void cdtext_plplot(char *text, double *x, double *y, int *col);
void cdline_plplot(double *x, double *y, int *col, int *n);
void cdbox_plplot(double *x, double *y, double *width, double *height,
		  double *xmin, double *xmax, double *ymin, double *ymax,
		  char *title, char *xlabel, char *ylabel, int *col,
		  int *xtick, int *ytick);
void cdpoints_plplot(int *n, double *x, double *y, int *col);
void cdhist_plplot(int *n, double *x, double *y, int *col);
void cdend_plplot(void);
void cdversion_plplot(char *str, int *len);
#endif
