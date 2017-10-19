#include <math.h>

/*
  Decide on tick intervals for a given range. Tries to get close
  to having nticks intervals in the range.
*/
double tick_interval(double rmin, double rmax, int nticks)
{
  int    ndiv[7]  = {1,2,5,10,20,50,100};
  double range    = fabs(rmax-rmin);
  int    ipow     = (int) log10(range);
  double interval = pow(10,(double) ipow);
  double rbest    = 1.0e10;
  int    ibest    = -1;
  int i;

  for(i=0;i<7;i++)
    {
      double r = fabs(range/(interval/ndiv[i]) - ((double) nticks));
      if((r < rbest) && (range/(interval/ndiv[i]) >= 2.0))
	{
	  rbest = r;
	  ibest = i;
	}
    }
  interval = interval / ndiv[ibest];

  return interval;
}
