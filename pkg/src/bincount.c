/*---------------------------------------------------------------------------*/
/* binning                                                                   */
/*---------------------------------------------------------------------------*/

/* #include <R.h> */
#include <Rdefines.h>
/* #include <Rinternals.h> */

/*---------------------------------------------------------------------------*/

SEXP
rvgt_bincount (SEXP sexp_x, SEXP sexp_breaks)
/*----------------------------------------------------------------------*/
/* Compute bin counts for histogram.                                    */
/*                                                                      */
/* Rules for counting:                                                  */
/*  - right boundary point belong to bin                                */
/*  - data out of break points belong to first and last bin, resp.      */
/*                                                                      */
/* Parameters:                                                          */
/*   x      ... sample                                                  */
/*   breaks ... break points for histogram                              */
/*              [ in increasing order. NOT CHECKED! ]                   */
/*                                                                      */
/* Return:                                                              */
/*   array for storing bin counts                                       */
/*                                                                      */
/* Remark:                                                              */
/*   R function 'hist' is not appropriate for our task as it adds some  */
/*   fuzzy to the break points of the histogram.                        */
/*----------------------------------------------------------------------*/
{
  SEXP sexp_counts = R_NilValue;     /* array for storing bin counts */
  double *count;
  int nc;            /* number of bins */

  double *x;         /* pointer to sample */
  int nx;            /* size of data array */

  double *breaks;    /* pointer to break points */
  int nbreaks;       /* number of break points */

  int i, lo, hi, new;

  /* extract and check sample 'x' */
  if (TYPEOF(sexp_x)!=REALSXP)
    error("_rvgt_bincount: argument 'x' invalid");
  x = NUMERIC_POINTER(sexp_x);
  nx = length(sexp_x);
  if (nx<1)
    error("_rvgt_bincount: argument 'x' has too few elements");

  /* extract and check 'breaks' */
  if (TYPEOF(sexp_breaks)!=REALSXP)
    error("_rvgt_bincount: argument 'breaks' invalid");
  breaks = NUMERIC_POINTER(sexp_breaks);
  nbreaks = length(sexp_breaks);
  if (nbreaks<3)
    error("_rvgt_bincount: argument 'breaks' has too few elements (< 3)");

  /* number of bins */
  nc = nbreaks - 1;

  /* create array for storing bin counts */
  PROTECT(sexp_counts = NEW_NUMERIC(nc));
  count = NUMERIC_POINTER(sexp_counts);
  for(i=0; i<nc; i++)
    count[i] = 0;

  /* Find bin using binary search.                     */
  /* Remark: guide table method is faster alternative. */
  for(i=0 ; i<nx ; i++) {

    if (breaks[0] >= x[i]) {
      ++(count[0]);
      continue;
    }
    if (breaks[nc] <= x[i]) {
      ++(count[nc-1]);
      continue;
    }

    lo = 0;
    hi = nc;

    while(hi-lo >= 2) {
      new = (hi+lo)/2;
      if(x[i] <= breaks[new]) 
	/* right boundary point belongs to bin */
	hi = new;
      else
	lo = new;
    }
    ++(count[lo]);
  }
  
  /* return bin counts to R */
  UNPROTECT(1);
  return sexp_counts;

} /* end of rvgt_bincount() */

/*---------------------------------------------------------------------------*/
