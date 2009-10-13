## --------------------------------------------------------------------------
##
## RVG frequency table
##
## --------------------------------------------------------------------------

rvgt.ftable <- function (n, rep=1, rdist, qdist, pdist, ..., breaks=101)

  ## ------------------------------------------------------------------------
  ## Create RVG frequency table for random variates generator.
  ## Each row contains the frequencies for one sample of size n.
  ## ------------------------------------------------------------------------
  ## n      : Size of random sample at each repetition
  ## rep    : Number of repetitions
  ## rdist  : Random variate generator  
  ## qdist  : Quantile function of distribution
  ## pdist  : Cumulative distribution function of distribution
  ## ....   : Parameters of distribution
  ## breaks : Number of breaks of histogram (see also R base function hist)
  ## ------------------------------------------------------------------------
  ## return:
  ## (rep x (breaks-1))-matrix of frequencies where each row contains
  ##   frequencies of sample of size n
  ## ------------------------------------------------------------------------
{
  ## sample size
  if (!is.numeric(n) || n<1 || n!=round(n))
    stop ("Invalid argument 'n'.")

  ## number of repetitions
  if (!is.numeric(rep) || rep<1 || rep!=round(rep))
    stop ("Invalid argument 'rep'.")

  ## number of break points
  if (!is.numeric(breaks) || !all(breaks>=1) || !all(breaks==round(breaks)))
    stop ("Invalid argument 'breaks'.")
  if (length(breaks) > 1)
    stop ("Cannot handle list of break points. Use number of break points instead.")
  if (breaks < 3) 
    stop (paste("Number of break points too small:",breaks))
  nbreaks <- breaks

  ## random variate generator
  if( missing(rdist) || !is.function(rdist))
    stop ("Argument 'rdist' missing or invalid.")

  ## quantile and distribution function
  if( missing(qdist) && missing(pdist) )
    stop ("Argument 'qdist' or 'pdist' required.")

  if( !missing(qdist) && !is.function(qdist))
    stop ("Argument 'qdist' invalid.")
  if( !missing(pdist) && !is.function(pdist))
    stop ("Argument 'pdist' invalid.")

  
  ## number of bins
  nbins <- nbreaks-1
  
  ## equidistributed break points for uniform scale
  ubreaks <- (0:(nbreaks-1))/nbins

  ## break points for x scale
  if (missing(qdist)) 
    xbreaks <- rep(NA,nbins)
  else
    xbreaks <- qdist(ubreaks,...)

  ## table for storing frequencies
  count <- matrix(0,nrow=rep,ncol=nbins)
     
  ## loop for each row of table
  for (i in 1:rep) {
    ## random sample of size n
    x <- rdist(n,...)
         
    ## get row
    if (!all(is.na(xbreaks))) {
      ## we can construct the histogram using the x-values
      count[i,] <- .Call("rvgt_bincount",x,xbreaks,PACKAGE="rvgtest")
    }
    else {
      ## otherwise we first have to transform the x-values into
      ## uniformly distributed u-values
      ## (slower but more robust for densities with poles)

      ## get frequency table (using function hist)
      u <- pdist(x,...)
      count[i,] <- .Call("rvgt_bincount",u,ubreaks,PACKAGE="rvgtest")
    }
  }
        
  ## return result as object of class "rvgt.ftable"
  ftable <- list(n=n,rep=rep,ubreaks=ubreaks,xbreaks=xbreaks,count=count)
  class(ftable) <- "rvgt.ftable"

  return(ftable)
}

## --------------------------------------------------------------------------
