## --------------------------------------------------------------------------
##
## Table of u-errors
##
## --------------------------------------------------------------------------

uerror <- function (n, aqdist, pdist, ..., udom=c(0,1),
                    res=1000, tails=FALSE, plot=FALSE)

  ## ------------------------------------------------------------------------
  ## Create table of u-errors for numerical inversion method.
  ## ------------------------------------------------------------------------
  ## n      : Size of random sample 
  ## aqdist : Approximate inverse distribution function (quantile function)
  ## pdist  : Cumulative distribution function of distribution
  ## ....   : Parameters of distribution
  ## udom   : domain for u
  ## res    : Resolution of table (number of intervals in [0,1] for which 
  ##          quantiles a sample of u-errors are computed and stored) 
  ## tails  : if TRUE, then the tail regions are treated more accurately
  ## plot   : if TRUE, then plot a u-errors
  ## ------------------------------------------------------------------------
  ## Return:
  ##   list of size 'res' that contains 'n', 'res', domain, kind="u-error" and
  ##   the quantiles of u-errors:
  ##     min, lqr (lower quartile), med, uqr (upper quartile), max 
  ## ------------------------------------------------------------------------
{
  ## sample size
  if (!is.numeric(n) || n<1 || n!=round(n))
    stop ("Invalid argument 'n'.")

  ## resolution
  if (!is.numeric(res) || res<1 || res!=round(res))
    stop ("Invalid argument 'rep'.")

  ## approximate inverse distribution function (quantile function)
  if( missing(aqdist) || !is.function(aqdist))
    stop ("Argument 'aqdist' missing or invalid.")

  ## distribution function
  if( missing(pdist) || !is.function(pdist))
    stop ("Argument 'pdist' missing or invalid.")

  ## domain
  umin <- max(0,udom[1])
  umax <- min(1,udom[2])
  if( umin>=umax) 
    stop ("Invalid argument 'udom'.")

  ## we do not treat tails more accurately
  ## when a smaller domain is given
  if(!isTRUE(all.equal(c(umin,umax),c(0,1))))
    tails <- FALSE
  
  ## interval boundaries
  length <- (umax-umin) / res
  uiv <- umin + length * (0:res)
  
  ## samplesize for each interval
  k <- round(n / res)
  if( k<1 ) stop ("Invalid arguments 'n' < 'rep'.")
  n <- k * res

  ## arrays for storing u-errors
  ue.min <- numeric(res)
  ue.lqr <- numeric(res)
  ue.med <- numeric(res)
  ue.uqr <- numeric(res)
  ue.max <- numeric(res)

  ## loop over intervals
  for (i in 1:res) {

    ## u values
    if (isTRUE(tails) && ( isTRUE(all.equal(i,1)) || isTRUE(all.equal(i,res)) )) {
      DBL.EPSILON <- 2^(-52)
      size <- round(n/2)
      if (isTRUE(all.equal(i,1))) {
        ivmin <- DBL.EPSILON
        ivmax <- 1/res
      }
      else {
        ivmin <- 1 - 1/res
        ivmax <- 1 - DBL.EPSILON
      }
      u <- ivmin + (0:(size-1)) * (ivmax - ivmin)/(size-1)
    }
    else {
      u <- uiv[i] + length * ((1:k) - 0.5)/k
    }

    ## compute u-error
    uerr <- abs(u - pdist(aqdist(u), ...))

    ## compute and store statistics
    udata <- quantile(uerr,c(0,0.25,0.5,0.75,1))

    ue.min[i] <- udata[[1]]
    ue.lqr[i] <- udata[[2]]
    ue.med[i] <- udata[[3]]
    ue.uqr[i] <- udata[[4]]
    ue.max[i] <- udata[[5]]
  }
  
  ## return result as object of class "rvgt.ierror"
  uerror <- list( n=n, res=res, udom=c(umin,umax), kind="u-error",
                  min=ue.min, lqr=ue.lqr, med=ue.med, uqr=ue.uqr, max=ue.max )
  class(uerror) <- "rvgt.ierror"
  
  ## plot u-errors
  if( isTRUE(plot) ) {
    plot(uerror)
  }

  ## return table of u-errors
  return(invisible(uerror))
}

## --------------------------------------------------------------------------
