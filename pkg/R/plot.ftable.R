## --------------------------------------------------------------------------
##
## Plot RVG frequency table
##
## --------------------------------------------------------------------------

plot.rvgt.ftable <- function(x, rows, alpha=0.01, ...)

  ## ------------------------------------------------------------------------
  ## Plot frequencies in table 'x' (histogram).
  ## The plot range is the union of 2*the confidence intervals
  ## and the range of the frequencies.
  ## ------------------------------------------------------------------------
  ## x     : Object of class "rvgt.ftable" containing frequencies
  ## row   : row(s) for which the histogram should be plotted
  ## alpha : draw lines corresponding to confidence intervals
  ## ...   : further graphical parameters
  ## ------------------------------------------------------------------------
{
  ## check arguments
  if (alpha <= 0 || alpha > 0.1)
    stop ("Invalid argument 'alpha'.")

  ## get table
  table <- x$count
  
  ## number of bins
  m <- ncol(table)
  
  ## get list of rows
  if (missing(rows)) {
    ## use all
    rows <- 1:x$rep
  }
  else {
    if (!is.numeric(rows) || !all(rows>=1) || !all(rows<=x$rep))
      stop ("Invalid argument 'rows'.")
  }

  ## total samplesize
  n <- x$n * length(rows)

  ## break points
  ubreaks <- x$ubreaks

  ## expected probabilities
  p0 <- ubreaks[-1] - ubreaks[-(m+1)]
  
  ## get requested frequencies
  if (length(rows)==1) {
    freq <- table[rows,]
  }
  else {
    freq <- colSums(table[rows,])
  }

  ## normalize frequencies
  freq <- freq / (n * p0)

  ## maximum and minimum frequencies
  fmax <- max(freq)
  fmin <- min(freq)

  ## standard deviation of bin densities
  s <- sqrt(p0*(1-p0)/n) / p0

  ## half length of confidence intervals
  ci <- s * qnorm(alpha/2., lower.tail=FALSE)

  ## limits for plot
  yl <- c(min(fmin, 1.-ci, 1.-2*mean(ci)),
          max(fmax, 1.+ci, 1.+2*mean(ci)))
  xl <- c(0,1)
  
  ## create plotting aera with labels
  plot(xl,yl,xlim=xl,ylim=yl,type="n",
       xlab="F(x)", ylab="normalized frequencies", ...)

  ## draw histogram
  polygon( rep(ubreaks,each=2), c(yl[1],rep(freq,each=2),yl[1]), col="light blue", lwd=0.1 )

  ## add expected value
  abline(1,0,col="dark green",lwd=2)
  
  ## add confidence intervals
  ## we treat equidistributed in a special manner
  p0 <- ubreaks[-1] - ubreaks[-length(ubreaks)]
  if (isTRUE(all.equal(p0, rep(1/length(p0),length(p0))))) {
    ## equidistributed
    abline(1-ci,0,col="red",lwd=2,lty=2)
    abline(1+ci,0,col="red",lwd=2,lty=2)
  }
  else {
    ## not equidistributed
    ub <- rep(ubreaks,each=2)
    ub <- ub[-1]; ub <- ub[-length(ub)]
    lines(ub, rep(1-ci,each=2), col="red",lwd=2,lty=1)
    lines(ub, rep(1+ci,each=2), col="red",lwd=2,lty=1)
  }

  ## draw histogram lines
  lines(rep(ubreaks,each=2), c(yl[1],rep(freq,each=2),yl[1]), col="dark blue", lwd=2 )
}

## --------------------------------------------------------------------------
