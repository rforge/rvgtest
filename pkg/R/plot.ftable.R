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

  ## get requested frequencies
  if (length(rows)==1) {
    freq <- table[rows,]
  }
  else {
    freq <- colSums(table[rows,])
  }

  ## normalize frequencies
  freq <- (m/n) * freq

  ## maximum and minimum frequencies
  fmax <- max(freq)
  fmin <- min(freq)

  ## standard deviation of bin densities
  s <- m * sqrt((1/m)*(1-1/m)/n)

  ## half length of confidence intervals
  ci <- s * qnorm(alpha/2., lower.tail=FALSE)

  ## limits for plot
  yl <- c( min(fmin, 1.-2*ci), max(fmax, 1.+2.*ci) )
  xl <- c(0,1)
  
  ## u break points
  ubreaks <- (0:m)/m

  ## create plotting aera with labels
  plot(xl,yl,xlim=xl,ylim=yl,type="n",
       xlab="F(x)", ylab="normalized frequencies", ...)

  ## draw histogram
  polygon( rep(ubreaks,each=2), c(yl[1],rep(freq,each=2),yl[1]), col="light blue", lwd=0.1 )
  lines( rep(ubreaks,each=2), c(yl[1],rep(freq,each=2),yl[1]), col="dark blue", lwd=2 )

  ## add confidence intervals
  abline(1,0,col="dark green",lwd=2)
  abline(1-ci,0,col="red",lwd=2,lty=2)
  abline(1+ci,0,col="red",lwd=2,lty=2)
}

## --------------------------------------------------------------------------
