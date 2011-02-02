#############################################################################
##                                                                         ## 
## Test RVG frequency table                                                ## 
##                                                                         ## 
#############################################################################

## Auxiliary routines -------------------------------------------------------

check.ftable <- function (ftable, rep=1) {
  if(VERBOSE) { print(ftable); print.default(ftable) }
  msg <- paste("\n   rvgt.ftable(): p-value too small\n", sep="")
  pval <- rvgt.chisq(ftable)$pval[rep]
  checkTrue(pval > 1e-5, msg)
}

## --------------------------------------------------------------------------
##
## Test functions
##
## --------------------------------------------------------------------------

## rvgt.ftable --------------------------------------------------------------

test.001.ftable........... <- function () {
  ft <- rvgt.ftable(n=1e5, rdist=rnorm,qdist=qnorm)
  check.ftable(ft)
}

test.002.ftable.distparams <- function () {
  ft <- rvgt.ftable(n=1e5, rdist=rnorm,qdist=qnorm, mean=1,sd=2)
  check.ftable(ft)
}

test.003.ftable.breaks.... <- function () {
  ft <- rvgt.ftable(n=1e5, rdist=rnorm,qdist=qnorm, breaks=51, mean=1,sd=2)
  check.ftable(ft)
  ft <- rvgt.ftable(n=1e5, rdist=rnorm,qdist=qnorm, breaks=1/(1:100))
  check.ftable(ft)
}

test.004.ftable.rep....... <- function () {
  ft <- rvgt.ftable(n=1e5, rep=5, rdist=rnorm,qdist=qnorm, breaks=51, mean=1,sd=2)
  check.ftable(ft, rep=5)
}

test.005.ftable.exact..... <- function () {
  ft <- rvgt.ftable(n=1e5,rep=1, rdist=rnorm,pdist=pnorm, exactu=TRUE)
  check.ftable(ft)
  ft <- rvgt.ftable(n=1e5,rep=1, rdist=rnorm,pdist=pnorm, exactu=FALSE)
  check.ftable(ft)
}

test.006.ftable.plot...... <- function () {
  ft <- rvgt.ftable(n=1e5,rep=5, rdist=rnorm,qdist=qnorm, breaks=51, plot=TRUE)
  check.ftable(ft, rep=5)
}


## truncated domain ---------------------------------------------------------

test.011.ftable.trunc..... <- function () {
  rdist <- function(n) {
    x <- numeric(n)
    for (i in 1:n) {
      while(TRUE) { x[i] <- rnorm(1); if (x[i]>0 && x[i]<1) break } }
    return(x)
  }

  ft <- rvgt.ftable(n=1e3,rep=5, rdist=rdist, pdist=pnorm, qdist=qnorm, plot=FALSE, trunc=c(0,1))
  check.ftable(ft, rep=5)
}

test.012.ftable.trunc..... <- function () {
  rdist <- function(n) {
    x <- numeric(n)
    for (i in 1:n) {
      while(TRUE) { x[i] <- rnorm(1); if (x[i]>0 && x[i]<1) break } }
    return(x)
  }

  ft <- rvgt.ftable(n=1e3,rep=5, rdist=rdist, qdist=qnorm, plot=FALSE, trunc=c(0,1))
  check.ftable(ft, rep=5)
}

test.013.ftable.trunc..... <- function () {
  rdist <- function(n) {
    x <- numeric(n)
    for (i in 1:n) {
      while(TRUE) { x[i] <- rnorm(1); if (x[i]>0 && x[i]<1) break } }
    return(x)
  }

  ft <- rvgt.ftable(n=1e4,rep=1, rdist=rdist, pdist=pnorm, plot=FALSE, trunc=c(0,1))
  check.ftable(ft, rep=1)
}


## plot.rvgt.ftable ---------------------------------------------------------

test.021.plot.ftable...... <- function () {
  ## we just run the code 
  ft <- rvgt.ftable(n=1e5,rep=5, rdist=rnorm,pdist=pnorm, exactu=TRUE)
  plot(ft)
  plot(ft,rows=c(2,3),alpha=0.005)

  ft <- rvgt.ftable(n=1e5,rep=5, rdist=rnorm,pdist=pnorm, exactu=FALSE)
  plot(ft)
}


## rvgt.chisq ---------------------------------------------------------------

##test.031.chisq............ <- function () {
##  ## we just run the code 
##  ft <- rvgt.ftable(n=1e5,rep=5, rdist=rnorm,pdist=pnorm)
##  ht <- rvgt.chisq(ft)
##  print(ht)
##  print.default(ht)
##
##  ft <- rvgt.ftable(n=1e5,rep=1, rdist=rnorm,pdist=pnorm)
##  ht <- rvgt.chisq(ft)
##  print(ht)
##  print.default(ht)
##}


## rvgt.Mtest ---------------------------------------------------------------

##test.032.Mtest............ <- function () {
##  ## we just run the code 
##  ft <- rvgt.ftable(n=1e5,rep=5, rdist=rnorm,pdist=pnorm)
##  ht <- rvgt.Mtest(ft)
##  print(ht)
##  print.default(ht)
##}


## plot.rvgt.htest ----------------------------------------------------------

test.041.plot.htest....... <- function () {
  ## we just run the code 
  ft <- rvgt.ftable(n=1e5,rep=5, rdist=rnorm,pdist=pnorm)
  ht1 <- rvgt.chisq(ft)
  plot(ht1)

  ht2 <- rvgt.Mtest(ft)
  plot(ht2)
  
  plot.rvgt.htest(list(ht1,ht2))
  
  ft <- rvgt.ftable(n=1e5,rep=1, rdist=rnorm,pdist=pnorm)
  ht3 <- rvgt.chisq(ft)
  plot(ht3)

  plot.rvgt.htest(list(ht1,ht2,ht3))
}


## --------------------------------------------------------------------------
##
## Check invalid arguments
##
## --------------------------------------------------------------------------

## rvgt.ftable --------------------------------------------------------------

test.051.ftable.invalid.. <- function () {
  ## sample size 'n'
  msg <- "\n   rvgt.ftable(): invalid argument 'n' not detected\n"
  checkException(rvgt.ftable(       rdist=rnorm, qdist=qnorm), msg)
  checkException(rvgt.ftable(n="a", rdist=rnorm, qdist=qnorm), msg)
  checkException(rvgt.ftable(n=0  , rdist=rnorm, qdist=qnorm), msg)
  checkException(rvgt.ftable(n=1.2, rdist=rnorm, qdist=qnorm), msg)

  ## resolution 'res'
  msg <- "\n   rvgt.ftable(): invalid argument 'rep' not detected\n"
  checkException(rvgt.ftable(n=100, rep="a", rdist=rnorm, qdist=qnorm), msg)
  checkException(rvgt.ftable(n=100, rep=0,   rdist=rnorm, qdist=qnorm), msg)
  checkException(rvgt.ftable(n=100, rep=1.2, rdist=rnorm, qdist=qnorm), msg)

  ## random variate generator
  msg <- "\n   rvgt.ftable(): invalid argument 'rdist' not detected\n"
  checkException(rvgt.ftable(n=100, qdist=rnorm               ), msg)
  checkException(rvgt.ftable(n=100, rdist="rnorm", qdist=qnorm), msg)

  ## quantile and distribution function
  msg <- "\n   rvgt.ftable(): invalid argument 'pdist' or 'qdist' not detected\n"
  checkException(rvgt.ftable(n=100, rdist=rnorm               ), msg)
  checkException(rvgt.ftable(n=100, rdist=rnorm, pdist="pnorm"), msg)
  checkException(rvgt.ftable(n=100, rdist=rnorm, qdist="qnorm"), msg)

  ## break points
  msg <- "\n   rvgt.ftable(): invalid argument 'breaks' not detected\n"
  checkException(rvgt.ftable(n=100, rdist=rnorm, qdist=qnorm, breaks=c(0,0.1,0.2,"0.3")), msg)
  checkException(rvgt.ftable(n=100, rdist=rnorm, qdist=qnorm, breaks=numeric()),          msg)
  checkException(rvgt.ftable(n=100, rdist=rnorm, qdist=qnorm, breaks=2),                  msg)
  checkException(rvgt.ftable(n=100, rdist=rnorm, qdist=qnorm, breaks=c(0,1)),             msg)
  checkException(rvgt.ftable(n=100, rdist=rnorm, qdist=qnorm, breaks=c(0,-2,1)),          msg)
  checkException(rvgt.ftable(n=100, rdist=rnorm, qdist=qnorm, breaks=c(0,0.5,0.5,1)),     msg)

  ## use exact location of break points
  msg <- "\n   rvgt.ftable(): invalid argument 'exactu' not detected\n"
  checkException(rvgt.ftable(n=100, rdist=rnorm, qdist=qnorm, exactu=0), msg)
}

## rvgt.chisq ---------------------------------------------------------------

test.061.chisq.invalid.... <- function () {
  msg <- "\n   rvgt.chisq(): invalid argument 'ftable' not detected\n"
  checkException(rvgt.chisq(),          msg)
  checkException(rvgt.chisq("ftable"),  msg)
  checkException(rvgt.chisq(list(a=1)), msg)
}

## rvgt.Mtest ---------------------------------------------------------------

test.062.Mtest.invalid.... <- function () {
  msg <- "\n   rvgt.Mtest(): invalid argument 'ftable' not detected\n"
  checkException(rvgt.Mtest(),          msg)
  checkException(rvgt.Mtest("ftable"),  msg)
  checkException(rvgt.Mtest(list(a=1)), msg)
}

## plot.rvgt.ftable ---------------------------------------------------------

test.071.plot.ftable.inval <- function () {
  ft <- rvgt.ftable(n=1e5, rdist=rnorm, qdist=qnorm)

  ## number of rows
  msg <- "\n   plot.rvgt.ftable(): invalid argument 'rows' not detected\n"
  checkException(plot(ft,rows="a"), msg)
  checkException(plot(ft,rows=0),   msg)
  checkException(plot(ft,rows=2),   msg)

  ## significance level 'alpha'
  msg <- "\n   plot.rvgt.ftable(): invalid argument 'alpha' not detected\n"
  checkException(plot(ft,alpha="a"), msg)
  checkException(plot(ft,alpha=0),   msg)
  checkException(plot(ft,alpha=1),   msg)
}

## plot.rvgt.htest ----------------------------------------------------------

test.072.plot.htest.invali <- function () {
  ft <- rvgt.ftable(n=1e5, rdist=rnorm, qdist=qnorm)
  ht <- rvgt.chisq(ft)

  ## significance level 'alpha'
  msg <- "\n   plot.rvgt.htest(): invalid argument 'alpha' not detected\n"
  checkException(plot(ht,alpha="a"), msg)
  checkException(plot(ht,alpha=0),   msg)
  checkException(plot(ht,alpha=1),   msg)

  ## htest object
  msg <- "\n   plot.rvgt.htest(): invalid argument 'x' not detected\n"
  checkException(plot.rvgt.htest(               alpha=0.05), msg)
  checkException(plot.rvgt.htest(x=1:10,        alpha=0.05), msg)
  checkException(plot.rvgt.htest(x=list(a=1:10),alpha=0.05), msg)
}

## -- End -------------------------------------------------------------------
