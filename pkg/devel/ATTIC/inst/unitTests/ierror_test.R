#############################################################################
##                                                                         ## 
## Test functions for estimating error in numerical inversion methods      ##
##                                                                         ## 
#############################################################################

## Auxiliary routines -------------------------------------------------------

check.ierror <- function (ierror, kind, limit=1e-10) {
  if(VERBOSE) { print(ierror); print.default(ierror) }
  emax <- max(ierror$max)
  msg <- paste("\n   ",kind,"(): detected error too large\n", sep="")
  checkTrue(emax < limit, msg)
}

check.uerror <- function (uerror, limit=1e-10) { check.ierror(uerror,"uerror",limit) }

check.xerror <- function (xerror, limit=1e-10) { check.ierror(xerror,"xerror",limit) }


## --------------------------------------------------------------------------
##
## Test functions
##
## --------------------------------------------------------------------------

## uerror -------------------------------------------------------------------

test.001.uerror........... <- function () {
  ue <- uerror(n=1e5, aqdist=qnorm, pdist=pnorm)
  check.uerror(ue)
}

test.002.uerror.res....... <- function () {
  ue <- uerror(n=1e4, res=100, aqdist=qnorm, pdist=pnorm)
  check.uerror(ue)
}

test.003.uerror.tails..... <- function () {
  ue <- uerror(n=1e3, res=100, aqdist=qnorm, pdist=pnorm, tails=TRUE)
  check.uerror(ue)
}

test.004.uerror.udomain... <- function () {
  ue <- uerror(n=1e3, res=100, aqdist=qnorm, pdist=pnorm, udomain=c(0,0.1))
  check.uerror(ue)
}

test.005.uerror.funct..... <- function () {
  ue <- uerror(n=1e3, res=100, aqdist=function(u){qgamma(u,shape=2)},
               pdist=pgamma, shape=2, udomain=c(0,0.1))
  check.uerror(ue)
}

test.006.uerror.plot...... <- function () {
  ue <- uerror(n=1e3, res=100, aqdist=function(u){qgamma(u,shape=2)},
               pdist=pgamma, shape=2, udomain=c(0,0.1), plot=TRUE)
  check.uerror(ue)
}


## xerror -------------------------------------------------------------------

test.011.xerror........... <- function () {
  aq <- function(u) { qnorm(u) + u*runif(length(u),max=9.e-11) }
  xe <- xerror(n=1e5, aqdist=aq, qdist=qnorm)
  check.xerror(xe)
}

test.012.xerror.res....... <- function () {
  aq <- function(u) { qnorm(u) + u*runif(length(u),max=9.e-11) }
  xe <- xerror(n=1e4, res=100, aqdist=aq, qdist=qnorm)
  check.xerror(xe)
}

test.013.xerror.tails..... <- function () {
  aq <- function(u) { qnorm(u) + u*runif(length(u),max=9.e-11) }
  xe <- xerror(n=1e3, res=100, aqdist=aq, qdist=qnorm, tails=TRUE)
  check.xerror(xe)
}

test.014.xerror.udomain... <- function () {
  aq <- function(u) { qnorm(u) + u*runif(length(u),max=9.e-11) }
  xe <- xerror(n=1e4, res=100, aqdist=aq, qdist=qnorm, udomain=c(0,0.01))
  check.xerror(xe)
}

test.015.xerror.plot...... <- function () {
  aq <- function(u) { qnorm(u) + u*runif(length(u),max=9.e-11) }
  xe <- xerror(n=1e4, res=100, aqdist=aq, qdist=qnorm, plot=TRUE)
  check.xerror(xe)
}

test.016.xerror.kind.abs.. <- function () {
  aq <- function(u) { qnorm(u) + u*runif(length(u),max=9.e-11) }
  xe <- xerror(n=1e4, res=100, kind="abs", aqdist=aq, qdist=qnorm, plot=TRUE)
  check.xerror(xe)
}

test.017.xerror.kind.rel.. <- function () {
  aq <- function(u) { qnorm(u) + u*runif(length(u),max=9.e-11) }
  xe <- xerror(n=1e4, res=100, kind="rel", aqdist=aq, qdist=qnorm, plot=TRUE)
  check.xerror(xe, limit=1e-5)
}


## plot.rvgt.ierror ---------------------------------------------------------

test.021.plot.uerror...... <- function () {
  ## we just run the code 
  ue1 <- uerror(n=1e3, res=100, aqdist=function(u){qgamma(u,shape=2)},
                pdist=pgamma, shape=2)
  plot(ue1)
  plot(ue1,maxonly=TRUE)
  
  ue2 <- uerror(n=1e3, res=100, aqdist=function(u){qgamma(u,shape=1)},
                pdist=pgamma, shape=1, udomain=c(0.1,0.8))
  plot(ue2)
  plot(ue2,tol=1e-15)
  plot(ue2,tol=1e-16)
  plot(ue2,tol=5e-18)
  
  plot.rvgt.ierror(list(ue1,ue2),tol=4.e-16)
}

test.022.plot.xerror.abs.. <- function () {
  ## we just run the code 
  aq1 <- function(u) { qnorm(u) + u*runif(length(u),max=1.e-10) }
  xe1 <- xerror(n=1e3, res=100, aqdist=aq1, qdist=qnorm)
  plot(xe1)
  plot(xe1,maxonly=TRUE)
  
  aq2 <- function(u) { qnorm(u) + (1-u*u)*runif(length(u),max=1.e-10) }
  xe2 <- xerror(n=1e3, res=100, aqdist=aq2, qdist=qnorm)
  plot(xe2)

  plot.rvgt.ierror(list(xe1,xe2),tol=5.e-11)
}

test.023.plot.xerror.rel.. <- function () {
  ## we just run the code 
  aq1 <- function(u) { qnorm(u) + u*runif(length(u),max=1.e-10) }
  xe1 <- xerror(n=1e3, res=100, aqdist=aq1, qdist=qnorm, kind="rel")
  plot(xe1)

  aq2 <- function(u) { qnorm(u) + (1-u*u)*runif(length(u),max=1.e-10) }
  xe2 <- xerror(n=1e3, res=100, aqdist=aq2, qdist=qnorm, kind="rel")
  plot(xe2)
  
  plot.rvgt.ierror(list(xe1,xe2),tol=5.e-11)
}


## truncated domain ---------------------------------------------------------

test.031.uerror.trunc..... <- function () {
  ## An inverse CDF for a truncated normal distribution
  aqtn <- function(x) { qnorm(x * (pnorm(2.5) - pnorm(1.5)) + pnorm(1.5)) }

  ue <- uerror(n=1e5, res=100, aqdist=aqtn, pdist=pnorm, trunc=c(1.5,2.5))
  check.xerror(ue)
}

test.032.xerror.abs.trunc. <- function () {
  aqtn <- function(x) { qnorm(x * (pnorm(2.5) - pnorm(1.5)) + pnorm(1.5)) }
  xe <- xerror(n=1e5, res=100, aqdist=aqtn, qdist=qnorm, trunc=c(1.5,2.5), kind="abs")
  check.xerror(xe)
}

test.033.xerror.rel.trunc. <- function () {
  aqtn <- function(x) { qnorm(x * (pnorm(2.5) - pnorm(1.5)) + pnorm(1.5)) }
  xe <- xerror(n=1e5, res=100, aqdist=aqtn, qdist=qnorm, trunc=c(1.5,2.5), kind="rel")
  check.xerror(xe)
}


## --------------------------------------------------------------------------
##
## Check invalid arguments
##
## --------------------------------------------------------------------------

## uerror -------------------------------------------------------------------

test.041.uerror.invalid.. <- function () {
  ## sample size 'n'
  msg <- "\n   uerror(): invalid argument 'n' not detected\n"
  checkException(uerror(       aqdist=qnorm, pdist=pnorm), msg)
  checkException(uerror(n="a", aqdist=qnorm, pdist=pnorm), msg)
  checkException(uerror(n=0  , aqdist=qnorm, pdist=pnorm), msg)
  checkException(uerror(n=1.2, aqdist=qnorm, pdist=pnorm), msg)

  ## resolution 'res'
  msg <- "\n   uerror(): invalid argument 'res' not detected\n"
  checkException(uerror(n=1e4, res="a", aqdist=qnorm, pdist=pnorm), msg)
  checkException(uerror(n=1e4, res=0,   aqdist=qnorm, pdist=pnorm), msg)
  checkException(uerror(n=1e4, res=1.2, aqdist=qnorm, pdist=pnorm), msg)
  checkException(uerror(n=100, res=201, aqdist=qnorm, pdist=pnorm), msg)

  ## approximate inverse distribution function (quantile function)
  msg <- "\n   uerror(): invalid argument 'aqdist' not detected\n"
  checkException(uerror(n=1e3, res=100                 ), msg)
  checkException(uerror(n=1e3, res=100, aqdist="aqdist"), msg)

  ## distribution function
  msg <- "\n   uerror(): invalid argument 'pdist' not detected\n"
  checkException(uerror(n=1e3, res=100, aqdist=qnorm               ), msg)
  checkException(uerror(n=1e3, res=100, aqdist=qnorm, pdist="pnorm"), msg)

  ## domain
  msg <- "\n   uerror(): invalid argument 'udomain' not detected\n"
  checkException(uerror(n=1e3, res=100, aqdist=qnorm, pdist=pnorm, udomain=c(0.5,0.5)), msg)
  checkException(uerror(n=1e3, res=100, aqdist=qnorm, pdist=pnorm, udomain=c(-0.5,0.)), msg)
}


## xerror -------------------------------------------------------------------

test.051.xerror.invalid.. <- function () {
  ## sample size 'n'
  msg <- "\n   xerror(): invalid argument 'n' not detected\n"
  checkException(xerror(       aqdist=qnorm, qdist=qnorm), msg)
  checkException(xerror(n="a", aqdist=qnorm, qdist=qnorm), msg)
  checkException(xerror(n=0  , aqdist=qnorm, qdist=qnorm), msg)
  checkException(xerror(n=1.2, aqdist=qnorm, qdist=qnorm), msg)

  ## resolution 'res'
  msg <- "\n   xerror(): invalid argument 'res' not detected\n"
  checkException(xerror(n=1e4, res="a", aqdist=qnorm, qdist=qnorm), msg)
  checkException(xerror(n=1e4, res=0,   aqdist=qnorm, qdist=qnorm), msg)
  checkException(xerror(n=1e4, res=1.2, aqdist=qnorm, qdist=qnorm), msg)
  checkException(xerror(n=100, res=201, aqdist=qnorm, qdist=qnorm), msg)

  ## approximate inverse distribution function (quantile function)
  msg <- "\n   xerror(): invalid argument 'aqdist' not detected\n"
  checkException(xerror(n=1e3, res=100                 ), msg)
  checkException(xerror(n=1e3, res=100, aqdist="aqdist"), msg)

  ## (exact) quatile function of distribution
  msg <- "\n   xerror(): invalid argument 'qdist' not detected\n"
  checkException(xerror(n=1e3, res=100, aqdist=qnorm               ), msg)
  checkException(xerror(n=1e3, res=100, aqdist=qnorm, qdist="qnorm"), msg)

  ## domain
  msg <- "\n   xerror(): invalid argument 'udomain' not detected\n"
  checkException(xerror(n=1e3, res=100, aqdist=qnorm, qdist=qnorm, udomain=c(0.5,0.5)), msg)
  checkException(xerror(n=1e3, res=100, aqdist=qnorm, qdist=qnorm, udomain=c(-0.5,0.)), msg)

  ## kind
  msg <- "\n   xerror(): invalid argument 'kind' not detected\n"
  checkException(xerror(n=1e4, aqdist=qnorm, qdist=qnorm, kind="foo"), msg)
  checkException(xerror(n=1e4, aqdist=qnorm, qdist=qnorm, kind=10000), msg)
}


## plot.rvgt.ierror ---------------------------------------------------------

test.061.plot.invalid..... <- function () {
  msg <- "\n   plot.rvgt.ierror(): invalid argument 'x' not detected\n"
  checkException(plot.rvgt.ierror(              ), msg)
  checkException(plot.rvgt.ierror(x=1:10        ), msg)
  checkException(plot.rvgt.ierror(x=list(a=1:10)), msg)

  msg <- "\n   plot.rvgt.ierror(): invalid argument 'tol' not detected\n"
  ue <- uerror(n=1e4, res=10, aqdist=qnorm, pdist=pnorm)
  checkException(plot.rvgt.ierror(ue, tol=0   ), msg)
  checkException(plot.rvgt.ierror(ue, tol=-0.1), msg)
}

## -- End -------------------------------------------------------------------
