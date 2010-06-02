#############################################################################
##                                                                         ## 
## Test functions for estimating error in numerical inversion methods      ##
##                                                                         ## 
#############################################################################

## Load library -------------------------------------------------------------

library(rvgtest)

## Auxiliary routines -------------------------------------------------------

## Test whether there is an error
iserror <- function (expr) { is(try(expr), "try-error") }

## --------------------------------------------------------------------------
##
## Run functions
##
## --------------------------------------------------------------------------

## uerror -------------------------------------------------------------------

ue <- uerror(n=1e4, aqdist=qnorm, pdist=pnorm)
rm(ue)

## ..........................................................................
ue <- uerror(n=1e3, res=100, aqdist=qnorm, pdist=pnorm)
ue; rm(ue)

## ..........................................................................
ue <- uerror(n=1e3, res=100, aqdist=qnorm, pdist=pnorm, tails=TRUE)
ue; rm(ue)

## ..........................................................................
ue <- uerror(n=1e3, res=100, aqdist=qnorm, pdist=pnorm, udomain=c(0,0.1))
ue; rm(ue)

## ..........................................................................
ue <- uerror(n=1e3, res=100, aqdist=function(u){qgamma(u,shape=2)},
             pdist=pgamma, shape=2, udomain=c(0,0.1))
ue; rm(ue)

## ..........................................................................
ue <- uerror(n=1e3, res=100, aqdist=function(u){qgamma(u,shape=2)},
             pdist=pgamma, shape=2, udomain=c(0,0.1), plot=TRUE)
ue; rm(ue)


## xerror -------------------------------------------------------------------

aq <- function(u) { qnorm(u) + u*runif(length(u),max=1.e-10) }

xe <- xerror(n=1e3, res=100, aqdist=aq, qdist=qnorm)
rm(xe)

## ..........................................................................
xe <- xerror(n=1e3, res=100, aqdist=aq, qdist=qnorm)
xe; rm(xe)

## ..........................................................................
xe <- xerror(n=1e3, res=100, aqdist=aq, qdist=qnorm, tails=TRUE)
xe; rm(xe)

## ..........................................................................
xe <- xerror(n=1e3, res=100, aqdist=aq, qdist=qnorm, udomain=c(0,0.01))
xe; rm(xe)

## ..........................................................................
xe <- xerror(n=1e3, res=100, aqdist=aq, qdist=qnorm, plot=TRUE)
xe; rm(xe)

## ..........................................................................
xe <- xerror(n=1e3, res=100, kind="abs", aqdist=aq, qdist=qnorm, plot=TRUE)
xe; rm(xe)

## ..........................................................................
xe <- xerror(n=1e3, res=100, kind="rel", aqdist=aq, qdist=qnorm, plot=TRUE)
xe; rm(xe)

rm(aq)


## plot.rvgt.ierror ---------------------------------------------------------

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

rm(ue1,ue2)


aq1 <- function(u) { qnorm(u) + u*runif(length(u),max=1.e-10) }
xe1 <- xerror(n=1e3, res=100, aqdist=aq1, qdist=qnorm)
plot(xe1)
plot(xe1,maxonly=TRUE)

aq2 <- function(u) { qnorm(u) + (1-u*u)*runif(length(u),max=1.e-10) }
xe2 <- xerror(n=1e3, res=100, aqdist=aq2, qdist=qnorm)
plot(xe2)

plot.rvgt.ierror(list(xe1,xe2),tol=5.e-11)
rm(xe1,xe2)


xe1 <- xerror(n=1e3, res=100, aqdist=aq1, qdist=qnorm, kind="rel")
plot(xe1)

xe2 <- xerror(n=1e3, res=100, aqdist=aq2, qdist=qnorm, kind="rel")
plot(xe2)

plot.rvgt.ierror(list(xe1,xe2),tol=5.e-11)
rm(xe1,xe2)

rm(aq1,aq2)


## --------------------------------------------------------------------------
##
## Check invalid arguments
##
## --------------------------------------------------------------------------

## uerror -------------------------------------------------------------------

## sample size 'n'
if (! iserror(uerror(aqdist=qnorm, pdist=pnorm))        ||
    ! iserror(uerror(n="a", aqdist=qnorm, pdist=pnorm)) ||
    ! iserror(uerror(n=0,   aqdist=qnorm, pdist=pnorm)) ||
    ! iserror(uerror(n=1.2, aqdist=qnorm, pdist=pnorm))
    )
  stop ("Invalid argument 'n' not detected.")

## resolution 'res'
if (! iserror(uerror(n=1e4, res="a", aqdist=qnorm, pdist=pnorm)) ||
    ! iserror(uerror(n=1e4, res=0,   aqdist=qnorm, pdist=pnorm)) ||
    ! iserror(uerror(n=1e4, res=1.2, aqdist=qnorm, pdist=pnorm)) ||
    ! iserror(uerror(n=100, res=201, aqdist=qnorm, pdist=pnorm))
    )
  stop ("Invalid argument 'res' not detected.")

## approximate inverse distribution function (quantile function)
if (! iserror(uerror(n=1e3, res=100))                  ||
    ! iserror(uerror(n=1e3, res=100, aqdist="aqdist"))
    )
  stop ("Invalid argument 'aqdist' not detected.")

## distribution function
if (! iserror(uerror(n=1e3, res=100, aqdist=qnorm))                ||
    ! iserror(uerror(n=1e3, res=100, aqdist=qnorm, pdist="pnorm"))
    )
  stop ("Invalid argument 'pdist' not detected.")

## domain
if (! iserror(uerror(n=1e3, res=100, aqdist=qnorm, pdist=pnorm, udomain=c(0.5,0.5))) ||
    ! iserror(uerror(n=1e3, res=100, aqdist=qnorm, pdist=pnorm, udomain=c(-0.5,0)))
    )
  stop ("Invalid argument 'udomain' not detected.")


## xerror -------------------------------------------------------------------

## sample size 'n'
if (! iserror(xerror(aqdist=qnorm, qdist=qnorm))        ||
    ! iserror(xerror(n="a", aqdist=qnorm, qdist=qnorm)) ||
    ! iserror(xerror(n=0,   aqdist=qnorm, qdist=qnorm)) ||
    ! iserror(xerror(n=1.2, aqdist=qnorm, qdist=qnorm))
    )
  stop ("Invalid argument 'n' not detected.")

## resolution 'res'
if (! iserror(xerror(n=1e4, res="a", aqdist=qnorm, qdist=qnorm)) ||
    ! iserror(xerror(n=1e4, res=0,   aqdist=qnorm, qdist=qnorm)) ||
    ! iserror(xerror(n=1e4, res=1.2, aqdist=qnorm, qdist=qnorm)) ||
    ! iserror(xerror(n=100, res=201, aqdist=qnorm, qdist=qnorm))
    )
  stop ("Invalid argument 'res' not detected.")

## kind
if (! iserror(xerror(n=1e4, aqdist=qnorm, qdist=qnorm, kind="foo")) ||
    ! iserror(xerror(n=1e4, aqdist=qnorm, qdist=qnorm, kind=100))
    )
  stop ("Invalid argument 'kind' not detected.")

## approximate inverse distribution function (quantile function)
if (! iserror(xerror(n=1e3, res=100))                  ||
    ! iserror(xerror(n=1e3, res=100, aqdist="aqdist"))
    )
  stop ("Invalid argument 'aqdist' not detected.")

## (exact) quatile function of distribution
if (! iserror(xerror(n=1e3, res=100, aqdist=qnorm))                ||
    ! iserror(xerror(n=1e3, res=100, aqdist=qnorm, qdist="qnorm"))
    )
  stop ("Invalid argument 'qdist' not detected.")

## domain
if (! iserror(xerror(n=1e3, res=100, aqdist=qnorm, qdist=qnorm, udomain=c(0.5,0.5))) ||
    ! iserror(xerror(n=1e3, res=100, aqdist=qnorm, qdist=qnorm, udomain=c(-0.5,0)))
    )
  stop ("Invalid argument 'udomain' not detected.")


## plot.rvgt.ierror ---------------------------------------------------------

ue <- uerror(n=1e5, aqdist=qnorm, pdist=pnorm)

if (! iserror(plot.rvgt.ierror())               ||
    ! iserror(plot.rvgt.ierror(x=1:10))         ||
    ! iserror(plot.rvgt.ierror(x=list(a=1:10)))
    )
  stop ("Invalid argument 'x' not detected.")

if (! iserror(plot.rvgt.ierror(ue, tol=0))    ||
    ! iserror(plot.rvgt.ierror(ue, tol=-0.1))
    )
  stop ("Invalid argument 'tol' not detected.")

## clear
rm(ue)

## -- End -------------------------------------------------------------------
