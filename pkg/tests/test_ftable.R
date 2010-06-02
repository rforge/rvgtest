#############################################################################
##                                                                         ## 
## Test RVG frequency table                                                ## 
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

## rvgt.ftable --------------------------------------------------------------

ft <- rvgt.ftable(n=1e5,rep=5, rdist=rnorm,qdist=qnorm, breaks=51, mean=1,sd=2)
ft; rm(ft)

## ...........................................................................
ft <- rvgt.ftable(n=1e5,rep=1, rdist=rnorm,pdist=pnorm, exactu=TRUE)
ft; rm(ft)

## ...........................................................................
ft <- rvgt.ftable(n=1e5,rep=1, rdist=rnorm,pdist=pnorm, exactu=FALSE)
ft; rm(ft)

## ...........................................................................
ft <- rvgt.ftable(n=1e5,rep=5, rdist=rnorm,qdist=qnorm, breaks=1/(1:100))
ft; rm(ft)

## ...........................................................................
ft <- rvgt.ftable(n=1e5,rep=5, rdist=rnorm,qdist=qnorm, breaks=51, plot=TRUE)
ft; rm(ft)


## plot.rvgt.ftable ---------------------------------------------------------

ft <- rvgt.ftable(n=1e5,rep=5, rdist=rnorm,pdist=pnorm, exactu=TRUE)
plot(ft)
plot(ft,rows=c(2,3),alpha=0.005)
rm(ft)

ft <- rvgt.ftable(n=1e5,rep=5, rdist=rnorm,pdist=pnorm, exactu=FALSE)
plot(ft)
rm(ft)


## rvgt.chisq ---------------------------------------------------------------

ft <- rvgt.ftable(n=1e5,rep=5, rdist=rnorm,pdist=pnorm)
ht <- rvgt.chisq(ft)
ht
rm(ft,ht)

ft <- rvgt.ftable(n=1e5,rep=1, rdist=rnorm,pdist=pnorm)
ht <- rvgt.chisq(ft)
ht
rm(ft,ht)


## rvgt.Mtest ---------------------------------------------------------------

ft <- rvgt.ftable(n=1e5,rep=5, rdist=rnorm,pdist=pnorm)
ht <- rvgt.Mtest(ft)
ht
rm(ft,ht)


## plot.rvgt.htest ----------------------------------------------------------

ft <- rvgt.ftable(n=1e5,rep=5, rdist=rnorm,pdist=pnorm)
ht1 <- rvgt.chisq(ft)
plot(ht1)

ht2 <- rvgt.Mtest(ft)
plot(ht2)

plot.rvgt.htest(list(ht1,ht2))

rm(ft)
ft <- rvgt.ftable(n=1e5,rep=1, rdist=rnorm,pdist=pnorm)
ht3 <- rvgt.chisq(ft)

plot(ht3)

plot.rvgt.htest(list(ht1,ht2,ht3))

rm(ft,ht1,ht2,ht3)


## --------------------------------------------------------------------------
##
## Check invalid arguments
##
## --------------------------------------------------------------------------

## rvgt.ftable --------------------------------------------------------------

## sample size 'n'
if (! iserror(rvgt.ftable(rdist=rnorm, qdist=rnorm))        ||
    ! iserror(rvgt.ftable(n="a", rdist=rnorm, qdist=qnorm)) ||
    ! iserror(rvgt.ftable(n=0,   rdist=rnorm, qdist=qnorm)) ||
    ! iserror(rvgt.ftable(n=1.2, rdist=rnorm, qdist=qnorm))
    )
  stop ("Invalid argument 'n' not detected.")

## number of repetitions 'rep'
if (! iserror(rvgt.ftable(n=100, rep="a", rdist=rnorm, qdist=qnorm)) ||
    ! iserror(rvgt.ftable(n=100, rep=0,   rdist=rnorm, qdist=qnorm)) ||
    ! iserror(rvgt.ftable(n=100, rep=1.2, rdist=rnorm, qdist=qnorm))
    )
  stop ("Invalid argument 'rep' not detected.")

## random variate generator
if (! iserror(rvgt.ftable(n=100, qdist=rnorm)) ||
    ! iserror(rvgt.ftable(n=100, rdist="rnorm", qdist=qnorm))
    )
  stop ("Invalid argument 'rdist' not detected.")

## quantile and distribution function
if (! iserror(rvgt.ftable(n=100, rdist=rnorm))                ||
    ! iserror(rvgt.ftable(n=100, rdist=rnorm, pdist="pnorm")) ||
    ! iserror(rvgt.ftable(n=100, rdist=rnorm, qdist="qnorm"))
    )
  stop ("Invalid argument 'pdist' or 'qdist' not detected.")

## break points
if (! iserror(rvgt.ftable(n=100, rdist=rnorm, qdist=qnorm, breaks=c(0,0.1,0.2,"0.3"))) ||
    ! iserror(rvgt.ftable(n=100, rdist=rnorm, qdist=qnorm, breaks=numeric()))          ||
    ! iserror(rvgt.ftable(n=100, rdist=rnorm, qdist=qnorm, breaks=2))                  ||
    ! iserror(rvgt.ftable(n=100, rdist=rnorm, qdist=qnorm, breaks=c(0,1)))             ||
    ! iserror(rvgt.ftable(n=100, rdist=rnorm, qdist=qnorm, breaks=c(0,-2,1)))          ||
    ! iserror(rvgt.ftable(n=100, rdist=rnorm, qdist=qnorm, breaks=c(0,0.5,0.5,1)))
    )
  stop ("Invalid argument 'breaks' not detected.")

## use exact location of break points
if (! iserror(rvgt.ftable(n=100, rdist=rnorm, qdist=qnorm, exactu=0)))
  stop ("Invalid argument 'exactu' not detected.")


## rvgt.chisq ---------------------------------------------------------------

if (! iserror(rvgt.chisq())         ||
    ! iserror(rvgt.chisq("ftable"))
    )
  stop ("Invalid argument 'ftable' not detected.")


## rvgt.Mtest ---------------------------------------------------------------

if (! iserror(rvgt.Mtest())         ||
    ! iserror(rvgt.Mtest("ftable"))
    )
  stop ("Invalid argument 'ftable' not detected.")

## plot.rvgt.ftable ---------------------------------------------------------

ft <- rvgt.ftable(n=1e5, rdist=rnorm, qdist=qnorm)

if (! iserror(plot(ft,rows="a")) ||
    ! iserror(plot(ft,rows=0))   ||
    ! iserror(plot(ft,rows=2))
    )
  stop ("Invalid argument 'rows' not detected.")

if (! iserror(plot(ft,alpha=0)) ||
    ! iserror(plot(ft,alpha=1))
    )
  stop ("Invalid argument 'alpha' not detected.")

## clear
rm(ft)


## plot.rvgt.htest ----------------------------------------------------------

ft <- rvgt.ftable(n=1e5, rdist=rnorm, qdist=qnorm)
ht <- rvgt.chisq(ft)

if (! iserror(plot(ht,alpha=0)) ||
    ! iserror(plot(ht,alpha=1))
    )
  stop ("Invalid argument 'alpha' not detected.")

if (! iserror(plot.rvgt.htest(alpha=0.05))                ||
    ! iserror(plot.rvgt.htest(x=1:10,alpha=0.05))         ||
    ! iserror(plot.rvgt.htest(x=list(a=1:10),alpha=0.05)) 
    )
  stop ("Invalid argument 'x' not detected.")

## clear
rm(ft,ht)

## -- End -------------------------------------------------------------------
