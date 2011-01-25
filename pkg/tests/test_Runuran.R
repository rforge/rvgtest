#############################################################################
##                                                                         ## 
## Test RVG interface for UNU.RAN objects                                  ## 
##                                                                         ## 
#############################################################################

## Load libraries -----------------------------------------------------------

library(rvgtest)

if (! require(Runuran)) {
  warning("cannot run test file 'test_Runuran' -- package 'Runuran' is not available")
  quit("no")
}

## Auxiliary routines -------------------------------------------------------

## Test whether there is an error
iserror <- function (expr) { is(try(expr), "try-error") }

## --------------------------------------------------------------------------
##
## Run functions
##
## --------------------------------------------------------------------------

## rvgt.ftable --------------------------------------------------------------

unr <- new("unuran", "normal(1,2)")
ft <- rvgt.ftable(n=1e5,rep=5, rdist=unr,qdist=qnorm, breaks=51, mean=1,sd=2)
ft
print.default(ft)
if (rvgt.chisq(ft)$pval[5] < 1e-5)  stop ("error in rvgt.ftable()")
rm(unr,ft)

## rvgt.ftable with truncated domain -----------------------------------------

unr <- new("unuran", "normal();domain=(0,1)")
ft <- rvgt.ftable(n=1e5,rep=5, rdist=unr,qdist=qnorm, breaks=51, trunc=c(0,1))
ft
print.default(ft)
if (rvgt.chisq(ft)$pval[5] < 1e-5)  stop ("error in rvgt.ftable()")
rm(unr,ft)

## uerror -------------------------------------------------------------------

unr <- pinvd.new(udnorm(mean=1, sd=2), uresolution=1.e-12)
ue <- uerror(n=1e3, res=100, aqdist=unr, pdist=pnorm, mean=1,sd=2)
print.default(ue)
if (max(ue$max) > 1e-10)  stop("error in uerror()")
rm(unr,ue)

## xerror -------------------------------------------------------------------

unr <- pinvd.new(udnorm(mean=1,sd=2), uresolution=1.e-12)
ue <- xerror(n=1e3, res=100, aqdist=unr, qdist=qnorm, mean=1,sd=2)
print.default(ue)
if (max(ue$max) > 1e-8)  stop("error in xerror()")
rm(unr,ue)


## --------------------------------------------------------------------------
##
## Check invalid arguments
##
## --------------------------------------------------------------------------

## UNU.RAN object with non-inversion method ---------------------------------

unr <- tdrd.new(udnorm())
if (! iserror(uerror(n=1e3, res=100, aqdist=unr, pdist=pnorm)) )
  stop ("Invalid argument 'aqdist' not detected.")
rm(unr)

unr <- tdrd.new(udnorm())
if (! iserror(xerror(n=1e3, res=100, aqdist=unr, pdist=pnorm)) )
  stop ("Invalid argument 'aqdist' not detected.")
rm(unr)


## -- End -------------------------------------------------------------------
