#############################################################################
##                                                                         ## 
## Test RVG interface for UNU.RAN objects                                  ## 
##                                                                         ## 
#############################################################################

## Load library -------------------------------------------------------------

if (require(Runuran)) {

## --------------------------------------------------------------------------
##
## Test functions
##
## --------------------------------------------------------------------------

## rvgt.ftable --------------------------------------------------------------

test.001.ftable........... <- function () {
  unr <- new("unuran", "normal(1,2)")
  ft <- rvgt.ftable(n=1e5,rep=5, rdist=unr,qdist=qnorm, breaks=51, mean=1,sd=2)
  if(VERBOSE) { print(unr); print(ft); print.default(ft) }
  pval <- rvgt.chisq(ft)$pval[5]
  msg <- "\n   rvgt.ftable(): Runuran API does not work\n"
  checkTrue(pval > 1e-5, msg)
}

## rvgt.ftable with truncated domain -----------------------------------------

test.002.ftable.trunc..... <- function () {
  unr <- new("unuran", "normal();domain=(0,1)")
  ft <- rvgt.ftable(n=1e5,rep=5, rdist=unr,qdist=qnorm, breaks=51, trunc=c(0,1))
  if(VERBOSE) { print(unr); print(ft); print.default(ft) }
  pval <- rvgt.chisq(ft)$pval[5]
  msg <- "\n   rvgt.ftable(): Runuran API does not work\n"
  checkTrue(pval > 1e-5, msg)
}

## uerror -------------------------------------------------------------------

test.003.uerror........... <- function () {
  unr <- pinvd.new(udnorm(mean=1, sd=2), uresolution=1.e-12)
  ue <- uerror(n=1e3, res=100, aqdist=unr, pdist=pnorm, mean=1,sd=2)
  if(VERBOSE) { print(unr); print(ue) }
  emax <- max(ue$max)
  msg <- "\n   uerror(): Runuran API does not work\n"
  checkTrue(emax < 1e-10, msg)
}

## xerror -------------------------------------------------------------------

test.004.xerror........... <- function () {
  unr <- pinvd.new(udnorm(mean=1,sd=2), uresolution=1.e-12)
  xe <- xerror(n=1e3, res=100, aqdist=unr, qdist=qnorm, mean=1,sd=2)
  if(VERBOSE) { print(unr); print(xe) }
  emax <- max(xe$max)
  msg <- "\n   xerror(): Runuran API does not work\n"
  checkTrue(emax < 1e-8, msg)
}

## --------------------------------------------------------------------------
##
## Check invalid arguments
##
## --------------------------------------------------------------------------

## UNU.RAN object with non-inversion method ---------------------------------

test.005.uerror.invalid... <- function () {
  ## uerror() requires inversion method but TDR is rejection method
  unr <- tdrd.new(udnorm())
  if(VERBOSE) { print(unr) }
  msg <- "\n   uerror(): Invalid Runuran object for argument 'aqdist' not detected\n"
  checkException(uerror(n=1e3, res=100, aqdist=unr, pdist=pnorm), msg)

}

test.006.xerror.invalid... <- function () {
  ## xerror() requires inversion method but TDR is rejection method
  unr <- tdrd.new(udnorm())
  if(VERBOSE) { print(unr) }
  msg <- "\n   xerror(): Invalid Runuran object for argument 'aqdist' not detected\n"
  checkException(xerror(n=1e3, res=100, aqdist=unr, qdist=qnorm), msg)
}


## --------------------------------------------------------------------------
##
## Package not available
##
## --------------------------------------------------------------------------

} else {
  message("\nCannot run test file 'test_Runuran' -- package 'Runuran' is not available\n")
} ## end of 'if (require(Runuran)) {'

## -- End -------------------------------------------------------------------
