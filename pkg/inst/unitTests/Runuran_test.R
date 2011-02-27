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

test.002.ftable........... <- function () {
  unr <- dgtd.new(udbinom(size=20,prob=0.3))
  ft <- rvgt.ftable(n=1e5,rep=5, rdist=unr,pdist=pbinom, breaks=51, size=20, prob=0.3)
  if(VERBOSE) { print(unr); print(ft); print.default(ft) }
  pval <- rvgt.chisq(ft)$pval[5]
  msg <- "\n   rvgt.ftable(): Runuran API does not work\n"
  checkTrue(pval > 1e-5, msg)
}

## rvgt.ftable with truncated domain -----------------------------------------

test.003.ftable.trunc..... <- function () {
  unr <- new("unuran", "normal();domain=(0,1)")
  ft <- rvgt.ftable(n=1e5,rep=5, rdist=unr,qdist=qnorm, breaks=51, trunc=c(0,1))
  if(VERBOSE) { print(unr); print(ft); print.default(ft) }
  pval <- rvgt.chisq(ft)$pval[5]
  msg <- "\n   rvgt.ftable(): Runuran API does not work\n"
  checkTrue(pval > 1e-5, msg)
}

## uerror -------------------------------------------------------------------

test.004.uerror........... <- function () {
  unr <- pinvd.new(udnorm(mean=1, sd=2), uresolution=1.e-12)
  ue <- uerror(n=1e3, res=100, aqdist=unr, pdist=pnorm, mean=1,sd=2)
  if(VERBOSE) { print(unr); print(ue) }
  emax <- max(ue$max)
  msg <- "\n   uerror(): Runuran API does not work\n"
  checkTrue(emax < 1e-10, msg)
}

## xerror -------------------------------------------------------------------

test.005.xerror........... <- function () {
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

## UNU.RAN object must be of class "unuran.cont" ----------------------------

test.011.ftable.invalid... <- function () {
  ## rvgt.ftable() requires univariate distribution
  unr <- hitro.new(dim=2, pdf=function(x){exp(-sum(x^2))})
  if(VERBOSE) { print(unr) }

  msg <- "\n   rvgt.ftable(): Invalid Runuran object for argument 'rdist' not detected\n"
  checkException(rvgt.ftable(n=1e4,rep=1, rdist=unr,qdist=qbinom, size=20,prob=0.3), msg)
}

test.012.uerror.invalid... <- function () {
  ## uerror() requires univariate continuous distribution
  unr <- dgtd.new(udbinom(size=20,prob=0.3))
  if(VERBOSE) { print(unr) }
  msg <- "\n   uerror(): Invalid Runuran object for argument 'aqdist' not detected\n"
  checkException(uerror(n=1e3,res=100, aqdist=unr,pdist=pbinom, size=20,prob=0.3), msg)
}

test.013.xerror.invalid... <- function () {
  ## xerror() requires univariate continuous distribution
  unr <- dgtd.new(udbinom(size=20,prob=0.3))
  if(VERBOSE) { print(unr) }
  msg <- "\n   xerror(): Invalid Runuran object for argument 'aqdist' not detected\n"
  checkException(xerror(n=1e3,res=100, aqdist=unr,qdist=qbinom, size=20,prob=0.3), msg)
}

## UNU.RAN object with non-inversion method ---------------------------------

test.021.uerror.invalid... <- function () {
  ## uerror() requires inversion method but TDR is rejection method
  unr <- tdrd.new(udnorm())
  if(VERBOSE) { print(unr) }
  msg <- "\n   uerror(): Invalid Runuran object for argument 'aqdist' not detected\n"
  checkException(uerror(n=1e3, res=100, aqdist=unr, pdist=pnorm), msg)
}

test.022.xerror.invalid... <- function () {
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
