## --------------------------------------------------------------------------
##'
##' Chisquare goodness-of-fit test for random variate generator
##
## --------------------------------------------------------------------------
##
##  @description
##'
##' Perform a chisquare goodness-of-fit test on the given random variate
##' generator for a range of parameter values.
##' 
## --------------------------------------------------------------------------
## 
##  @details
## 
##' Routine \code{rvgt.range.chisq} performs a chisquare goodness-of-fit test on
##' the given random variate \code{rdist} for a range of parameters,
##' see \code{\link{rvgt.chisq}} for details. 
##'
## --------------------------------------------------------------------------
##'
##' @seealso
##' \code{\link{rvgt.range.engine}} for a description of objects of class
##' \code{"rvgt.range"}.
##' \code{\link{rvgt.ftable}} and \code{\link{rvgt.chisq}} for the routines
##' that performs the test for a particular combinations of parameters.
##' \code{\link{print.rvgt.range}} for printing a summary of test results,
##' \code{\link{plot.rvgt.range}} for plotting the test results.
##' 
## --------------------------------------------------------------------------
##'
##' @author Josef Leydold \email{josef.leydold@@wu.ac.at}
##'
## --------------------------------------------------------------------------
##
##' @examples
##' ## Run chisquare gof test on the output of rbeta()
##' ## on the following set of parameters:
##' dp <- list(shape1=1:3, shape2=2)
##'
##' ## first we need the running times for the generator
##' mgt <- rvgt.range.marginal(rdist = rbeta, dist.params = dp,
##'                            duration = 0.01, gen.time = 1e-5)
##'
##' ## now run the test
##' chisq <- rvgt.range.chisq(rdist = rbeta, dist.params = dp,
##'                           n = 1e4, breaks = 100, qdist=qbeta,
##'                           duration = 1, gen.time = mgt)
##'
##' ## print summary
##' chisq
##' 
## --------------------------------------------------------------------------
##'
##  Arguments:
##' @inheritParams rvgt.range.engine
##'
##' @param duration
##'        maximal running time for drawing random sample.
##'        A test is not performed if sample size \code{n} times
##'        marginal generation time (given by argument \code{gen.time})
##'        exceeds this value (numeric).
##' @param n
##'        sample size for test (integer).
##' @param breaks
##'        number of bins in histogram, i.e,
##'        equidistributed break points for uniform scale
##'        (integer).
##' @param qdist
##'        quantile function for distribution (function).
##' @param pdist
##'        cumulative distribution function for distribution (function).
##' 
## --------------------------------------------------------------------------
##'
##' @return
##' The function returns an object of class \code{"rvgt.range.gof.chisq"}
##' where the p-values are stored in field \code{$data},
##' see \code{\link{rvgt.range.engine}} for a description of such objects.
##' The routine returns \code{NA} in all cases where the setup fails, and
##' \code{Inf} when the marginal generation time is too slow or when
##' a timeout has been reached.
##' When the chisquare goodness-of-fit tests aborts for other reasons then
##' \code{NaN} is returned.
##' 
## --------------------------------------------------------------------------

rvgt.range.chisq <- function (rdist, dist.params, r.params=list(),
                              n, breaks, qdist, pdist,
                              duration=0.1, gen.time,
                              ncores=NULL, timeout=Inf, verbose=FALSE) {
        ## ..................................................................

        ## --- sample size and number of break points
        if (missing(n) || !is.numeric(n) || n<100) {
                stop ("Argument 'n' is missing or invalid.")
        }
        n <- as.integer(n)

        if (missing(breaks) || !is.numeric(breaks) || breaks<2) {
                stop ("Argument 'breaks' is missing or invalid.")
        }
        breaks <- as.integer(breaks)

        ## --- argument 'gen.time' must be of class "rvgt.range.time"
        if (missing(gen.time)) {
                stop ("Argument 'gen.time' is missing")
        }
        if (! is(gen.time, "rvgt.range.time")) {
                stop("Argument 'gen.time' must be of class \"rvgt.range.time\".")
        }

        ## --- quantile and distribution function
        if (missing(qdist)) qdist <- NULL
        if (missing(pdist)) pdist <- NULL

        if (is.null(qdist) && is.null(pdist)) {
                stop ("Argument 'qdist' or 'pdist' required.")
        }
        if (!is.null(qdist) && !is.function(qdist)) {
                stop ("Argument 'qdist' invalid.")
        }
        if( !is.null(pdist) && !is.function(pdist)) {
                stop ("Argument 'pdist' invalid.")
        }

        ## --- run test engine
        rvgt.range.engine(rdist = rdist,
                          dist.params = dist.params,
                          r.params = r.params,
                          test.routine = .run.chisq,
                          test.class = "gof.chisq",
                          test.params = list(
                                  n=n,
                                  breaks=breaks,
                                  qdist=qdist,
                                  pdist=pdist),
                          duration = duration,
                          gen.time = gen.time,
                          ncores = ncores,
                          timeout = timeout,
                          timeout.val = NA_real_,
                          verbose = verbose)
}

                            
## --- Perform chissquare gog test ------------------------------------------

.run.chisq <- function (rdist, dist.params, r.params, emgt,
                        test.params, duration, verbose) {

        ## --- we use approximate marginal generation times

        ## check whether we can expect that the setup of rdist() works
        ## case 'NA':  setup failed in a previous run
        if (is.na(emgt)) {
                if (verbose) cat("\t---> setup failed!")
                return (NA)
        }
        ## case 'Inf': the generation time was too slow
        if (!is.finite(emgt)) {
                if (verbose) cat("\t---> too slow (gen.time=Inf)!")
                return (Inf)
        }

        ## --- further parameters for test

        ## sample size
        size <- test.params$n

        ## number of break points for histogram
        breaks <- test.params$breaks

        ## distribution and quantile functions
        qdist <- test.params$qdist
        pdist <- test.params$pdist
        
        ## --- check estimated runtime

        if (! isTRUE(size * emgt <= duration)) {
                if (verbose) cat("\t---> too slow for sample size!")
                return (Inf)
        }

        ## --- create calls
        
        fun.rdist <- function(n) { eval(as.call(c(list(name=rdist,n=n), dist.params, r.params))) }

        if (is.null(qdist)) {
                fun.qdist <- NULL
        } else {
                fun.qdist <- function(q) { eval(as.call(c(list(name=qdist,q ), dist.params))) }
                if (! is.finite(fun.qdist(0.5))) {
                        if (verbose) cat("\t---> qdist broken!")
                        return (NaN)
                }
        }

        if (is.null(pdist)) {
                fun.pdist <- NULL
        } else {
                fun.pdist <- function(p) { eval(as.call(c(list(name=pdist,p), dist.params))) }
                if (! is.finite(fun.pdist(0))) {
                        if (verbose) cat("\t---> pdist broken!")
                        return (NaN)
                }
        }

        ## --- run chisquare test

        ft <- rvgt.ftable(n=size, rep=1, rdist=fun.rdist, pdist=fun.pdist, qdist=fun.qdist)
        pvals <- rvgt.chisq(ft)$pval
        pval <- pvals[length(pvals)]

        ## --- check p-value
        if (!isTRUE(is.finite(pval))) { pval <- NaN }

        if (verbose) cat("\t... pval =",pval,"\n")

        ## return p-value
        pval
}

## --- End ------------------------------------------------------------------
