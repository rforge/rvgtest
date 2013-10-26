## --------------------------------------------------------------------------
##'
##' Test rejection constants for random variate generator
## 
## --------------------------------------------------------------------------
##
##  @description
##'
##' Simple goodness-of-fit test for a generation algorithm that is based on
##' the acceptance-rejection method:
##' Test hypothesis that observed rejection constant coincides with the 
##' theoretical rejection constant.
##' 
## --------------------------------------------------------------------------
## 
##  @details
## 
##' This routine performs a test for the observed (empirical) rejection constant
##' for the given random variate generator for each combinations of parameter
##' values. It is a very simple but also fast test that compares the
##' (theoretical) rejection constant with the observed one.
##' It can be used as a first test as it allows to detect coding errors or
##' severe numerical problems (or a design error of the algorithm).
##'
##' Both the theoretical rejection constant and the observed rejection
##' constant must be provided by random variate generator \code{rdist}
##' as respective attributes \code{"trc"} and \code{"orc"}
##' when \code{rdist} is called with argument \code{show.properties=TRUE}.
##'
##' Of course this test is only sensible if \code{rdist} is based on the 
##' acceptance-rejection method.
##'
## --------------------------------------------------------------------------
##'
##' @seealso
##' \code{\link{rvgt.range.engine}} for a description of objects of class
##' \code{"rvgt.range"}.
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
##' ## a simple generator for the beta distribution 
##' myrbeta <- function(n, shape1, shape2, show.properties=FALSE) {
##'    if (shape1 <= 1 || shape2 <= 1 || n < 0) stop("arguments invalid")
##'    mode <- (shape1 - 1) / (shape1 + shape2 - 2)
##'    fmode <- dbeta(mode,shape1,shape2)
##'    trials <- 0
##'    res <- numeric(n) 
##'    for (i in 1:n) {
##'       while(n>0) {
##'          trials <- trials + 1
##'          X <- runif(1)
##'          Y <- fmode * runif(1)
##'          if (Y <= dbeta(X,shape1,shape2)) {
##'             res[n] <- X
##'             break
##'          }
##'       }
##'    }
##'    if (isTRUE(show.properties)) {
##'       trc <- fmode
##'       attr(res,"trc") <- trc
##'       attr(res,"orc") <- trials / n
##'    }
##'    res
##' }
##'
##' ## first we need the marginal generation times
##' mgt <- rvgt.range.marginal(rdist = myrbeta,
##'                            dist.params = list(shape1=c(2,3), shape2=2),
##'                            duration = 0.01)
##'
##' ## test rejection constants
##' orc <- rvgt.range.orc(rdist = myrbeta,
##'                       dist.params = list(shape1=c(2,3), shape2=2),
##'                       duration = 0.01, gen.time = mgt)
##'
##' print(orc)
##' 
## --------------------------------------------------------------------------
##'
##  Arguments:
##' @inheritParams rvgt.range.engine
##' 
## --------------------------------------------------------------------------
##'
##' @return
##' The function returns an object of class \code{"rvgt.range.gof.orc"}
##' where the p-values are stored in field \code{$data},
##' see \code{\link{rvgt.range.engine}} for a description of such objects.
##' The routine returns \code{NA} in all cases where the setup fails,
##' \code{NaN} when theoretical rejection constant is less than 1
##' (i.e., obviously invalid), and
##' \code{Inf} when the marginal generation time is too slow or when
##' a timeout has been reached.
##' 
## --------------------------------------------------------------------------

rvgt.range.orc <- function (rdist, dist.params, r.params=list(), 
                            duration=0.1, gen.time,
                            ncores=NULL, timeout=Inf, verbose=FALSE) {
        ## ..................................................................

        ## --- function 'rdist' must have argument 'show.properties'
        if (is.null(formals(rdist)$show.properties)) {
                stop("'rdist' must have argument 'show.properties').")
        }

        ## --- argument 'gen.time' must be of class "rvgt.range.time"
        if (! is(gen.time, "rvgt.range.time")) {
                stop("Argument 'gen.time' must be of class \"rvgt.range.time\".")
        }

        ## --- run test engine
        rvgt.range.engine(rdist = rdist,
                          dist.params = dist.params,
                          r.params = r.params,
                          test.routine = .run.orc,
                          test.class = "gof.orc",
                          duration = duration,
                          gen.time = gen.time,
                          ncores = ncores,
                          timeout = timeout,
                          timeout.val = NA_real_,
                          verbose = verbose)
}

                            
## --- Perform test on enpirical rejection constant -------------------------

.run.orc <- function (rdist, dist.params, r.params, emgt,
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
                if (verbose) cat("\t---> too slow!")
                return (Inf)
        }

        ## --- check whether the setup of rdist() works

        cl <- as.call(c(list(name=rdist,n=0), dist.params, r.params, list(show.properties=TRUE)))
        X <- try(eval(cl), silent=TRUE)
        if (is(X, "try-error")) {
                ## running rdist() failed
                if (verbose) cat("\t---> setup failed!")
                return (NA)
        }

        ## --- get theoretical rejection constant.

        ## it must be stored as attribute 'trc' in sample 'X'.
        trc <- attr(X,"trc")
        if (is.null(trc)) {
                ## attribute 'trc' not available
                stop("returned value of 'rdist' must have attribute 'trc'.")
        }        

        ## check for serious errors
        if (isTRUE(trc < 0.9999)) {
                warning("ERROR: rejection constant too small!!!!\n")
                if (verbose) cat("\tERROR: rejection constant too small!!!!\n")
                return (NaN)
        }
        if (isTRUE(trc < 1)) { trc <- 1 }  ## catch round-off error

        ## --- now estimate required sample size

        n <- round(duration/emgt)

        ## check sample size 'n'
        ## we only run the test the expected runtime is not too large
        ## i.e., when the sample size is > 0
        if (verbose) cat("n =", n)
        if (!isTRUE(n > 0)) {
                if (verbose) cat("\t---> too slow!")
                return (Inf)
        }

        ## --- run test

        ## draw random sample
        cl <- as.call(c(list(name=rdist,n=n), dist.params, r.params, list(show.properties=TRUE)))
        X <- eval(cl)

        ## read empirical rejection constant
        orc <- attr(X,"orc")
        if (is.null(orc)) {
                ## attribute 'orc' not available
                stop("returned value of 'rdist' must have attribute 'orc'.")
        }        
        
        ## compute p-value
        pval <- orc.pvalue(orc,trc,n)

        if (verbose) cat("\t... pval =",pval,"\n")

        ## return p-value
        pval
}

## --- compute p-value for orc test

orc.pvalue <- function (orc,trc,success) {

        ## H0
        p0 <- 1/trc

        ## number of trials = number of rejection loops
        trials <- round(orc*success)

        ## run test
        ## approximate test
        ## pval <- prop.test(x=success, n=trials, p=p0, correct = TRUE)$p.value
        ## exact (slower) test
        pval <- binom.test(x=success, n=trials, p=p0)$p.value

        ## check p-value
        if (!isTRUE(is.finite(pval))) { pval <- NA }

        ## return p-value
        pval
}

## --- End ------------------------------------------------------------------
