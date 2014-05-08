## --------------------------------------------------------------------------
##'
##' Collect theoretical rejection constants for random variate generator
## 
## --------------------------------------------------------------------------
##
##  @description
##' 
##' Collect (theoretical) rejection constants from the given random variate
##' generator for a range of parameters.
##' 
## --------------------------------------------------------------------------
## 
##  @details
## 
##' Routine \code{rvgt.range.trc} collects the (theoretical) rejection
##' constant from generator \code{rdist} for a range of parameters.
##' The rejection constant must be provide as attribute \code{"trc"}
##' (theoretical rejection constant) in the returned sample
##' when \code{rdist} is called with argument \code{show.properties=TRUE}.
##' Of course this property is only sensible if \code{rdist} is based on the 
##' acceptance-rejection method.
##'
## --------------------------------------------------------------------------
##'
##' @seealso
##' \code{\link{rvgt.range.engine}} for a description of objects of class
##' \code{"rvgt.range"}.
##' \code{\link{rvgt.range.rdist.example}} for an example of a random
##' variate generator that sets attribute \code{"trc"}.
##' \code{\link{summary.rvgt.range}} for a summary of test results.
##' \code{\link{plot.rvgt.range}} for plotting the test results.
##' 
## --------------------------------------------------------------------------
##'
##' @author Josef Leydold \email{josef.leydold@@wu.ac.at}
##'
## --------------------------------------------------------------------------
##
##' @examples
##' ## Generator 'rvgt.range.rdist.example' is a simple generator for
##' ## the beta distribution. It merely demonstrates how the "trc"
##' ## attribute has to be set.
##'
##' ## Collect rejection constants for a range of parameter values.
##' trc <- rvgt.range.trc(rdist = rvgt.range.rdist.example,
##'                       dist.params = list(shape1=2:10,shape2=5))
##'
##' ## print summary
##' summary(trc)
##' 
## --------------------------------------------------------------------------
##'
##  Arguments:
##' @inheritParams rvgt.range.engine
##' 
## --------------------------------------------------------------------------
##'
##' @return
##' The function returns an object of class \code{"rvgt.range.prop.trc"}
##' where the rejection constants are stored in field \code{$data},
##' see \code{\link{rvgt.range.engine}} for a description of such objects.
##' The routine returns \code{NA} in all cases where the setup failed or
##' when a timeout has been reached.
##' 
## --------------------------------------------------------------------------

rvgt.range.trc <- function (gen.data=NULL, rdist, dist.params, r.params=list(), 
                            ncores=NULL, timeout=Inf, verbose=FALSE) {
        ## ..................................................................

        rvgt.range.engine(gen.data = gen.data,
                          rdist = rdist,
                          dist.params = dist.params,
                          r.params = r.params,
                          test.routine = .run.trc,
                          test.class = "prop.trc",
                          needs.properties = TRUE,
                          ncores = ncores,
                          timeout = timeout,
                          timeout.val = NA_real_,
                          verbose = verbose)
}

                            
## --- Compute theoretical rejection constant -------------------------------

.run.trc <- function (rdist, dist.params, r.params, emgt,
                      test.params, duration, verbose) {

        ## check whether the setup of rdist() works
        cl <- as.call(c(list(name=rdist,n=0), dist.params, r.params, list(show.properties=TRUE)))
        X <- try(eval(cl), silent=TRUE)
        if (is(X, "try-error")) {
                ## running rdist() failed
                if (verbose) cat("\t---> setup failed!")
                return (NA)
        }

        ## get theoretical rejection constant.
        ## it must be stored as attribute 'trc' in sample 'X'.
        trc <- attr(X,"trc")
        if (is.null(trc)) {
                ## attribute 'trc' not available
                stop("returned value of 'rdist' must have attribute 'trc'.")
        }        

        if (verbose) cat("\t... rejectionconstant =", trc)
       
        ## return marginal generation time
        return (trc);
}

## --- End ------------------------------------------------------------------
