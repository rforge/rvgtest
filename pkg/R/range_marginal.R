## --------------------------------------------------------------------------
##'
##' Estimate marginal generation times of random variate generator
## 
## --------------------------------------------------------------------------
##
##  @description
##' 
##' Estimate marginal generation times for the given random variate generator
##' for a range of parameters.
##' 
## --------------------------------------------------------------------------
## 
##  @details
## 
##' Routine \code{rvgt.range.marginal} estimates marginal generation times of
##' random variate generator \code{rdist} for a range of parameters.
##' For this purpose the total generation time of a larger sample is estimated
##' using the running time that is returned by
##' \code{\link{system.time}}.
##'
##' The scheduled sampling time for a particular combination of parameters
##' is given by parameter \code{duration}. Notice that it must not be too
##' small compared to the resolution of the system clock (typically
##' 0.001 seconds).
##'
##' The required sample size is estimated using the given \code{duration}
##' and an (approximate) estimate of the marginal generation time.
##' As the latter is not known at the first call to this routine we have a
##' chicken-and-egg situation. Thus one has to provide an (at least rough)
##' estimate for the marginal generation time via argument
##' \code{gen.time}.
##' It has to be a single positive numeric value. However, if there is
##' already the result of a previous run of routine
##' \code{rvgt.range.marginal} (e.g., a pilot study) then it
##' is recommended to pass its result via argument \code{gen.data}
##' (instead of providing argument \code{gen.time}).
##' If neither \code{gen.time} nor \code{gen.data} is given, then 
##' the \code{duration} is used as a first guess.
##'
##' The timing information is then used to estimate the marginal
##' running time in a sequence of trials where the sample size is
##' \emph{increased} after each run when the total running time for
##' the sample is too small. 
##' (So the first guess of the marginal generation time should be
##' rather too large than too small.)
##' This iterative process is stopped if the total running time is
##' close to \code{duration}.
##'
##' \emph{Notice:}
##' if \code{gen.data} is provided instead of \code{gen.time}, then
##' tests for parameter values where the result was \code{NA} or
##' \code{NaN} (i.e., the setup failed) or \code{Inf} (timeout was
##' reached) are not performed any more and the old results are just
##' copied.
##' 
##' An alternative approach works for random number generators that are based
##' on the rejection method. Then it is assumed that the running time for a
##' single acceptance-rejection loop does not depend on the parameters of the
##' distribution. It can be provided by means of argument \code{el.time}.
##' Moreover, random variate generator \code{rdist} has to return a sample
##' with attribute \code{"trc"} (theoretical rejection constant) when
##' argument \code{show.properties=TRUE} is present. Then the expected
##' running time is approximated by the product of these two values.
##'
##' Notice that \code{el.time} is always used (instead of
##' \code{gen.time} or \code{gen.data}) if it given. 
##'
##' Timings can vary considerably. In order to get a more robust estimate
##' one can repeat this test several times and use the median of all runs.
##' This can be achieved by means of argument \code{repetitions}.
##'
##' A big issue of tests on a large range of parameter settings is that the
##' running times may not be known in advance. Thus the total running time of
##' the test suite may be extremely long due to some unexpected long runs
##' for some tests.
##' In order to avoid such problems the running times for each test can be
##' limited by means of argument \code{timeout} which sets an upper limit
##' for the running time of each test. Notice, however, that this
##' feature requires multicore support and is thus available only on
##' POSIX-compliant OSes (i.e., not on Windows OSes). It can be
##' enabled by means of argument \code{ncores}.
##'
## --------------------------------------------------------------------------
##'
##' @note
##' It is assumed that the setup time is negligible compared the total
##' sampling time.
##' 
## --------------------------------------------------------------------------
##'
##' @seealso
##' \code{\link{rvgt.range.engine}} for a description of objects of class
##' \code{"rvgt.range"}.
##' \code{\link{summary.rvgt.range}} for a summary of test results,
##' \code{\link{plot.rvgt.range}} for plotting the test results.
##' 
## --------------------------------------------------------------------------
##'
##' @author Josef Leydold \email{josef.leydold@@wu.ac.at}
##'
## --------------------------------------------------------------------------
##'
##' @examples
##' ## Estimate marginal generation time for beta distribution
##' mgt <- rvgt.range.marginal(rdist = rbeta,
##'                            dist.params = list(
##'                              shape1=c(0.1,1,10),
##'                              shape2=c(0.1,1,10)),
##'                            duration = 0.01, gen.time = 1e-5)
##'
##' ## print summary of generation times
##' summary(mgt)
##'
##' ## plot generation times (using logarithmic scales for parameters)
##' plot(mgt, xscale="log", yscale="log", main="rbeta")
##' 
## --------------------------------------------------------------------------
##'
##  Arguments:
##' @inheritParams rvgt.range.engine
##' 
##' @param el.time
##'        running time for an accept-reject loop of a generator based on
##'        the rejection method. (numeric)
##' @param repetitions
##'        number of repetitions of the test. (integer)
##' 
##'        The marginal generation time is estimated by the median of
##'        all test results.
##'
## --------------------------------------------------------------------------
##'
##' @return
##' The function returns an object of class \code{"rvgt.range.time.marginal"}
##' where the observed marginal generation times are stored in field \code{$data},
##' see \code{\link{rvgt.range.engine}} for a description of such objects.
##' The routine returns \code{NA} in all cases where the setup fails, and
##' \code{Inf} when a timeout has been reached.
##' 
## --------------------------------------------------------------------------

rvgt.range.marginal <- function (gen.data=NULL,
                                 rdist, dist.params, r.params=list(),
                                 el.time=NA, duration=0.1, gen.time, repetitions=1L,
                                 ncores=NULL, timeout=Inf, verbose=FALSE) {
        ## ..................................................................

        ## --- expected runtime for acceptance-rejection cycle
        
        if (! (is.na(el.time) ||
               (is.numeric(el.time) && isTRUE(length(el.time)==1L) &&
                isTRUE(el.time>0))) )
                stop("Argument 'el.time' invalid")

        if (! is.na(el.time)) {
                ## Function 'rdist' must have argument 'show.properties'
                if (is.null(formals(rdist)$show.properties)) {
                        warning(paste("Argument 'el.time' ignored",
                                      "('rdist' requires parameter 'show.properties')"))
                        el.time <- NA
                }
        }

        ## --- number of repetitions

        if (! (is.numeric(repetitions) && length(repetitions)==1L && isTRUE(repetitions>=1)))
                stop("Argument 'repetitions' invalid")
        repetitions <- as.integer(repetitions)

        ## --- run test engine

        test.routine <- if (isTRUE(repetitions > 1L)) .run.mgt.reps else .run.mgt.single
        
        rvgt.range.engine(gen.data = gen.data,
                          rdist = rdist,
                          dist.params = dist.params,
                          r.params = r.params,
                          test.routine = test.routine,
                          test.class = "time.marginal",
                          test.params = list(el.time=el.time, repetitions=repetitions),
                          duration = duration,
                          gen.time = gen.time,
                          ncores = ncores,
                          timeout = timeout,
                          timeout.val = Inf,
                          verbose = verbose)
}

## --- estimate marginal generation time ------------------------------------

.run.mgt.reps <- function (rdist, dist.params, r.params, emgt,
                           test.params, duration, verbose) {

        ## prepare vector for particular results
        repetitions <- test.params$repetitions
        results <- rep(NA_real_, repetitions)

        ## run test 'repetitions' times
        for (k in 1:repetitions) {
                if (verbose) cat("\n\trun ",k,": ", sep="")
                results[k] <- .run.mgt.single(rdist, dist.params, r.params, emgt,
                                              test.params, duration, verbose)
                ## use marginal generation time for next run
                emgt <- results[k]
                test.params = list(el.time=NA)
        }
        
        ## return median of all runs
        median(results)
}

## ..........................................................................

.run.mgt.single <- function (rdist, dist.params, r.params, emgt,
                             test.params, duration, verbose) {

        ## --- estimate required sample size

        n <- NA

        ## first try:
        ## use approximate runtime for an acceptance-rejection loop
        ## (if given)
        if (! is.na(test.params$el.time)) {

                ## get theoretical rejection constant.
                ## it should be stored as attribute 'trc' in the
                ## returned samole when 'show.properties' is set to TRUE
                cl <- as.call(c(list(name=rdist,n=0), dist.params, r.params, list(show.properties=TRUE)))
                X <- try(eval(cl), silent=TRUE)
                if (is(X, "try-error")) {
                        ## running rdist() failed
                        if (verbose) cat("\t---> setup failed!")
                        return (NA)
                }
                trc <- attr(X,"trc")

                ## check whether we know the (expected) rejection constant
                if (isTRUE(is.finite(trc))) {
                        ## required sample size
                        n <- round(1. / (test.params$el.time * trc))
                } else if (is.null(trc)) {
                        ## property 'trc' not available
                        stop(paste("Argument 'el.time' cannot be used",
                                   "(returned value of 'rdist' must have attribute 'trc')"))
                } else {
                        ## 'trc' is either Inf or NaN
                        if (verbose) cat("\t---> invalid value!")
                        return (NA)
                }
        }        

        ## second try:
        ## use expected approximate marginal generation times

        if (is.na(n)) {
                ## 'n' not known yet

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

                ## check whether the setup of rdist() works
                cl <- as.call(c(list(name=rdist,n=0), dist.params, r.params))
                X <- try( eval(cl), silent=TRUE)
                if (is(X, "try-error")) {
                        ## running rdist() failed
                        if (verbose) cat("\t---> setup failed!")
                        return (NA)
                }

                ## required sample size
                n <- round(duration/emgt)
        }

        ## check sample size 'n'
        ## we only run the test the expected runtime is not too large
        ## i.e., when the sample size is > 0
        if (verbose) cat("n =", n)
        n <- as.integer(n)
        if (! is.finite(n)) {
                if (verbose) cat("\t---> invalid value (maybe too large)!")
                return (NA)
        }
        if (!isTRUE(n > 0)) {
                if (verbose) cat("\t---> too slow!")
                return (Inf)
        }

        ## --- run simulation

        ## draw sample
        while (TRUE) {
                cl <- as.call(c(list(name=rdist, n=n), dist.params, r.params))
                run.time <- get.runtime(system.time(eval(cl)))
                marginal.time <- run.time / n
                if (run.time > 0.8*duration) {
                        ## runtime approx scheduled duration --> stop
                        break
                } else if (run.time > duration/10) {
                        ## runtime close to scheduled duration --> use as emgt
                        n <- round(duration/marginal.time)
                } else {
                        ## measured runtime too small --> increase sample size
                        n <- 10*n
                }
        }
        if (verbose)
                cat(" (-> ",n,")","\t... marginal = ",marginal.time,"\tduration = ",run.time, sep="")
       
        ## return marginal generation time
        return (marginal.time)
}

## --- End ------------------------------------------------------------------
