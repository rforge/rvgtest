## --------------------------------------------------------------------------
##'
##' Estimate setup time of random variate generator
## 
## --------------------------------------------------------------------------
##
##  @description
##' 
##' Estimate running time of setup and generation of one random number
##' for the given random variate generator for a range of parameters. 
##' 
## --------------------------------------------------------------------------
## 
##  @details
## 
##' Routine \code{rvgt.range.setup} estimates the running for generating
##' exactly one random number (inluding the setup of the generatur)
##' with the given random variate generator for a range of parameters.
##' (This corresponds to the \dQuote{varying parameter case} in applications).
##' For this purpose the running time returned by
##' \code{\link{system.time}} is used.
##'
##' The scheduled sampling time for a particular combination of parameters
##' is given by parameter \code{duration}. Notice that it must not be too
##' small compared to the resolution of the system clock.
##' The required sample size is then estimated in a sequence of trials where
##' the sample size is \emph{increased} after each run.
##' (So the first guess of the marginal generation time should be
##' rather too large than too small.)
##' This iterative process is stopped if the total running time is close to
##' \code{duration}.
##'
##' The total running time for sampling one random variate can be too small
##' compared to the resolution of the system clock. Thus the call to
##' generator \code{rdist} has to be repeated sufficiently often using a
##' \code{for} loop. However, this also includes the overhead for calling an
##' \R function. For experimental generators that are implemented at C level
##' there is a workaround: if generator \code{rdist} accepts argument \code{rep}
##' it is assumed that this \code{for} loop is implemented at C level, too,
##' where \code{rep} is the number of repetitions. 
##'
##' Timings can vary considerably. In order to get a more robust estimate
##' one can repeat this test several times and use the median of all timings.
##' This can be achieved by means of argument \code{repetitions}.
##' Notice, that this of course increases the total running time.
##' This has to be taken into account if a timeout is set.
##' 
##' A big issue of tests on a large range of parameter settings is that the
##' running times may not be known in advance. Thus the total running time of
##' the test suite may be extremely long due to some unexpected long runs
##' for some tests. 
##' In order to avoid such problems the running times for each test can be
##' limited by means of argument \code{timeout} which required to enable
##' multicore support using \code{ncores}.
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
##' ## Estimate setup time for beta distribution
##' sut <- rvgt.range.setup(rdist = rbeta,
##'                         dist.params = list(
##'                            shape1=c(0.1,1,10),
##'                            shape2=c(0.1,1,10)),
##'                         duration = 0.01, verbose=FALSE)
##'
##' ## show summary of setup times
##' summary(sut)
##'
## --------------------------------------------------------------------------
##'
##  Arguments:
##' @inheritParams rvgt.range.engine
##' 
##' @param repetitions
##'        number of repetitions of the test. (positive integer)
##' 
##'        The marginal generation time is estimated by the median of
##'        all test results.
##'
## --------------------------------------------------------------------------
##'
##' @return
##' The function returns an object of class \code{"rvgt.range.time.setup"}
##' where the observed setup times are stored in field \code{$data},
##' see \code{\link{rvgt.range.engine}} for a description of such objects.
##' The routine returns \code{NA} in all cases where the setup failed, and
##' \code{Inf} when a timeout has been reached.
##' 
## --------------------------------------------------------------------------

rvgt.range.setup <- function (gen.data=NULL,
                              rdist, dist.params, r.params=list(), 
                              duration=0.1, gen.time, repetitions=1L,
                              ncores=NULL, timeout=Inf, verbose=FALSE) {
        ## ..................................................................

        ## --- number of repetitions

        if (! (is.numeric(repetitions) && length(repetitions)==1L && isTRUE(repetitions>=1)))
                stop("Argument 'repetitions' invalid")
        repetitions <- as.integer(repetitions)

        ## --- run test engine

        test.routine <- if (isTRUE(repetitions > 1L)) .run.setup.reps else .run.setup.single
        
        rvgt.range.engine(gen.data = gen.data,
                          rdist = rdist,
                          dist.params = dist.params,
                          r.params = r.params,
                          test.routine = test.routine,
                          test.class = "time.setup",
                          test.params = list(
                                  repetitions=repetitions,  ## number of repetitions
                                  esut=duration),           ## expected setup time
                          duration = duration,
                          gen.time = gen.time,
                          ncores = ncores,
                          timeout = timeout,
                          timeout.val = Inf,
                          verbose = verbose)
}

## --- estimate setup time --------------------------------------------------

.run.setup.reps <- function (rdist, dist.params, r.params, emgt,
                             test.params, duration, verbose) {

        ## prepare vector for particular results
        repetitions <- test.params$repetitions   ## number of repetitions
        results <- rep(NA_real_, repetitions)

        ## run test 'repetitions' times
        for (k in 1:repetitions) {
                if (verbose) cat("\n\trun ",k,": ", sep="")
                results[k] <- .run.setup.single(rdist, dist.params, r.params, emgt,
                                                test.params, duration, verbose)
                ## use setup time for next run
                test.params = list(esut=results[k])
        }
        
        ## return median of all runs
        median(results)
}

## ..........................................................................

.run.setup.single <- function (rdist, dist.params, r.params, emgt,
                               test.params, duration, verbose) {


        ## --- approximate marginal generation times

        ## We have to draw 1 random variate.
        ## So we can use 'emgt' to get a lower bound for the
        ## setup time.
        
        ## Check whether we can expect that the setup of rdist() works at all.
        ## Case 'NA':  setup failed in a previous run
        if (is.na(emgt)) {
                if (verbose) cat("\t---> setup failed!")
                return (NA)
        }
        ## Case 'Inf' or 'emgt' > 'duration':
        ##    The marginal generation time was too slow
        if (! (is.finite(emgt) && emgt < 1.1*duration)) { 
                if (verbose) cat("\t---> too slow!")
                return (Inf)
        }
        
        ## --- estimate required sample size

        esut <- test.params$esut                 ## expected setup time

        ## check whether we can expect that the setup of rdist() works
        ## case 'NA':  setup failed in a previous run
        if (is.na(esut)) {
                if (verbose) cat("\t---> setup failed!")
                return (NA)
        }
        ## case 'Inf': the generation time was too slow
        if (!is.finite(esut)) {
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
        k <- round(duration/esut)

        ## check sample size 'n'
        ## we only run the test the expected runtime is not too large
        ## i.e., when the sample size is > 0
        if (verbose) cat("k =", k)
        if (!isTRUE(k > 0)) {
                if (verbose) cat("\t---> too slow!")
                return (Inf)
        }

        ## --- run simulation

        ## draw sample
        while (TRUE) {

                ## get total runtime
                if (is.null(formals(rdist)$rep)) {
                        ## generator 'rdist' does not have argument 'rep'
                        cl <- as.call(c(list(name=rdist, n=1), dist.params, r.params))
                        run.time <- get.runtime(system.time(for(i in 1:k) eval(cl)))
                } else {
                        ## generator 'rdist' has argument 'rep'
                        cl <- as.call(c(list(name=rdist, n=1, rep=k), dist.params, r.params))
                        run.time <- get.runtime(system.time(eval(cl)))
                }

                ## compute setup time
                setup.time <- run.time / k

                ## check order of magnitude of total runtime
                if (run.time > 0.8*duration) {
                        ## runtime approx scheduled duration --> stop
                        break
                } else if (run.time > duration/10) {
                        ## runtime close to scheduled duration --> use as esut
                        k <- round(duration/setup.time)
                } else {
                        ## measured runtime too small --> increase sample size
                        k <- 10*k
                }
        }
        
        if (verbose)
                cat(" (->",k,")","\t... sut = ",setup.time,"\tduration = ",run.time, sep="")
       
        ## return setup time
        return (setup.time)
}

## --- End ------------------------------------------------------------------
