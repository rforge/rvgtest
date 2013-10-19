## --------------------------------------------------------------------------
##'
##' Engine for performing tests for a set of parameters
## 
## --------------------------------------------------------------------------
##
##  @description
##' 
##' Perform tests on the output of a random variate generator for all
##' combinations of the given parameters for the distribution.
##' 
## --------------------------------------------------------------------------
## 
##  @details
## 
##' Routine \code{rvgt.range.engine} is the workhorse for performing tests
##' encoded in function \code{test.routine} on the output of random variate
##' generator \code{rdist} for a given set of parameters.
##' This set is provided by argument \code{dist.params} where each entry
##' corresponds to a parameter and holds the vector of possible values.
##'
##' Detailed description of the arguments are given in the sections below.
##'
## ..........................................................................
##'
##' @section Random variate generator:
##' 
##' The arguments of random variate generator \code{rdist} must be similar
##' to that of the \R built-in random variate generators such as
##' \code{\link{rnorm}}:
##' the first argument must be the sample size \code{n} followed by the
##' parameters of the distribution.
##'
##' The values for the parameters of the distribution must be given in
##' argument \code{dist.params} as list.
##' Each entry of this list corresponds to a paramter as used in the call
##' to \code{rdist}. Each of these entries contains either a number when
##' the value for that parameter is kept fixed or a vector of numbers.
##' In the latter case \code{rvgt.range.engine} runs through all possible
##' combinations of parameter values.
##' (Parameters of the distribution that have defaults need not be listed in
##' argument \code{dist.params}.)
##'
##' Function \code{rdist} may also have some additional arguments,
##' e.g., debugging flags. This can be useful for testing experimental
##' generators. Additional arguments can be provided via argument
##' \code{r.params}. The entries of this list (if non-empty) are used as-is
##' as arguments for \code{rdist}.
##'
##' Some tests require propoerties of the random variate generator that
##' can only be collected during setup (like the rejection constant)
##' or running random variate generator \code{rdist} (like rejection rate).
##' These properties must be stored in the returned sample
##' as attributes. 
##' In order to run these tests function \code{rdist}
##' \itemize{
##' \item must except argument \code{show.properties},
##' \item must return its properties (like the rejection constant)
##'       as attributes when \code{show.properties=TRUE}.
##' }
##' 
## ..........................................................................
##'
##' @section Test routine:
##' 
##' Routine \code{test.routine} actually performs the test on a
##' single combination of parameter values.
##' It has to accept the following arguments:
##' \describe{
##'   \item{\code{rdist}}{%
##'         random number generator of distribution (function).}
##'   \item{\code{dist.params}}{%
##'         parameters for distribution (list).}
##'   \item{\code{r.params}}{%
##'         additional arguments for \code{rdist} (list).}
##'   \item{\code{emgt}}{%
##'         expected (approximate) marginal generation of \code{rdist} (in seconds).}
##'   \item{\code{test.params}}{%
##'         additional arguments for \code{test.routine} (list).}
##'   \item{\code{duration}}{%
##'         scheduled duration for test (in seconds).}
##'   \item{\code{verbose}}{%
##'         if TRUE show progress (logical).}
##' }
##' The function prototype reads
##' \code{test.routine(rdist, dist.params, r.params, emgt, test.params, duration, verbose)}.
##' 
##' When \code{test.routine} is called then the corresponding arguments
##' from \code{rvgt.range.engine} are just passed to this function.
##' However, each entry in \code{dist.params} contains one of the
##' possible values. Argument \code{emgt} contains the corresponding
##' generation time as given by \code{gen.time}.
##'
##' Further arguments that are needed to run a test (e.g., the CDF
##' or quantile function) have to be passed via list \code{test.params}.
##'
##' A test should belong to some class which has to be given by
##' argument \code{test.class}.
##' It is used to create the class names of the object returned by
##' \code{rvgt.range.engine}.
##' 
## ..........................................................................
##'
##' @section Duration and timeout:
##'
##' When testing a wide range of parameter settings running time is an issue.
##' Each test must run sufficiently long in order to get a sensible result.
##' On the other hand the total running time of the test suite must not exceed
##' some constraints.
##'
##' Function \code{rvgt.range.engine} provides a simple
##' mechanism to control running times. By argument \code{gen.time} one can
##' provide the (approximate) marginal generation time for random variate generator
##' \code{rdist}. By argument \code{duration} one can set the require running
##' time for each of the tests. However, the two parameters are just passed to
##' function \code{test.routine} and it is the task of this routine to use
##' this information. Thus \code{duration} times the total number of tests
##' (i.e., the number of possible tuples created from the given
##' \code{dist.params}) can only be used for a (very!) rough estimate of the
##' total running time.
##'
##' In order to prevent one of the tests from running too long (or from
##' looping infinitely) argument \code{timeout} allows to set an upper limit
##' for the running time of each test. Notice, however, that this feature requires
##' multicore support and is thus available only on POSIX-compliant OSes (i.e.,
##' not on Windows OSes). It can be enabled by means of argument \code{ncores}.
##' 
## --------------------------------------------------------------------------
##'
##' @seealso
##' \code{\link{print.rvgt.range}} for printing a summary of test results,
##' \code{\link{plot.rvgt.range}} for plotting the test results.
##' 
## --------------------------------------------------------------------------
##'
##' @author Josef Leydold \email{josef.leydold@@wu.ac.at}
##'
## --------------------------------------------------------------------------
##'
##' @examples
##' ## Simple "test" that just samples one random number
##' create.sample <- function (rdist, dist.params, ...) {
##'    cl <- as.call(c(list(name=rdist,n=1),dist.params))
##'    eval(cl)
##' }
##'
##' ## Run this "test" on rnorm() for various values of 'mean' and 'sd'
##' samp <- rvgt.range.engine(rdist=rnorm,
##'                           dist.params=list(mean=0:10, sd=1:10),
##'                           test.routine=create.sample,
##'                           test.class="sample")
##'
##' ## print summary
##' print(samp)
##'
##' ## Plot sample
##' plot(samp)
##'
## --------------------------------------------------------------------------
##'
##  Arguments:
##' @param rdist
##'        random number generator for distribution (function).
##' @param dist.params
##'        parameters for distribution (non-empty list).
##' @param r.params
##'        additional arguments for \code{rdist} (list).
##' 
##' @param test.routine
##'        routine for performing tests (function).
##' @param test.class
##'        class of test (character string).
##' @param test.params
##'        additional arguments for test.routine (list).
##' @param test.name
##'        a short description of the test (character string).
##' @param duration
##'        scheduled duration for a single test in seconds (numeric).
##' @param gen.time
##'        (approximate) marginal generation time for \code{rdist}.
##'        It must be either a single positive number (numeric) or
##'        an object of class \code{"rvgt.range.time"} that holds the
##'        result of a previous call to routine \code{\link{rvgt.range.marginal}}.
##'
##' @param ncores
##'        enable multicore support for performing tests in
##'        parallel and for setting timeout.
##'        The following values are possible:
##'        \itemize{
##'        \item \code{NULL}:
##'              the tests run sequentially on a single core.
##'        \item A positive integer:
##'              the tests run on \code{ncores} in parallel.
##'              Notice that \code{ncores=1L} is possible.
##'              Although then only one core is used the behavior
##'              is different from \code{ncores=NULL}.
##'        \item \code{0L}:
##'              the number of available cores is autodetected by means of
##'              \code{\link[parallel]{detectCores}}.
##'        }
##'        Multicore support is only available on POSIX-compliant OSes
##'        (basically every but Windows OSes).
##' 
##' @param timeout
##'        set a timeout in seconds for each test (numeric).
##'        This allows to protect against unexpected long running times
##'        or infinite loops.
##'        It requires multicore support which is enabled by setting
##'        argument \code{ncores}.
##'        (When a timeout is set then \code{ncores} is automatically
##'        set to \code{1L} if it is not already set by the user.) 
##' @param timeout.val
##'        value returned if a timeout is reached (numeric, including Inf).
##'
##' @param verbose
##'        if TRUE show progress.
##'        Ignored when multicore support is enabled and 'ncores' > 1 (logical).
##' 
## --------------------------------------------------------------------------
##'
##' @return
##' The function returns an object (list) with class attributes
##' \code{c("rvgt.range.testclass","rvgt.range"} if \emph{testclass} is the string
##' given by argument \code{test.class}). If string \emph{testclass} contains one
##' or more periods \code{'.'}, then a cascade of class names is created.
##' E.g., if \code{test.class="test.foo.bar"} then the class attributes is set to
##' \code{c("rvgt.range.test.foo.bar", "rvgt.range.test.foo", "rvgt.range.test", "rvgt.range")}.
##' 
##' The list has the following components:
##' \item{data}{
##'        array that holds the test results for each combination of
##'        parameters of the distribution
##'        (array of numeric values).}
##' \item{rdist.name}{
##'        given function for calling random generator
##'        (character string copied from input).}
##' \item{dist.params}{
##'        given list of parameter for the distribution
##'        (list copied from input).}
##' \item{r.params}{
##'        additional arguments for \code{rdist}
##'        (list copied from input).}
##' \item{test.class}{
##'        test class
##'        (character string, copied from input).}
##' \item{test.name}{
##'        a short description of the test
##'        (character string, copied from input).}
##' \item{started}{
##'        date and time when tests have been started
##'        (object of class \code{"POSIXct"},
##'        see \code{\link{Sys.time}}).}
##' \item{runtime}{
##'        total running time of all tests
##'        (object of class \code{"difftime"},
##'        see \code{\link{difftime}}).}
##'
## --------------------------------------------------------------------------

rvgt.range.engine <- function (rdist, dist.params, r.params=list(), 
                               test.routine, test.class,
                               test.params=list(), test.name=NULL,
                               duration=0.1, gen.time=duration,
                               ncores=NULL, timeout=Inf, timeout.val=Inf,
                               verbose=FALSE) {
        ## ..................................................................
        
        ## --- timing information

        started <- Sys.time()
        
        ## --- arguments for RVG and distribution

        if (missing(rdist) || !is.function(rdist))
                stop("RVG 'rdist' is missing or invalid")

        if (!is.list(r.params))
                stop("Argument 'r.params' invalid")
                
        ## --- parameters for distribution (used by 'rdist')

        ## check arguments for distributions
        if (missing(dist.params) ||
            !is.list(dist.params) ||
            identical(length(dist.params), 0L))
                stop("Argument 'dist.params' missing or invalid")
                
        if (is.null(names(dist.params)) || "" %in% names(dist.params))
                ## distribution has unnamed parameters
                stop("List entries in 'dist.params' must have names")

        unsorted <- lapply(1:length(dist.params), function(i){is.unsorted(dist.params[[i]])})
        if (any(unsorted==TRUE))
                stop("List entries in 'dist.params' must be sorted")

        ## --- arguments for test

        if (missing(test.routine) || !is.function(test.routine))
                stop("Argument 'test.routine' is missing or invalid")
        
        if (missing(test.class) || !is.character(test.class))
                stop("Argument 'test.class' is missing or invalid")

        if (! (is.null(test.name) || is.character(test.name)))
                stop("Argument 'test.name' invalid")

        if (!is.list(test.params))
                stop("Argument 'test.params' invalid")
                
        if (! (is.numeric(duration) && length(duration)==1 && isTRUE(duration>0)) )
                stop("Argument 'duration' invalid")

        ## --- arguments for setting timeout

        if (! (is.numeric(timeout) && timeout >= 0))
                stop("Argument 'timeout' invalid.")

        if (is.finite(timeout)) {
                if (.Platform$OS.type != "unix")
                        stop("Timeout is not supported on this plattform.")
                if (is.null(ncores)) {
                        message("Timeout requires multicore support. Argument 'ncores' set to 1L.")
                        ncores=1L
                }
        }
        if (timeout > 2^30) timeout <- 2^30  ## we cannot pass Inf to selectChildren
        
        ## --- arguments for multicore support

        if (! is.null(ncores)) {
                if (.Platform$OS.type != "unix")
                        stop("Multicore supported is not available on this plattform.")
                ncores <- as.integer(ncores)
                if (ncores < 0L)
                        stop("Argument 'ncores' must be non-negative.")
                if (identical(ncores,0L)) {
                        ncores <- parallel::detectCores()
                        if (is.na(ncores))
                                stop("Cannot autodetect number of cores. Please specify 'ncores'.")
                }
        }

        ## --- verbosity

        if (! is.logical(verbose))
                stop("Argument 'verbose' invalid")

        if (verbose && isTRUE(ncores>1L)) {
                message("Argument 'verbose' ignored when 'ncore' > 1")
                verbose <- FALSE
        }        

        ## --- expected marginal generation time

        if (is.numeric(gen.time)) {
                if (! (length(gen.time)==1L && isTRUE(gen.time>0)) )
                        stop("Argument 'gen.time' invalid")

                ## 'gen.time' is a single number:
                ## use 'gen.time' for each tuple of parameters
                emgt <- .alloc.data(dist.params, value=gen.time)

        } else if (is(gen.time, "rvgt.range.time")) {

                ## 'gen.time' is an object that contains
                ## marginal generation times:
                if (! isTRUE(all.equal(dist.params, gen.time$dist.params)))
                        stop("Object 'gen.time' contains incompatible element 'dist.params'")
                emgt <- gen.time$data

        } else {
                stop("Argument 'gen.time' invalid")
        }

        ## --- run test on each tuple of parameters

        if (is.null(ncores)) {
                result <- .run.engine.sequential(rdist, dist.params, r.params, emgt,
                                                 test.routine, test.params, duration,
                                                 verbose)
        } else {
                result <- .run.engine.mc(rdist, dist.params, r.params, emgt,
                                         test.routine, test.params, duration,
                                         ncores, timeout, timeout.val,
                                         verbose)
        }
        
        ## --- combine all data into list

        retval <- list(data=result,
                       rdist.name=deparse(substitute(rdist)),
                       dist.params=dist.params,
                       r.params=r.params,
                       test.class=test.class,
                       test.name=test.name,
                       started=started,
                       runtime=Sys.time()-started)

        
        ## set class
        tmp <- str_split(test.class,"\\.")[[1]]
        clns <- "rvgt.range"
        for (i in 1:length(tmp))
                clns <- c(paste("rvgt.range", paste(tmp[1:i],collapse="."), sep="."), clns)

        class(retval) <- clns

        ## return result
        retval
}

## === Auxiliary functions ==================================================

## --- Allocate array for storing result ------------------------------------

.alloc.data <- function(dist.params, value=NA_real_) {
        dims <- as.integer(lapply(dist.params,length))
        data <- rep(value, prod(dims))
        dim(data) <- dims
        dimnames(data) <- dist.params
        data
}

## --- Sequential runs ------------------------------------------------------

.run.engine.sequential <- function (rdist, dist.params, r.params, emgt,
                                    test.routine, test.params, duration,
                                    verbose) {

        ## allocate array for results
        result <- .alloc.data(dist.params)

        ## run test on each tuple of parameters
        for (pos in 1:length(result)) {

                ## position in array
                coord <- pos2coord(pos, dim(result))
                ## corresponding list of parameters
                dp <- lapply(1:length(coord), function(i){(dist.params[[i]])[coord[i]]})
                names(dp) <- names(dist.params)

                ## print parameters
                if (verbose) {
                        ap <- c(r.params, dp)
                        lapply(1:length(ap),
                               function(i) {cat(names(ap)[i]," = ",ap[[i]],", ",sep="")} )
                }

                ## compute and store
                result[pos] <- test.routine(rdist=rdist, dist.params=dp, r.params=r.params,
                                            emgt=emgt[pos],
                                            test.params=test.params, duration=duration,
                                            verbose=verbose)

                if (verbose) cat("\n")
        }

        ## return result
        result
}

## --- Multicore ------------------------------------------------------------

.run.engine.mc <- function (rdist, dist.params, r.params, emgt,
                            test.routine, test.params, duration,
                            ncores, timeout, timeout.val,
                            verbose) {

        ## allocate array for results
        result <- .alloc.data(dist.params)

        ## bookkeeping
        finished <- .alloc.data(dist.params, value=FALSE)

        while(!all(finished==TRUE)) {
                ## we use a simple strategy:
                ## we starts threads on each cores and wait until
                ## all jobs are finished or until timeout has reached.
                
                ## list of parallel jobs
                ## the next 'ncores' jobs
                run.idx <- head(which(!finished), n=ncores)

                ## routine for starting thread
                test.routine.mc <- function (pos) {
                        ## position in array
                        coord <- pos2coord(pos, dim(result))
                        ## corresponding list of parameters
                        dp <- lapply(1:length(coord), function(i){(dist.params[[i]])[coord[i]]})
                        names(dp) <- names(dist.params)

                        ## print parameters
                        if (verbose) {
                                ap <- c(r.params, dp)
                                lapply(1:length(ap),
                                       function(i) {cat(names(ap)[i]," = ",ap[[i]],", ",sep="")} )
                        }
                        
                        ## start thread that performs a single test
                        parallel::mcparallel(
                          test.routine(rdist=rdist, dist.params=dp, r.params=r.params,
                                       emgt=emgt[pos],
                                       test.params=test.params, duration=duration,
                                       verbose=verbose))
                }

                ## start all parallel threads
                jobs <- lapply(run.idx, test.routine.mc)
                jobIDs <- parallel:::processID(jobs)
                
                ## get results from started jobs
                while(TRUE) {

                        ## check all children for available data
                        s <- parallel:::selectChildren(jobs,timeout)

                        if (is.null(s)) {
                                ## no more childs
                                break
                        }
                        
                        if (isTRUE(s)) {
                                ## timeout --> kill all active jobs
                                active.jobs <- parallel:::children(jobs)
                                parallel:::mckill(active.jobs, tools::SIGTERM)
                                for (ch in parallel:::processID(active.jobs)) {
                                        i <- run.idx[(which(jobIDs==ch)[1])]
                                        result[i] <- timeout.val
                                        finished[i] <- TRUE
                                        ## close pipe to (terminated) child process 
                                        parallel:::readChild(ch)
                                }
                                if (verbose) cat("\t---> timeout!\n")
                                break
                        }

                        if (!is.integer(s)) {
                                ## something is wrong --> retry
                                ## Remark: Can this cause an infinite loop with too many childs?
                                break
                        }

                        ## read all results
                        for (ch in s) {
                                r <- parallel:::readChild(ch)
                                i <- run.idx[(which(jobIDs==ch)[1])]
                                if (is.raw(r)) {
                                        result[i] <- unserialize(r) 
                                        finished[i] <- TRUE
                                        if (verbose) cat("\n")
                                }
                        }
                }
        }

        ## return result
        result
}

## --- End ------------------------------------------------------------------
