## --------------------------------------------------------------------------
##'
##' Engine for performing tests for a set of parameter values
## 
## --------------------------------------------------------------------------
##
##  @description
##' 
##' Perform tests on the output of a random variate generator for all
##' combinations of the given parameter values for the distribution.
##' 
## --------------------------------------------------------------------------
## 
##  @details
## 
##' Routine \code{rvgt.range.engine} is the workhorse for performing tests
##' encoded in function \code{test.routine} on the output of random variate
##' generator \code{rdist} for a given set of parameter values.
##' This set is provided by arguments \code{dist.params} and \code{r.params}
##' where each entry of these lists corresponds to a parameter and holds the
##' vector of possible values.
##'
##' Detailed description of the arguments of this functions are given in the
##' sections below.
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
##' The values for the parameters of the distribution must be passed to
##' routine \code{rvgt.range.engine} by means of argument
##' \code{dist.params} as list.
##' Each entry of this list corresponds to a paramter as used in the call
##' to \code{rdist}.
##' Each of these entries contains either a number when
##' the value for that parameter is kept fixed or a vector of numbers.
##' In the latter case \code{rvgt.range.engine} runs through all possible
##' combinations of parameter values.
##' (Parameters of the distribution that have defaults in \code{rdist}
##' need not be listed in argument \code{dist.params}.)
##'
##' Function \code{rdist} may also have some additional arguments,
##' e.g., debugging flags or parameters for adjusting the algorithm.
##' This can be useful for testing experimental generators.
##' These additional arguments can be provided via argument
##' \code{r.params}. The entries of this list are handled in the same
##' way as those in \code{dist.params}.
##'
##' \emph{Some} tests require properties of the random variate
##' generator that can only be collected during setup (like the
##' rejection constant) or running the random variate generator
##' \code{rdist} (like the observed rejection rate).
##' These properties must be stored in the returned sample
##' as attributes. 
##' In order to run \emph{such} tests function \code{rdist}
##' \itemize{
##' \item must except argument \code{show.properties},
##' \item must return its properties (like the rejection constant)
##'       as attributes when \code{show.properties=TRUE}.
##' }
##' See function \code{\link{rvgt.range.trc}} or
##' \code{\link{rvgt.range.orc}} for examples.
##'
##' An example implementation of a random variate generator that
##' stores some of its properties as attributes of the returned random
##' vector is given by \code{\link{rvgt.range.rdist.example}}.
##' 
## ..........................................................................
##'
##' @section Test routine:
##' 
##' Routine \code{test.routine} actually performs the test on a
##' particular combination of parameter values.
##' It has to accept the following arguments:
##' \describe{
##'   \item{rdist:}{
##'         random number generator of distribution. (function)}
##'   \item{dist.params:}{
##'         parameter values for distribution. (list)}
##'   \item{r.params:}{
##'         additional arguments for \code{rdist}. (list)}
##'   \item{emgt:}{
##'         expected (approximate) marginal generation time of
##'         \code{rdist} in seconds. (numeric)}
##'   \item{test.params:}{
##'         additional arguments for \code{test.routine}. (list)}
##'   \item{duration:}{
##'         scheduled duration for the test in seconds. (numeric)}
##'   \item{verbose:}{
##'         if \code{TRUE} show progress. (logical)}
##' }
##' Thus the function prototype reads
##' \code{test.routine(rdist, dist.params, r.params, emgt, test.params, duration, verbose)}.
##' 
##' When \code{test.routine} is called then the corresponding arguments
##' from \code{rvgt.range.engine} are just passed to this function.
##' However, each entry in \code{dist.params} or \code{r.params}
##' contains one of the possible values.
##' Argument \code{emgt} contains the corresponding
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
##' some time constraints.
##'
##' Function \code{rvgt.range.engine} provides a simple
##' mechanism to control running times. By argument \code{gen.time} one can
##' provide the (approximate) marginal generation time for random
##' variate generator \code{rdist}. By argument \code{duration} one
##' can set the required running time for each of the tests. However,
##' the two parameters are just passed to function \code{test.routine}
##' and it is the task of this routine to make use this
##' information. Thus \code{duration} times the total number of tests 
##' (i.e., the number of possible tuples created from the given
##' \code{dist.params}) can only be used for a (very!) rough estimate
##' of the total running time.
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
##' \code{\link{rvgt.range.rdist.example}} for an example of a random
##' variate generator that stores some of its properties as attributes
##' of the returned random vector.
##' \code{\link{summary.rvgt.range}} for a summary of test results.
##' \code{\link{plot.rvgt.range}} for plotting the test results.
##' 
## --------------------------------------------------------------------------
##'
##' @author Josef Leydold \email{josef.leydold@@wu.ac.at}.
##'
##' Thanks to Simon Urbanek for helpful explanations of the low level
##' functions of the \pkg{parallel} package.
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
##' summary(samp)
##'
##' ## Plot sample
##' plot(samp)
##'
## --------------------------------------------------------------------------
##'
##  Arguments:
##' @param gen.data
##'        object of class \code{"rvgt.range.time"} that holds the
##'        result of a previous call to \code{\link{rvgt.range.marginal}}
##'        or \code{\link{rvgt.range.setup}}.
##'        It provides timing information and is used as defaults for some
##'        of the other arguments for this routine, see below.
##' @param rdist
##'        random number generator for distribution,
##'        default is read from \code{gen.data}.
##'        (function)
##' @param dist.params
##'        parameters for distribution, 
##'        default is read from \code{gen.data}.
##'        (non-empty list)
##' 
##'        If both \code{gen.data} and \code{dist.params} are provided, then
##'        \code{dist.params} is used to extract a subset of parameter values
##'        from \code{gen.data} via routine \code{\link{get.subrange}}.
##' @param r.params
##'        additional arguments for \code{rdist},
##'        default read from \code{gen.data}.
##'        (list)
##' 
##'        If both \code{gen.data} and \code{r.params} are provided, then
##'        \code{r.params} is used to extract a subset of parameter values
##'        from \code{gen.data} via routine \code{\link{get.subrange}}.
##' 
##' @param test.routine
##'        routine for performing tests. (function)
##' @param test.class
##'        class of test. (character string)
##' @param test.params
##'        additional arguments for \code{test.routine}. (list)
##' @param test.name
##'        a short description of the test. (character string)
##' @param needs.properties
##'        if \code{TRUE} then it is tested whether RVG \code{rdist} accepts
##'        argument \code{show.properties}. (logical)
##' @param duration
##'        scheduled duration for a single test in seconds. (numeric)
##' @param gen.time
##'        (approximate) marginal generation time for \code{rdist}.
##'        (positive numeric)
##'
##'        It has to have length 1.
##' 
##'        Default is read from \code{gen.data} which is then an array
##'        instead of a single number.
##'
##' @param ncores
##'        enable multicore support for performing tests in
##'        parallel and for setting timeout.
##'        The following values are accepted:
##'        \itemize{
##'        \item \code{NULL}:
##'              the tests run sequentially on a single core.
##'        \item A positive integer:
##'              the tests run on \code{ncores} cores in parallel.
##'              Notice that \code{ncores=1L} is possible.
##'              Then only one core is used but the behavior
##'              is different from \code{ncores=NULL}.
##'        \item \code{0L}:
##'              the number of available cores is autodetected by means of
##'              \code{\link[parallel]{detectCores}}.
##'        }
##'        Multicore support is only available on POSIX-compliant OSes
##'        (basically every but Windows OSes).
##' 
##' @param timeout
##'        set a timeout in seconds for each test. (numeric)
##' 
##'        This allows to protect against unexpected long running times
##'        or infinite loops.
##' 
##'        It requires multicore support.
##'        Thus \code{ncores} defaults to \code{1L} (instead of \code{NULL})
##'        if \code{timeout} is finite.
##' @param timeout.val
##'        value returned if a timeout is reached.  (numeric)
##'
##' @param verbose
##'        if \code{TRUE} show progress.
##'        Ignored when multicore support is enabled and \code{ncores} > 1.
##'        (logical)
##' 
## --------------------------------------------------------------------------
##'
##' @return
##' The function returns an object (list) with class attributes
##' \code{c("rvgt.range.testclass","rvgt.range")} if
##' \dQuote{testclass} is the string given by argument
##' \code{test.class}. If string \dQuote{testclass} contains one 
##' or more periods \code{.}, then a cascade of class names is created.
##' E.g., if \code{test.class="test.foo.bar"} then the class attributes is set to
##' \code{c("rvgt.range.test.foo.bar", "rvgt.range.test.foo", "rvgt.range.test", "rvgt.range")}.
##' 
##' The object (list) has the following components:
##' \item{data}{
##'        array that holds the test results for each combination of
##'        parameter values of the distribution.
##'        (array of numeric values)}
##' \item{rdist}{
##'        given function for calling random generator.
##'        (function, copied from input)}
##' \item{rdist.name}{
##'        name of given function for calling random generator.
##'        (character string copied from input)}
##' \item{dist.params}{
##'        given list of parameter values for the distribution.
##'        (list, copied from input)}
##' \item{r.params}{
##'        additional arguments for \code{rdist}.
##'        (list, copied from input)}
##' \item{test.class}{
##'        test class.
##'        (character string, copied from input)}
##' \item{test.name}{
##'        a short description of the test.
##'        (character string, copied from input)}
##' \item{started}{
##'        date and time when tests have been started.
##'        (object of class \code{"POSIXct"},
##'        see \code{\link{Sys.time}})}
##' \item{runtime}{
##'        total running time of all tests.
##'        (object of class \code{"difftime"},
##'        see \code{\link{difftime}})}
##'
## --------------------------------------------------------------------------

rvgt.range.engine <- function (gen.data=NULL,
                               rdist, dist.params, r.params=list(), 
                               test.routine, test.class,
                               test.params=list(), test.name=NULL,
                               needs.properties=FALSE,
                               duration=0.1, gen.time,
                               ncores=NULL, timeout=Inf, timeout.val=Inf,
                               verbose=FALSE) {
        ## ..................................................................

        ## --- timing information

        started <- Sys.time()

        ## --- 'gen.data'

        if (! (is.null(gen.data) || is(gen.data, "rvgt.range.time")) )
                stop("Argument 'gen.data' must be of class 'rvgt.range.time'")

        ## --- argument 'rdist'

        ## set defaults
        if (missing(rdist)) {
            if (!is.null(gen.data))
                rdist <- gen.data$rdist
            else
                stop("RVG 'rdist' is missing, with no default")
        }
        
        if (!is.function(rdist))
                stop("RVG 'rdist' is invalid")

        ## --- must function 'rdist' have argument 'show.properties' ?

        if (isTRUE(needs.properties)) {
            if (is.null(formals(rdist)$show.properties))
                stop("'rdist' must accept argument 'show.properties'.")
        }
        
        ## --- parameters for distribution (used by 'rdist')

        sub.params <- list()

        if (missing(dist.params)) dist.params <- NULL
        
        ## Case: neither 'dist.params' nor 'gen.data' is provided
        if (is.null(dist.params) && is.null(gen.data))
            stop ("Argument 'dist.params' is missing, with no default")

        ## Case: both 'dist.params' and 'gen.data' are provided
        if (!is.null(dist.params) && !is.null(gen.data)) 
            sub.params <- c(sub.params, dist.params)

        ## Case: 'dist.params' is missing but 'gen.data' is provided
        if (is.null(dist.params) && !is.null(gen.data))
            ## use defaults from 'gen.data'
            dist.params <- gen.data$dist.params

        ## check argument
        if (!is.list(dist.params) || identical(length(dist.params), 0L))
            stop("Argument 'dist.params' invalid")
            
        if (is.null(names(dist.params)) || "" %in% names(dist.params))
            ## distribution has unnamed parameters
            stop("List entries in 'dist.params' must have names")
            
        unsorted <- lapply(1:length(dist.params), function(i){is.unsorted(dist.params[[i]])})
        if (any(unsorted==TRUE))
            stop("List entries in 'dist.params' must be sorted")

        ## --- additional parameters for 'rdist' 

        ## Case: both 'r.params' and 'gen.data' are provided
        if (!identical(r.params, list()) && !is.null(gen.data)) 
            sub.params <- c(sub.params, r.params)

        if (identical(r.params, list()) && !is.null(gen.data))
            ## use defaults from 'gen.data'
            r.params <- gen.data$r.params

        if (!is.list(r.params))
                stop("Argument 'r.params' invalid")
        
        ## --- check whether a subset of parameter values is given

        if (!identical(sub.params, list())) {
            if (!isTRUE(all.equal(sub.params, c(gen.data$dist.params, gen.data$r.params)))) {
                gen.data <- get.subrange(gen.data, sub.params, drop=TRUE)
                dist.params <- gen.data$dist.params
                r.params <- gen.data$r.params
            }
        }

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
        if (timeout > 2^30) timeout <- 2^15  ## we cannot pass Inf to selectChildren

        if (! is.numeric(timeout.val))
                stop("Argument 'timeout.val' invalid.")
        
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
                message("Argument 'verbose' ignored when 'ncores' > 1")
                verbose <- FALSE
        }        

        ## --- expected marginal generation time

        ## check argument
        if (! missing(gen.time)) {
            if (! (is.numeric(gen.time) && length(gen.time)==1L && isTRUE(gen.time>0)) )
                stop("Argument 'gen.time' invalid")
        }
        
        ## set defaults
        if (missing(gen.time)) {
            if (!is.null(gen.data))
                gen.time <- gen.data
            else
                gen.time <- duration
        }

        if (is.numeric(gen.time)) {
                ## 'gen.time' is a single number:
                ## use 'gen.time' for each tuple of parameters
                emgt <- .alloc.data(c(dist.params,r.params), value=gen.time)

        } else if (is(gen.time, "rvgt.range.time")) {
                ## 'gen.time' is an object that contains
                ## marginal generation times:
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
                       rdist=rdist,
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

.alloc.data <- function(params, value=NA_real_) {
        dims <- as.integer(lapply(params,length))
        data <- rep(value, prod(dims))
        dim(data) <- dims
        dimnames(data) <- params
        data
}

## --- Sequential runs ------------------------------------------------------

.run.engine.sequential <- function (rdist, dist.params, r.params, emgt,
                                    test.routine, test.params, duration,
                                    verbose) {

        ## combine all parameters
        params <- c(dist.params,r.params)

        ## allocate array for results
        result <- .alloc.data(params)

        ## run test on each tuple of parameters
        for (pos in 1:length(result)) {

                ## position in array
                coord <- pos2coord(pos, dim(result))

                ## corresponding list of parameters
                p <- lapply(1:length(coord), function(i){(params[[i]])[coord[i]]})

                dp <- head(p, length(dist.params))
                names(dp) <- names(dist.params)

                rp <- list()
                if (length(r.params) >= 1L) {
                        rp <- tail(p, length(r.params))
                        names(rp) <- names(r.params)
                }
                
                ## print parameters
                if (verbose) {
                        ap <- c(rp, dp)
                        lapply(1:length(ap),
                               function(i) {cat(names(ap)[i]," = ",ap[[i]],", ",sep="")} )
                }

                ## compute and store
                result[pos] <- test.routine(rdist=rdist, dist.params=dp, r.params=rp,
                                            emgt=as.numeric(emgt[pos]),
                                            test.params=test.params, duration=duration,
                                            verbose=verbose)

                if (verbose) cat("\n")
        }

        ## return result
        result
}

## --- Multicore ------------------------------------------------------------

## Remark:
## The author of this code gratefully thanks Simon Urbanek for helpful
## explanations of the low level functions of the parallel package.

.run.engine.mc <- function (rdist, dist.params, r.params, emgt,
                            test.routine, test.params, duration,
                            ncores, timeout, timeout.val,
                            verbose) {

## ..................................................................
        
        ## combine all parameters
        params <- c(dist.params,r.params)

        ## allocate array for results
        result <- .alloc.data(params)

        ## bookkeeping
        finished <- .alloc.data(params, value=FALSE)

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
                        p <- lapply(1:length(coord), function(i){(params[[i]])[coord[i]]})
                        dp <- head(p, length(dist.params))
                        names(dp) <- names(dist.params)
                        rp <- list()
                        if (length(r.params) >= 1L) {
                                rp <- tail(p, length(r.params))
                                names(rp) <- names(r.params)
                        }
                        
                        ## print parameters
                        if (verbose) {
                                ap <- c(rp, dp)
                                lapply(1:length(ap),
                                       function(i) {cat(names(ap)[i]," = ",ap[[i]],", ",sep="")} )
                        }
                        
                        ## start thread that performs a single test
                        rvgt.mcparallel(
                          test.routine(rdist=rdist, dist.params=dp, r.params=rp,
                                       emgt=as.numeric(emgt[pos]),
                                       test.params=test.params, duration=duration,
                                       verbose=verbose))
                }

                ## start all parallel threads
                jobs <- lapply(run.idx, test.routine.mc)
                jobIDs <- rvgt.processID(jobs)
                jobaborttime <- proc.time()[3] + timeout
                
                ## get results from started jobs
                while(TRUE) {

                        ## check all children for available data
                        s <- rvgt.selectChildren(jobs, timeout=max(0, jobaborttime - proc.time()[3]))

                        if (is.null(s)) {
                                ## no more childs
                                break
                        }

                        if (is.logical(s)) {
                            ## 3 Cases:
                            ## (1) TRUE: timeout has been reached
                            ## (2) TRUE: caught signal (before timeout has been reached)
                            ##           (the help page for parallel:::selectChildren
                            ##            is not accurate here.)
                            ## (3) FALSE: an error has occured
                            if (proc.time()[3] < jobaborttime) {
                                ## Case (2) [ or maybe case (3) ]
                                ## check again until we get data or timeout has been reached
                                next
                            }
                            
                            ## proc.time()[3] >= jobaborttime:
                            ## Case (1): timeout --> kill all active jobs
                            active.jobs <- rvgt.children(jobs)
                            rvgt.mckill(active.jobs, tools::SIGINT)
                            for (ch in rvgt.processID(active.jobs)) {
                                i <- run.idx[(which(jobIDs==ch)[1])]
                                result[i] <- timeout.val
                                finished[i] <- TRUE
                                ## close pipe to (terminated) child process 
                                rvgt.readChild(ch)
                            }
                            if (verbose) cat("\t---> timeout!\n")
                            break
                        }

                        ## read all results
                        for (ch in s) {
                            r <- rvgt.readChild(ch)
                            i <- run.idx[(which(jobIDs==ch)[1])]
                            if (is.raw(r)) {
                                res <- unserialize(r)
                                if (! is.numeric(res)) {
                                    if (verbose) cat("\t --> error!")
                                    res <- NA_real_
                                }
                                result[i] <- res
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
