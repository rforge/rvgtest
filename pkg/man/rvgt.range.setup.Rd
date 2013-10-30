\name{rvgt.range.setup}
\alias{rvgt.range.setup}
\title{Estimate setup time of random variate generator}
\usage{
  rvgt.range.setup(rdist, dist.params, r.params = list(),
    duration = 0.1, gen.time = duration, repetitions = 1L,
    ncores = NULL, timeout = Inf, verbose = FALSE)
}
\arguments{
  \item{repetitions}{number of repetitions of the test.
  Then the median of the test results is used to estimate
  the setup time (positive integer).}

  \item{rdist}{random number generator for distribution
  (function).}

  \item{dist.params}{parameters for distribution (non-empty
  list).}

  \item{r.params}{additional arguments for \code{rdist}
  (list).}

  \item{duration}{scheduled duration for a single test in
  seconds (numeric).}

  \item{gen.time}{(approximate) marginal generation time
  for \code{rdist}.  It must be either a single positive
  number (numeric) or an object of class
  \code{"rvgt.range.time"} that holds the result of a
  previous call to routine
  \code{\link{rvgt.range.marginal}}.}

  \item{ncores}{enable multicore support for performing
  tests in parallel and for setting timeout.  The following
  values are possible: \itemize{ \item \code{NULL}: the
  tests run sequentially on a single core.  \item A
  positive integer: the tests run on \code{ncores} cores in
  parallel.  Notice that \code{ncores=1L} is possible.
  Although then only one core is used the behavior is
  different from \code{ncores=NULL}.  \item \code{0L}: the
  number of available cores is autodetected by means of
  \code{\link[parallel]{detectCores}}.  } Multicore support
  is only available on POSIX-compliant OSes (basically
  every but Windows OSes).}

  \item{timeout}{set a timeout in seconds for each test
  (numeric).  This allows to protect against unexpected
  long running times or infinite loops.  It requires
  multicore support which is enabled by setting argument
  \code{ncores}.  (When a timeout is set then \code{ncores}
  is automatically set to \code{1L} if it is not already
  set by the user.)}

  \item{verbose}{if TRUE show progress.  Ignored when
  multicore support is enabled and 'ncores' > 1 (logical).}
}
\value{
  The function returns an object of class
  \code{"rvgt.range.time.setup"} where the observed setup
  times are stored in field \code{$data}, see
  \code{\link{rvgt.range.engine}} for a description of such
  objects. The routine returns \code{NA} in all cases where
  the setup failed, and \code{Inf} when a timeout has been
  reached.
}
\description{
  Estimate running time of setup and generation of one
  random number for the given random variate generator for
  a range of parameters.
}
\details{
  Routine \code{rvgt.range.setup} estimates the running for
  generating exactly one random number (inluding the setup
  of the generatur) with the given random variate generator
  for a range of parameters. (This corresponds to the
  \dQuote{varying parameter case} in applications). For
  this purpose the running time returned by
  \code{\link{system.time}} is used.

  The scheduled sampling time for a particular combination
  of parameters is given by parameter \code{duration}.
  Notice that it must not be too small compared to the
  resolution of the system clock. The required sample size
  is then estimated in a sequence of tries where the sample
  size is increasing after each run. This iterative process
  is stopped if the total running time is close to
  \code{duration}.

  The total running time for sampling one random variate
  can be too small compared to the resolution of the system
  clock. Thus the call to generator \code{rdist} has to be
  repeated sufficiently often using a \code{for} loop.
  However, this also includes the overhead for calling an
  \R function. For experimental generators that are
  implemented at C level there is a workaround: if
  generator \code{rdist} accepts argument \code{rep} it is
  assumed that this \code{for} loop is implemented at C
  level, too, where \code{rep} is the number of
  repetitions.

  Timings can vary considerably. In order to get a more
  robust estimate one can repeat this test several times
  and use the median of all timings. This can be achieved
  by means of argument \code{repetitions}. Notice, that
  this of course increases the total running time. This has
  to be taken into account if a timeout is set.

  A big issue of tests on a large range of parameter
  settings is that the running times may not be known in
  advance. Thus the total running time of the test suite
  may be extremely long due to some unexpected long runs
  for some tests. In order to avoid such problems the
  running times for each test can be limited by means of
  argument \code{timeout} which required to enable
  multicore support using \code{ncores}.
}
\examples{
## Estimate setup time for beta distribution
sut <- rvgt.range.setup(rdist = rbeta,
                        dist.params = list(
                           shape1=c(0.1,1,10),
                           shape2=c(0.1,1,10)),
                        duration = 0.01, verbose=FALSE)

## show summary of setup times
summary(sut)
}
\author{
  Josef Leydold \email{josef.leydold@wu.ac.at}
}
\seealso{
  \code{\link{rvgt.range.engine}} for a description of
  objects of class \code{"rvgt.range"}.
  \code{\link{summary.rvgt.range}} for a summary of test
  results, \code{\link{plot.rvgt.range}} for plotting the
  test results.
}

