\name{rvgt.range.engine}
\alias{rvgt.range.engine}
\title{Engine for performing tests for a set of parameters}
\usage{
  rvgt.range.engine(rdist, dist.params, r.params = list(),
    test.routine, test.class, test.params = list(),
    test.name = NULL, duration = 0.1, gen.time = duration,
    ncores = NULL, timeout = Inf, timeout.val = Inf,
    verbose = FALSE)
}
\arguments{
  \item{rdist}{random number generator for distribution
  (function).}

  \item{dist.params}{parameters for distribution (non-empty
  list).}

  \item{r.params}{additional arguments for \code{rdist}
  (list).}

  \item{test.routine}{routine for performing tests
  (function).}

  \item{test.class}{class of test (character string).}

  \item{test.params}{additional arguments for test.routine
  (list).}

  \item{test.name}{a short description of the test
  (character string).}

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

  \item{timeout.val}{value returned if a timeout is reached
  (numeric or Inf).}

  \item{verbose}{if TRUE show progress.  Ignored when
  multicore support is enabled and 'ncores' > 1 (logical).}
}
\value{
  The function returns an object (list) with class
  attributes \code{c("rvgt.range.testclass","rvgt.range"}
  if \emph{testclass} is the string given by argument
  \code{test.class}). If string \emph{testclass} contains
  one or more periods \code{.}, then a cascade of class
  names is created. E.g., if
  \code{test.class="test.foo.bar"} then the class
  attributes is set to \code{c("rvgt.range.test.foo.bar",
  "rvgt.range.test.foo", "rvgt.range.test", "rvgt.range")}.

  The list has the following components: \item{data}{ array
  that holds the test results for each combination of
  parameters of the distribution (array of numeric
  values).} \item{rdist.name}{ given function for calling
  random generator (character string copied from input).}
  \item{dist.params}{ given list of parameter for the
  distribution (list copied from input).} \item{r.params}{
  additional arguments for \code{rdist} (list copied from
  input).} \item{test.class}{ test class (character string,
  copied from input).} \item{test.name}{ a short
  description of the test (character string, copied from
  input).} \item{started}{ date and time when tests have
  been started (object of class \code{"POSIXct"}, see
  \code{\link{Sys.time}}).} \item{runtime}{ total running
  time of all tests (object of class \code{"difftime"}, see
  \code{\link{difftime}}).}
}
\description{
  Perform tests on the output of a random variate generator
  for all combinations of the given parameter values for
  the distribution.
}
\details{
  Routine \code{rvgt.range.engine} is the workhorse for
  performing tests encoded in function \code{test.routine}
  on the output of random variate generator \code{rdist}
  for a given set of parameter values. This set is provided
  by argument \code{dist.params} where each entry of this
  list corresponds to a parameter and holds the vector of
  possible values.

  Detailed description of the arguments of this functions
  are given in the sections below.
}
\section{Random variate generator}{
  The arguments of random variate generator \code{rdist}
  must be similar to that of the \R built-in random variate
  generators such as \code{\link{rnorm}}: the first
  argument must be the sample size \code{n} followed by the
  parameters of the distribution.

  The values for the parameters of the distribution must be
  given in argument \code{dist.params} as list. Each entry
  of this list corresponds to a paramter as used in the
  call to \code{rdist}. Each of these entries contains
  either a number when the value for that parameter is kept
  fixed or a vector of numbers. In the latter case
  \code{rvgt.range.engine} runs through all possible
  combinations of parameter values. (Parameters of the
  distribution that have defaults in \code{rdist} need not
  be listed in argument \code{dist.params}.)

  Function \code{rdist} may also have some additional
  arguments, e.g., debugging flags. This can be useful for
  testing experimental generators. Additional arguments can
  be provided via argument \code{r.params}. The entries of
  this list (if non-empty) are used as-is as arguments for
  \code{rdist}.

  Some tests require propoerties of the random variate
  generator that can only be collected during setup (like
  the rejection constant) or running the random variate
  generator \code{rdist} (like the rejection rate). These
  properties must be stored in the returned sample as
  attributes. In order to run these tests function
  \code{rdist} \itemize{ \item must except argument
  \code{show.properties}, \item must return its properties
  (like the rejection constant) as attributes when
  \code{show.properties=TRUE}. } See function
  \code{rvgt.range.trc} or \code{rvgt.range.orc} for
  examples.
}

\section{Test routine}{
  Routine \code{test.routine} actually performs the test on
  a single combination of parameter values. It has to
  accept the following arguments: \describe{
  \item{\code{rdist}}{% random number generator of
  distribution (function).} \item{\code{dist.params}}{%
  parameters for distribution (list).}
  \item{\code{r.params}}{% additional arguments for
  \code{rdist} (list).} \item{\code{emgt}}{% expected
  (approximate) marginal generation of \code{rdist} (in
  seconds).} \item{\code{test.params}}{% additional
  arguments for \code{test.routine} (list).}
  \item{\code{duration}}{% scheduled duration for test (in
  seconds).} \item{\code{verbose}}{% if TRUE show progress
  (logical).} } Thus the function prototype reads
  \code{test.routine(rdist, dist.params, r.params, emgt,
  test.params, duration, verbose)}.

  When \code{test.routine} is called then the corresponding
  arguments from \code{rvgt.range.engine} are just passed
  to this function. However, each entry in
  \code{dist.params} contains one of the possible values.
  Argument \code{emgt} contains the corresponding
  generation time as given by \code{gen.time}.

  Further arguments that are needed to run a test (e.g.,
  the CDF or quantile function) have to be passed via list
  \code{test.params}.

  A test should belong to some class which has to be given
  by argument \code{test.class}. It is used to create the
  class names of the object returned by
  \code{rvgt.range.engine}.
}

\section{Duration and timeout}{
  When testing a wide range of parameter settings running
  time is an issue. Each test must run sufficiently long in
  order to get a sensible result. On the other hand the
  total running time of the test suite must not exceed some
  constraints.

  Function \code{rvgt.range.engine} provides a simple
  mechanism to control running times. By argument
  \code{gen.time} one can provide the (approximate)
  marginal generation time for random variate generator
  \code{rdist}. By argument \code{duration} one can set the
  require running time for each of the tests. However, the
  two parameters are just passed to function
  \code{test.routine} and it is the task of this routine to
  use this information. Thus \code{duration} times the
  total number of tests (i.e., the number of possible
  tuples created from the given \code{dist.params}) can
  only be used for a (very!) rough estimate of the total
  running time.

  In order to prevent one of the tests from running too
  long (or from looping infinitely) argument \code{timeout}
  allows to set an upper limit for the running time of each
  test. Notice, however, that this feature requires
  multicore support and is thus available only on
  POSIX-compliant OSes (i.e., not on Windows OSes). It can
  be enabled by means of argument \code{ncores}.
}
\examples{
## Simple "test" that just samples one random number
create.sample <- function (rdist, dist.params, ...) {
   cl <- as.call(c(list(name=rdist,n=1),dist.params))
   eval(cl)
}

## Run this "test" on rnorm() for various values of 'mean' and 'sd'
samp <- rvgt.range.engine(rdist=rnorm,
                          dist.params=list(mean=0:10, sd=1:10),
                          test.routine=create.sample,
                          test.class="sample")

## print summary
print(samp)

## Plot sample
plot(samp)
}
\author{
  Josef Leydold \email{josef.leydold@wu.ac.at}.

  Thanks to Simon Urbanek for helpful explanations of the
  low level functions of the \pkg{parallel}/\pkg{multicore}
  package.
}
\seealso{
  \code{\link{print.rvgt.range}} for printing a summary of
  test results, \code{\link{plot.rvgt.range}} for plotting
  the test results.
}

