\name{rvgt.range.orc}
\alias{rvgt.range.orc}
\title{Test rejection constants for random variate generator}
\usage{
  rvgt.range.orc(rdist, dist.params, r.params = list(),
    duration = 0.1, gen.time, ncores = NULL, timeout = Inf,
    verbose = FALSE)
}
\arguments{
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
  \code{"rvgt.range.gof.orc"} where the p-values are stored
  in field \code{$data}, see
  \code{\link{rvgt.range.engine}} for a description of such
  objects. The routine returns \code{NA} in all cases where
  the setup fails, \code{NaN} when theoretical rejection
  constant is less than 1 (i.e., obviously invalid), and
  \code{Inf} when the marginal generation time is too slow
  or when a timeout has been reached.
}
\description{
  Simple goodness-of-fit test for a generation algorithm
  that is based on the acceptance-rejection method: Test
  hypothesis that observed rejection constant coincides
  with the theoretical rejection constant.
}
\details{
  This routine performs a test for the observed (empirical)
  rejection constant for the given random variate generator
  for each combinations of parameter values. It is a very
  simple but also fast test that compares the (theoretical)
  rejection constant with the observed one. It can be used
  as a first test as it allows to detect coding errors or
  severe numerical problems (or a design error of the
  algorithm).

  Both the theoretical rejection constant and the observed
  rejection constant must be provided by random variate
  generator \code{rdist} as respective attributes
  \code{"trc"} and \code{"orc"} when \code{rdist} is called
  with argument \code{show.properties=TRUE}.

  Of course this test is only sensible if \code{rdist} is
  based on the acceptance-rejection method.
}
\examples{
## a simple generator for the beta distribution
myrbeta <- function(n, shape1, shape2, show.properties=FALSE) {
   if (shape1 <= 1 || shape2 <= 1 || n < 0) stop("arguments invalid")
   mode <- (shape1 - 1) / (shape1 + shape2 - 2)
   fmode <- dbeta(mode,shape1,shape2)
   trials <- 0
   res <- numeric(n)
   for (i in 1:n) {
      while(n>0) {
         trials <- trials + 1
         X <- runif(1)
         Y <- fmode * runif(1)
         if (Y <= dbeta(X,shape1,shape2)) {
            res[n] <- X
            break
         }
      }
   }
   if (isTRUE(show.properties)) {
      trc <- fmode
      attr(res,"trc") <- trc
      attr(res,"orc") <- trials / n
   }
   res
}

## first we need the marginal generation times
mgt <- rvgt.range.marginal(rdist = myrbeta,
                           dist.params = list(shape1=c(2,3), shape2=2),
                           duration = 0.01)

## test rejection constants
orc <- rvgt.range.orc(rdist = myrbeta,
                      dist.params = list(shape1=c(2,3), shape2=2),
                      duration = 0.01, gen.time = mgt)

print(orc)
}
\author{
  Josef Leydold \email{josef.leydold@wu.ac.at}
}
\seealso{
  \code{\link{rvgt.range.engine}} for a description of
  objects of class \code{"rvgt.range"}.
  \code{\link{print.rvgt.range}} for printing a summary of
  test results, \code{\link{plot.rvgt.range}} for plotting
  the test results.
}

