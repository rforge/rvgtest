\name{xerror}
\Rdversion{1.1}
\alias{xerror}

\title{Create Table of X-Errors for Numerical Inversion Method}

\description{
  Function for creating a table of x-errors of a numerical inversion
  method (i.e., it uses an approximate quantile function of the target
  distribution). Thus the domain of the inverse distribution function is
  partitioned into intervals for which maxima, minima and some other
  quantiles of the x-errors are computed. 

  Currently the function only works for generators for continuous
  univariate distribution.
}

\usage{
xerror(n, aqdist, qdist, \dots, trunc=NULL, udomain=c(0,1),
       res=1000, kind=c("abs","rel"),tails=FALSE, plot=FALSE)
}

\arguments{
  \item{n}{sample size for one repetition.}
  \item{aqdist}{approximate inverse distribution function (quantile
    function) for a continuous univariate distribution.}
  \item{qdist}{(Exact) quatile function of distribution.}
  \item{\dots }{parameters to be passed to \code{qdist}.}
  \item{trunc}{boundaries of truncated domain. (optional)}
  \item{udomain}{domain of investigation for (approximate) quantile
    function \code{aqdist}.}
  \item{res}{resolution (number of intervals).}
  \item{kind}{kind of x-error.}
  \item{tails}{logical. If \code{TRUE}, then the tail regions are
    treated more accurately. However, this doubles the given sample size.}
  \item{plot}{logical. If \code{TRUE}, the (range of the) x-errors is
    plotted.}
}

\details{
  The absolute x-error of an approximate inverse distribution function
  (quantile function) \eqn{G^{-1}}{G^[-1]} for some
  \eqn{u\in (0,1)}{u in (0,1)} is given by

  \deqn{\epsilon_x(u) = |F^{-1}(u) - G^{-1}(u)|}{e_x(u) = |F^[-1](u) - G^[-1](u)|}

  where \eqn{F^{-1}}{F^[-1]} denotes the (exact) quantile function of
  the distribution.
  The relative x-error is then defined as

  \deqn{\epsilon_x(u) / |F^{-1}(u)|}{e_x(u) / |F^[-1](u)|}

  Computing, plotting and analyzing of such x-errors can be quite time
  consuming.
  \deqn{\epsilon_x(u)}{e_x(u)} is a very volatile function and requires
  the computation at a lot of points. For plotting we can condense the
  information by partitioning (0,1) into intervals of equal 
  length. In each of these the x-error is computed at equidistributed
  points and some quantiles (see below) are estimated and stored.
  Thus we save memory and it is much faster to plot and compare x-errors
  for different methods or distributions.

  If \code{trunc} is given, then function \code{qdist} is rescaled to
  this given domain. Notice, however, that this has some influence on
  the accuracy of the results of the \dQuote{exact} quantile function
  \code{qdist}.
  
  Using argument \code{udomain} it is possible to restrict the domain of
  the given (approximate) quantile function \code{aqdist}, i.e., of its
  argument \eqn{u}.
  
  When \code{tails=TRUE} we use additional \code{n} points for the first
  and last interval (which correspond to the tail regions of the
  distribution).
}

\value{
  An object of class \code{"rvgt.ierror"}, see
  \code{\link{uerror}} for details.
}

\note{
  It should be noted that \code{xerror} computes the difference
  between the approximate inversion function \code{aqdist(u)} and the
  given \sQuote{exact} quantile function \code{qdist}.
  Thus one needs a quantile function \code{qdist} that is
  numerically (much) more accurate than \code{aqdist}.

  The random variate generator \code{rdist} can alternatively be a
  generator object form the
  \pkg{\link[Runuran:Runuran-package]{Runuran}} package.
}

%% \references{ }

\author{
  Josef Leydold \email{josef.leydold@wu.ac.at}
}

\seealso{
  See \code{\link{plot.rvgt.ierror}} for the syntax of the plotting
  method. See \code{\link{uerror}} for computing u-errors.
}

\examples{
## Create a table of absolute x-errors for spline interpolation of
## the inverse CDF of the standard normal distribution.
aq <- splinefun(x=pnorm((-100:100)*0.05), y=(-100:100)*0.05,
                method="monoH.FC")
## Use a sample of size of 10^5 random variates.
xerr <- xerror(n=1e4, aqdist=aq, qdist=qnorm, kind="abs")

## Plot x-errors
plot(xerr)


## Same for the relative error.
## But this time we use a resolution of 500, and
## we immediately plot the error.
xerr <- xerror(n=1e4, aqdist=aq, qdist=qnorm,
               res=500, kind="rel", plot=TRUE)


## An inverse CDF for a truncated normal distribution
aqtn <- splinefun(x=(pnorm((0:100)*0.015) - pnorm(0))/(pnorm(1.5)-pnorm(0)),
                  y=(0:100)*0.015, method="monoH.FC")
xerrtn <- xerror(n=1e4, aqdist=aqtn, qdist=qnorm, trunc=c(0,1.5),
                 plot=TRUE)
}

\keyword{distribution}
\keyword{datagen}
