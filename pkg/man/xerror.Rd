\name{xerror}
\Rdversion{1.1}
\alias{xerror}
\alias{xerror.default}

\title{Create Table of X-Errors for Numerical Inversion Method}

\description{
  Function for creating a table of x-errors of a numerical inversion
  method (i.e., it uses an approximate quantile function of the target
  distribution). Thus the domain of the inverse distribution function is
  partitioned into intervals for which maxima, minima and some other
  quantiles of the x-errors are computed. 
}

\usage{
xerror(n, aqdist, qdist, \dots, udomain=c(0,1),
       res=1000, kind=c("abs","rel"),tails=FALSE, plot=FALSE)
}

\arguments{
  \item{n}{sample size for one repetition.}
  \item{aqdist}{approximate inverse distribution function (quantile
    function) for the distribution.}
  \item{qdist}{(Exact) quatile function of distribution.}
  \item{\dots }{parameters to be passed to \code{pdist}.}
  \item{udomain}{domain for u.}
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

  Using argument \code{udomain} it is possible to restrict the domain of
  \eqn{u}.
  
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
## Create a table of absolute x-errors for a generator for the normal
## distribution using a quantile function with errors.
aq <- function(u) { qnorm(u) + runif(1,max=1.e-10) }
## Use a sample of size of 10^5 random variates.
xerr <- xerror(n=1e5, aqdist=aq, qdist=qnorm, kind="abs")

## Plot x-errors
plot(xerr)

## Same for the relative error.
## But this time we use a resolution of 500, and
## we immediately plot the error.
aq <- function(u) { qnorm(u) * runif(1,min=1,max=1+1.e-10) }
xerr <- xerror(n=1e5, aqdist=aq, qdist=qnorm,
               res=500, kind="rel", plot=TRUE)

}

\keyword{distribution}
\keyword{datagen}
