\name{uerror}
\Rdversion{1.1}
\alias{uerror}
\alias{uerror.default}

\title{Create Table of U-Errors for Numerical Inversion Method}

\description{
  Function for creating a table of u-errors of a numerical inversion
  method (i.e., it uses an approximate quantile function of the target
  distribution). Thus the domain of the inverse distribution function is
  partitioned into intervals for which maxima, minima and some other
  quantiles of the u-errors are computed. 

  Currently the function only works for generators for continuous
  univariate distribution.
}

\usage{
uerror(n, aqdist, pdist, \dots, trunc=NULL, udomain=c(0,1),
       res=1000, tails=FALSE, plot=FALSE)
}

\arguments{
  \item{n}{sample size for one repetition.}
  \item{aqdist}{approximate inverse distribution function (quantile
    function) for a continuous univariate distribution.}
  \item{pdist}{cumulative distribution function for distribution.}
  \item{\dots }{parameters to be passed to \code{pdist}.}
  \item{trunc}{boundaries of truncated domain. (optional)}
  \item{udomain}{domain of investigation for (approximate) quantile
    function \code{aqdist}.}
  \item{res}{resolution (number of intervals).}
  \item{tails}{logical. If \code{TRUE}, then the tail regions are
  treated more accurately. However, this doubles the given sample size.}
  \item{plot}{logical. If \code{TRUE}, the (range of the) u-errors is
    plotted.}
}

\details{
  The u-error of an approximate inverse distribution function (quantile
  function) \eqn{G^{-1}}{G^[-1]} for some \eqn{u\in (0,1)}{u in (0,1)}
  is given by

  \deqn{\epsilon_u(u) = |u - F(G^{-1}(u))|}{e_u(u) = |u - F(G^[-1](u))|}

  where \eqn{F} denotes the (exact) cumulative distribution.
  It is a convenient measure for approximation errors in non-uniform
  random variate generators bases on numerical inversion, see the
  reference below for our arguments.

  Computing, plotting and analyzing of such u-errors can be quite time
  consuming.
  \deqn{\epsilon_u(u)}{e_u(u)} is a very volatile function and requires
  the computation at a lot of points. For plotting we can condense the
  information by partitioning (0,1) into intervals of equal 
  length. In each of these the u-error is computed at equidistributed
  points and some quantiles (see below) are estimated and stored.
  Thus we save memory and it is much faster to plot and compare u-errors
  for different methods or distributions.

  If \code{trunc} is given, then function \code{pdist} is rescaled to
  this given domain. Notice, however, that this has some influence on
  the accuracy of the results of the distribution function \code{pdist}.
  
  Using argument \code{udomain} it is possible to restrict the domain of
  the given (approximate) quantile function \code{aqdist}, i.e., of its
  argument \eqn{u}.
  
  When \code{tails=TRUE} we use additional \code{n} points for the first
  and last interval (which correspond to the tail regions of the
  distribution).
}

\value{
  An object of class \code{"rvgt.ierror"} which is a list with components:

  \item{n}{sample size.}
  \item{res}{resolution (number of intervals).}
  \item{kind}{kind of error (string).}
  \item{udomain}{domain for u.}
  \item{min}{an array of minimum u-errors (of length \code{res}).}
  \item{lqr}{an array of lower quartile of u-errors (of length \code{res}).}
  \item{med}{an array of median u-errors (of length \code{res}).}
  \item{uqr}{an array of upper quartile of u-errors (of length \code{res}).}
  \item{max}{an array of maximum u-errors (of length \code{res}).}
  \item{mad}{an array of mean absolute deviations (of length \code{res}).}
  \item{mse}{an array of mean squared errors (of length \code{res}).}
}

\note{
  It should be noted that \code{uerror} computes the numerical
  error of the \emph{composed} function \code{pdist(aqdist(u))}.
  Thus one needs a distribution function \code{pdist} that is
  numerically (much) more accurate than \code{aqdist}.
}

\references{
  G. Derflinger, W. H\"ormann, and J. Leydold (2009):
  ACM Trans. Model. Comput. Simul., to appear.
}

\author{
  Josef Leydold \email{josef.leydold@wu.ac.at}
}

\seealso{
  See \code{\link{plot.rvgt.ierror}} for the syntax of the plotting
  method. See \code{\link{xerror}} for computing x-errors.
}

\examples{
## Create a table of u-errors for spline interpolation of
## the inverse CDF of the standard normal distribution.
aqn <- splinefun(x=pnorm((-100:100)*0.05), y=(-100:100)*0.05,
                 method="monoH.FC")
## Use a sample of size of 10^5 random variates.
uerrn <- uerror(n=1e5, aqdist=aqn, pdist=pnorm)

## Plot u-errors
plot(uerrn)

## Investigate tails more accurately, and use
## a resolution of 1000 intervals.
uerrn <- uerror(n=1e5, aqdist=aqn, pdist=pnorm, res=1000, tails=TRUE)


## Same for a gamma distribution.
## But this time we immediately plot the error.
aqg <- splinefun(x=pgamma((0:500)*0.1,shape=5),
                 y=(0:500)*0.1, method="monoH.FC")
uerrg <- uerror(n=1e5, aqdist=aqg, pdist=pgamma, shape=5,
                plot=TRUE)


## Compute u-error for a subdomain of a beta distribution
aqb <- splinefun(x=pbeta((0:100)*0.01,shape1=2,shape2=5),
                 y=(0:100)*0.01, method="monoH.FC")
uerrb <- uerror(n=1e5, aqdist=aqb, pdist=pbeta, shape1=2, shape2=5,
                udomain=c(0.6,0.65), plot=TRUE)

## Show all u-errors in one plot
plot.rvgt.ierror(list(uerrn,uerrg,uerrb))

## An inverse CDF for a truncated normal distribution
aqtn <- splinefun(x=(pnorm((0:100)*0.015) - pnorm(0))/(pnorm(1.5)-pnorm(0)),
                  y=(0:100)*0.015, method="monoH.FC")
uerrtn <- uerror(n=1e5, aqdist=aqtn, pdist=pnorm, trunc=c(0,1.5),
                 plot=TRUE)
}

\keyword{distribution}
\keyword{datagen}
