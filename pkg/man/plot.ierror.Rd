\name{plot.rvgt.ierror}
\Rdversion{1.1}
\alias{plot.rvgt.ierror}

\title{Plot Errors in Inversion Methods}

\description{
  Method for plotting errors in numerical inversion methods.
}

\usage{
\S3method{plot}{rvgt.ierror}(x, maxonly=FALSE, tol, \dots)

plot.rvgt.ierror(x, maxonly=FALSE, tol, \dots)
}

\arguments{
  \item{x}{an object of class \code{"rvgt.ierror"} or a list of such
    objects.}
  \item{maxonly}{logical. If \code{TRUE}, only show maximal errors.}
  \item{tol}{maximal tolerated error (optional).}
  \item{\dots}{further graphical parameters.}
}

\details{
  Plot errors stored in an object of class \code{"rvgt.ierror"}.
  The function plots range, interquartile range, median, and maximum.
  If \code{maxonly} is \code{TRUE}, then only the maximal errors are
  plotted.

  If present, the maximal tolerated u-error \code{tol} is
  marked by a blue dashed line. Furthermore, \code{tol} also can be used
  to control the plotting range for the error. The plotting range
  depends on the maximal observed error. If \code{tol} is given, then
  the range is between \code{1.1*tol} and \code{10*tol}.

  The argument \code{x} must be an object of class \code{"rvgt.ierror"}
  that contains information about u-errors.
  Such an object is created by \code{\link{uerror}} or
  \code{\link{xerror}}.
  
  Alternatively, when using the function \code{plot.rvgt.ierror} then
  argument \code{x} also can be a list of such objects.
  Then maximual errors for multiple experiments will be plotted in the
  same graph with different colors (that is, with \code{maxonly=TRUE}).
  Thus one can compare errors of different inverse distribution functions.
}

\author{
  Josef Leydold \email{josef.leydold@wu.ac.at}
}

\seealso{
  \code{\link{uerror}}, \code{\link{xerror}}.
}

\examples{
## Create a table of u-errors for a generator for the normal
## and a gamma distribution using the R built-in quantile functions.
aqn <- function(u) { qnorm(u) }
uerrn <- uerror(n=1e5, res=1000, aqdist=aqn, pdist=pnorm)
aqg <- function(u) { qgamma(u,shape=2) }
uerrg <- uerror(n=1e5, res=1000, aqdist=aqg, pdist=pgamma, shape=2)

## Plot u-errors of the normal distribution
plot(uerrn)

## Plot maximal u-errors of the gamma distribution
plot(uerrn,maxonly=TRUE)

## Compare the u-errors of these two distributions and
## draw maximal tolerated error
plot.rvgt.ierror(list(uerrn,uerrg),tol=4.e-16)

}

\keyword{distribution}
\keyword{hplot}
\keyword{htest}
