\name{plot.rvgt.range}
\alias{plot.rvgt.range}
\title{Plot result of range tests}
\usage{
  \method{plot}{rvgt.range} (x, sub.params = list(),
    xscale = c("linear", "logarithmic"),
    yscale = c("linear", "logarithmic"),
    zscale = c("auto", "linear", "logarithmic"), ...)
}
\arguments{
  \item{x}{object of class \code{"rvgt.range"} to be
  plotted.}

  \item{xscale}{type of scale used for first coordinate.}

  \item{yscale}{type of scale used for second coordinate.}

  \item{zscale}{type of scale used for third coordinate.}

  \item{...}{graphical parameters (see \code{\link{plot}}
  and \code{\link{par}}).}

  \item{sub.params}{list that contains the subset of
  parameter values for the distribution parameters or for
  additional parameters of the random variate generator.
  These can be either indices given as integers like
  \code{c(1L,3L)} or \code{2:5}, or values given by doubles
  like \code{c(1,3,5)}.  Alternatively, lower and upper
  bound can be given as a pair of numbers when the name of
  the parameter is postfixed by \code{.lim}.}
}
\description{
  \code{plot} method for class \code{"rvgt.range"}.
}
\details{
  Method \code{plot.rvgt.range} allows to visualize the
  output of routine \code{rvgt.range.engine}. The plot
  depends on the number of parameters where a range of
  values if given. If there is one such parameter then the
  output of the test routine are plotted against this
  single parameter. If two such parameters are given then
  the output is presented by means of an image plot. Three
  or more of such parameters are not supported.

  Argument \code{sub.params} allows to restrict the given
  parameter values before plotting. This allows to
  \dQuote{zoom} into a region of interest. It also allows
  to restrict a vector of parameter values to one of its
  entries. Thus one can reduce the number of dimensions for
  the plot.

  The entries of \code{sub.params} correspond to the
  parameters of the distribution as given in argument
  \code{x}. Each member of this list is either \itemize{
  \item a list of indices given as a vector of type
  \code{"integer"}, or \item a list of values given as a
  vector of type \code{"double"}. } Alternatively, a name
  in argument \code{sub.params} can also be one of the
  names in \code{x$dist.params} or \code{x$r.params} with
  suffix \code{.lim} appended and the corresponding entries
  must be pairs of type \code{"double"} for the lower and
  upper bound, resp., for the parameter values.

  Arguments \code{xscale}, \code{yscale}, and \code{zscale}
  allow to select a particular scaling (\code{"linear"} or
  \code{"logarithmic"}) for the given data. Notice, the
  z-axis always corresponds to the data. So for an
  1-dimensional plot we only have an x-axis and an z-axis
  and the value of \code{yscale} is ignored.
}
\examples{
## Simple "test" that just samples one random number
create.sample <- function (rdist, dist.params, ...) {
   cl <- as.call(c(list(name=rdist,n=1),dist.params))
   eval(cl)
}
samp <- rvgt.range.engine(rdist=rnorm,
                          dist.params=list(mean=0:10, sd=1:10),
                          test.routine=create.sample,
                          test.class="sample")

## Plot all samples
plot(samp)

## Plot all samples, use logarithmic scale for parameter 'sd'
plot(samp, yscale="logarithmic")

## Plot samples where 'sd' is the 5th entry (of 1:10).
## It is important that index 5 is given as integer 5L.
plot(samp, sub.params=list(sd=5L))

## Plot samples where 'sd' is restricted to the 5th, 6th and 7th entry.
## Notice that 5:7 creates a vector of integers.
plot(samp, sub.params=list(sd=5:7))

## Plot samples where 'mean' is between 7 and 9.
## This time we have to give a pair of numeric values which represent
## the lower and upper bound, resp.
## Notice that exact comparisons of floating point numbers can sometimes
## have surprising results. So it is recommended to add some tolerance.
plot(samp, sub.params=list(mean.lim=c(6.99,9.01)))

## Plot samples where 'mean' equals 7
## (again we have to use a pair of numeric values).
plot(samp, sub.params=list(mean=7))

## Zoom into the region with smalls values for 'mean' and 'sd'.
plot(samp, sub.params=list(mean.lim=c(0,5.01), sd.lim=c(0,5.01)))
}
\author{
  Josef Leydold \email{josef.leydold@wu.ac.at}
}
\seealso{
  \code{\link{rvgt.range.engine}} for a description of
  objects of class \code{"rvgt.range"}.
}

