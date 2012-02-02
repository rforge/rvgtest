\name{plot.rvgt.htest}
\Rdversion{1.1}
\alias{plot.rvgt.htest}

\title{Plot p-Values against Sample Size}

\description{
  Method for plotting the p-values of test results on RVG frequency
  tables.
}

\usage{
\S3method{plot}{rvgt.htest}(x, alpha = 0.001, \dots)
}

\arguments{
  \item{x}{an object of class \code{"rvgt.htest"} or a list of such
    objects.}
  \item{alpha}{significance level.}
  \item{\dots}{further graphical parameters.}
}

\details{
  Plot common logarithm of p-values stored in an object of class
  \code{"rvgt.htest"} (i.e., p-values of tests on RVG frequency tables).
  The p-values are plotted against increasing sample size.
  The significance level \code{alpha} is marked by a
  red dashed line.
  This allows for visible inspection of test results.

  Argument \code{x} must be an object of class
  \code{"rvgt.htest"} that contains information about the test and
  p-values. Such an object is created by \code{\link{rvgt.chisq}} or
  \code{\link{rvgt.Mtest}}.

  Alternatively, when the method is called by its full name,
  \code{plot.rvgt.htest}, then the first argument \code{x} may also
  be a list of such objects.
  Then p-values for multiple experiments will be plotted in the same
  graph with different colors. Thus one can compare the power of
  different tests or the results for different generation methods.
}

\author{
  Sougata Chaudhuri \email{sgtchaudhuri@gmail.com},
  Josef Leydold \email{josef.leydold@wu.ac.at}
}

\seealso{
  \code{\link{rvgt.chisq}}, \code{\link{rvgt.Mtest}}.
}

\examples{
## Create a frequency table for normal distribution and show histogram.
## Use a sample of size of 5 times 10^5 random variates.
table <- rvgt.ftable(n=1e5,rep=5, rdist=rnorm,qdist=qnorm, mean=1,sd=2)

## Perform a chi-square goodness-of-fit test and plot result
r1 <- rvgt.chisq(table)
plot(r1)

## Perform M-test
r2 <- rvgt.Mtest(table)
plot(r2)

## Create a plot that contains the results of both tests
plot.rvgt.htest(list(r1,r2))
}

\keyword{distribution}
\keyword{hplot}
\keyword{htest}
