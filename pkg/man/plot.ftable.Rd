\name{plot.rvgt.ftable}
\Rdversion{1.1}
\alias{plot.rvgt.ftable}

\title{Plot RVG Frequency Table}

\description{
  Method for plotting the density of an RVG frequency table.
}

\usage{
\S3method{plot}{rvgt.ftable}(x, rows, alpha = 0.01, \dots)
}

\arguments{
  \item{x}{object of class \code{"rvgt.ftable"}.}
  \item{rows}{integer or array of integers that indicate the rows of the
    table for which the density is plotted. If missing all rows are used.}
  \item{alpha}{significance level for plotting critical values.}
  \item{\dots}{further graphical parameters.}
}

\details{
  \code{plot} creates a density plot for the given RVG frequency table.
  The display zooms into the union of the range of frequency values and
  2 times the confidence intervals for the frequencies.
  Thus it visualizes significant deviations from the unifrom
  distributions. The critical values for significance level \code{alpha}
  are marked by a red (dashed) line.

  \code{rows} is either a number or a vector of numbers that indicate
  the rows of RVG tabel \code{x} which are merged (cumulated) for the
  plot. If this argument is missing, all rows are merged.
}

\author{
  Sougata Chaudhuri \email{sgtchaudhuri@gmail.com},
  Josef Leydold \email{josef.leydold@wu.ac.at}
}

\seealso{
  \code{\link{rvgt.ftable}} for creating the frequency table;
  \code{\link{hist}} for the height of the drawn rectangle.
}

\examples{
## Create a frequency table for normal distribution and show histogram.
## Use a sample of size of 5 times 10^5 random variates.
ft <- rvgt.ftable(n=1e5,rep=5, rdist=rnorm,qdist=qnorm, mean=1,sd=2)

## Plot histogram
plot(ft)

## Plot histogram for row 1 only
plot(ft,rows=1)

## Plot histogram for rows 2 and 4 only
plot(ft,rows=c(2,4))

## Same with the buggy random variate generator
## (try to increase sample size 'n' or 'rep')
RNGkind(normal.kind="Buggy Kinderman-Ramage")
ft <- rvgt.ftable(n=1e5,rep=1, rdist=rnorm,qdist=qnorm)
plot(ft)

}

\keyword{distribution}
\keyword{hplot}
