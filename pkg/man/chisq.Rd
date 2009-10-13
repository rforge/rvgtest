\name{rvgt.chisq}
\Rdversion{1.1}
\alias{rvgt.chisq}

\title{Perform Chi-Square goodness-of-fit Test on RVG Frequency Table}

\description{
  Perform achi-square goodness-of-fit test on the RVG frequency table.
}

\usage{
rvgt.chisq(ftable)
}

\arguments{
  \item{ftable}{object of class \code{"rvgt.ftable"}.}
}

\details{
  \code{rvgt.chisq} performs aa chi-square goodness-of-fit test
  on the bin counts of the RVG frequency tables created by means of
  \code{\link{rvgt.ftable}}.
  The null hypothesis is that the bin counts are uniformly distributed.

  If the \code{ftable} contains the bin counts for more than one
  repetitions then the test is repeated with column sums of increasing
  number of rows which corresponds to increasing sampling sizes.
  This allows for getting an idea of the power of the test.
}

\value{
  An object of class \code{"rvgt.htest"} which is a list with components:

  \item{type}{character string containing type of test;
    equals \code{"chisqu"}.}
  \item{n}{sample size for every row in \code{ftable}.}
  \item{rep}{number of rows in \code{ftable}.}
  \item{breaks}{number of break points in \code{ftable}
    (i.e., number of bins + 1).}
  \item{pval}{vector of p-values.}
}

\author{
  Sougata Chaudhuri \email{sgtchaudhuri@gmail.com},
  Josef Leydold \email{josef.leydold@wu.ac.at}
}

\seealso{
  See \code{\link{plot.rvgt.htest}} for the syntax of the plotting
  method.
}

\examples{
## Create a frequency table for normal distribution and
## conduct a chi-square goodness-of-fit test on this data.
## Use a sample of size of 5 times 10^5 random variates.
table <- rvgt.ftable(n=1e5,rep=5, rdist=rnorm,qdist=qnorm, mean=1,sd=2)

## Perform test
result <- rvgt.chisq(table)

## Plot result
plot(result)
}

\keyword{distribution}
\keyword{htest}
