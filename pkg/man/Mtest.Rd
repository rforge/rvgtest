\name{rvgt.Mtest}
\Rdversion{1.1}
\alias{rvgt.Mtest}

\title{Perform M-Test on RVG Frequency Table}

\description{
  Perform an adjusted residual test (\dQuote{M-Test}) on the RVG
  frequency table.
}

\usage{
rvgt.Mtest(ftable)
}

\arguments{
  \item{ftable}{object of class \code{"rvgt.ftable"}.}
}

\details{
  \code{rvgt.Mtest} performs an adjusted residual test (\dQuote{M-Test})
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
    equals \code{"M-test"}.}
  \item{n}{sample size for every row in \code{ftable}.}
  \item{rep}{number of rows in \code{ftable}.}
  \item{breaks}{number of break points in \code{ftable}
    (i.e., number of bins + 1).}
  \item{pval}{vector of p-values.}
}

\references{
  C. Fuchs and R. Kenett.
  \emph{A test for Detecting Outlying Cells in the Multinomial
    Distribution and Two-Way Contingency Tables}.
  Journal of American Statistical Association, Vol 75, June 1980,
  395--398.
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
## conduct an M-test on this data.
## Use a sample of size of 5 times 10^5 random variates.
table <- rvgt.ftable(n=1e5,rep=5, rdist=rnorm,qdist=qnorm, mean=1,sd=2)

## Perform test
result <- rvgt.Mtest(table)

## Plot result
plot(result)
}

\keyword{distribution}
\keyword{htest}
