\name{Perturbation}
\Rdversion{1.1}
\alias{pertadd}
\alias{pertsub}

\title{Generate Random Variates from a Perturbed RVG}

\description{
  These functions perturb the distribution of a given random variate
  generator. Thus it allows to investigate the power of tests
  for random variate generators.
}

\usage{
pertadd(n, rdist = rnorm, \dots, min = 0, max = 1, p = 0.001)
pertsub(n, rdist = rnorm, \dots, min = 0, max = 1, p = 0.001)
}

\arguments{
  \item{n}{sample size.}
  \item{rdist}{given RVG to be perturbed.}
  \item{\dots}{parameters for given random variate generator.}
  \item{min, max}{left and right boundary of perturbed domain.}
  \item{p}{strength of perturbation.}
}

\details{
  \code{pertadd} generates random variates from a mixture of
  \code{rdist} and a uniform distribution on the interval
  (\code{min},\code{max}). The uniform distribution is chosen with
  probability \code{p}.

  \code{pertsub} generates random variates from the \code{rdist} but
  rejects all points in the interval (\code{min},\code{max}) with
  probability \code{p}.
  
  By varying the width of uniform distribution (\code{min},\code{max})
  and probability of error \code{p}, different levels of \dQuote{artificial}
  error can be introduced.
  Thus it allows to investigate the power of tests
  for random variate generators.
}

\value{
  A vector of random variates from the perturbed distribution is
  returned.
}

\author{
  Sougata Chaudhuri \email{sgtchaudhuri@gmail.com},
  Josef Leydold \email{josef.leydold@wu.ac.at}
}

\examples{
## Generating random sample with default settings
x <- pertadd(n=1000)
y <- pertsub(n=1000)

## Generating random sample, with parent distribution as exponential.
x <- pertadd(n=1000, rdist=rexp, rate=2, min=2, max=3, p=0.005)
y <- pertsub(n=1000, rdist=rexp, rate=2, min=2, max=3, p=0.005)

## Conducting chi-square test on random variates generated from function
## pertsub with parent distribution as exponential.
ft <- rvgt.ftable(n=1e4,rep=10,
                  rdist=function(n){pertsub(n,rdist=rexp,p=0.1)},
                  qdist=qexp,breaks=1001)
plot(ft)
rvgt.chisq(ft)

## Conducting chi-square test on random variates generated from function
## pertadd with parent distribution as Weibull,shape=1,scale=2.
ft <- rvgt.ftable(n=1e4,rep=10,
                  rdist=function(n,...){pertadd(n,rdist=rweibull,...,p=0.05)},
                  qdist=qweibull,shape=1,scale=2)
plot(ft)
rvgt.chisq(ft)
}

\keyword{distribution}
\keyword{datagen}
