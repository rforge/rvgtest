\name{rvgtest-package}
\Rdversion{1.1}
\docType{package}

\alias{rvgtest-package}
\alias{rvgtest}

\title{Tools for Analyzing Non-Uniform Pseudo-Random Variate Generators (RVG)}

\description{
  Suite for testing non-uniform random number generators.
}

\details{
  \tabular{ll}{
    Package: \tab rvgtest\cr
    Type:    \tab Package\cr
    Version: \tab 0.5.901       \cr
    Date:    \tab 2011-02-02    \cr
    License: \tab GPL 2 or later\cr
    LazyLoad:\tab yes\cr
  }

  \pkg{rvgtest} is a set of tools to investigate the quality of
  non-uniform pseudo-random random generators (RVG).
  Thus it provides functions to visualize and test for possible
  defects. There are three mean reasons for such defects and errors:
  \enumerate{
    \item Errors in the design of algorithms -- The proof for theorem
    that claims the correctness of the algorithm is wrong.
    \item Implementation errors -- Mistakes in computer programs.
    \item Limitations of floating point arithmetic and round-off
    errors in implementations of these algorithms in real world
    computers.
  }
  Of course testing software is a self-evident part of software
  engineering. Implementation errors usually result in large deviations
  from the requested distribution and thus errors of type 2 are easily
  detected. However, this need not always be the case, for example for
  rather complicated algorithms like those based on patchwork methods.

  The same holds for errors of type 1. In the best of all worlds, there
  exists a correct proof for the validity of the algorithm. In our world
  however human can err. Then the deviations are rather small, since
  otherwise it would have been detected when testing the implementation
  for errors of type 2.

  Errors of type 3 can be a problem when the requested distribution
  has extreme properties. E.g., it is no problem to generate a sample of
  beta distributed random variates with shape parameters 0.001 using
  \code{rbeta(n=100, shape1=0.001, shape2=0.001)}.
  However, due the limited resolution of floating point numbers it behaves
  like a discrete distribution (especially near 1). It is not always
  obvious whether such round-off errors will influence ones simulation
  results.

  It is the purpose of this package to provide some tools to find
  possible errors in RVGs. However, observing a defect in (the
  implementation of) a pseudo-random variate generator by purely
  statistical tools may require a large sample size which that exceeds
  the memory when hold in a single array in \R. 
  (Nevertheless, there is some chance that this defect causes an error
  in a particular simulation with a moderate sample size.)
  Hence we have implemented routines that can run tests on very large
  sample sizes (which are only limited by the available runtimes).

  Currently there are two toolsets for testing random variate generators
  for \emph{continuous univariate distributions}:
  \enumerate{
    \item Testing based on histograms for all kinds of RVGs.
    \item Estimating errors of RVGs that are based on numerical
    inversion methods.
  }  
}

\section{1. Histograms}{
  A frequently used method for testing continuous univariate
  distributions is based on the following strategy:
  Draw a sample, compute a histogram and run a goodness-of-fit test on
  the resulting frequency table.

  We have implemented a three step procedure:
  \enumerate{
    \item Create tables that can hold the information of huge random samples.
    \item Perform some test for the null hypothesis on these tables.
    \item Visualize these tables as well the results of the tests.
  }

  The advantages of this procedure are:
  \itemize{
    \item Huge total sample sizes are possible (only limited by
    available runtime but not by memory).
    \item Can run multiple tests on the same random sample.
    \item Inspect data visually.
  }
  
  In addition there are also some random functions for introducing
  defects in other random variate generators artificially.
  Thus one may investigate the power of tests.

  \subsection{List of Routines}{
    Data generation: \code{\link{rvgt.ftable}}.
    
    Tests: \code{\link{rvgt.chisq}}, \code{\link{rvgt.Mtest}}.
    
    Visualization: \code{plot}
    (see \code{\link{plot.rvgt.ftable}}, \code{\link{plot.rvgt.htest}} for
    the respective syntax of the call).
    
    Perturbation of RVGs: \code{\link{pertadd}},
    \code{\link{pertsub}}.
  }
}

\section{2. Numerical Inversion}{
  Random variate generators that are based on inverting the distribution
  function preserve the structure of the underlying uniform random
  number generator. Given the fact that state-of-the-art uniform random
  number generators are well tested, it is sufficient to estimate
  (maximal) approximation errors.

  Let \eqn{G^{-1}}{G^[-1]} denote the approximate inverse distribution
  function (quantile function) and \eqn{F} the (exact) cumulative
  distribution. Then the following measures for the approximation erros
  are implemented:
  \describe{
    \item{u-error:}{
      \eqn{\epsilon_u(u) = |u - F(G^{-1}(u))|}{e_u(u) = |u - F(G^[-1](u))|}
    }
    \item{absolute x-error:}{
      \eqn{\epsilon_x(u) = |F^{-1}(u) - G^{-1}(u)|}{e_x(u) = |F^[-1](u) - G^[-1](u)|}
    }
    \item{relative x-error:}{
      \eqn{\epsilon_x(u) / |F^{-1}(u)|}{e_x(u) / |F^[-1](u)|}
    }
  }  

  We are convinced that the u-error is the most convenient measure for
  the approximation error in the framework of Monte Carlo simulation.
  E.g., goodness-of-fit tests like the chi-square test or the
  Kolmogorov-Smirnov test look at this deviation.

  As for the histogram based tests we have implemented in such a way
  that sample sizes are not limited by memory. Again data generation and
  visualization is separated into to routines.
  
  \subsection{List of Routines}{
    Data generation: \code{\link{uerror}}, \code{\link{xerror}}.
    
    Visualization: \code{plot}
    (see \code{\link{plot.rvgt.ierror}} for the syntax of the
    call).
  }
}

\author{
  Josef Leydold \email{josef.leydold@wu.ac.at},
  Sougata Chaudhuri \email{sgtchaudhuri@gmail.com}
}

%%\references{}

\examples{
## ------------------------------------------------
## 1. Histogram
## ------------------------------------------------

## Use a poor Gaussian random variate generator
## (otherwise we should not see a defect).
RNGkind(normal.kind="Buggy Kinderman-Ramage")

## Create table of bin counts.
## Use a sample of 20 times 10^5 random variates.
table <- rvgt.ftable(n=1e5, rep=20, rdist=rnorm, qdist=qnorm)

## Plot histogram for (cumulated) data
plot(table)

## Perform a chi-square goodness-of-fit test and plot result
r1 <- rvgt.chisq(table)
plot(r1)

## Perform M-test and plot both results
r2 <- rvgt.Mtest(table)
plot.rvgt.htest(list(r1,r2))

## ------------------------------------------------
## 2. Numerical Inversion
## ------------------------------------------------

## Create a table of u-errors for spline interpolation of
## the inverse CDF of the standard normal distribution.
aqn <- splinefun(x=pnorm((-100:100)*0.05), y=(-100:100)*0.05,
                 method="monoH.FC")
## Use a sample of size of 10^5 random variates.
uerrn <- uerror(n=1e5, aqdist=aqn, pdist=pnorm)

## Plot u-errors
plot(uerrn)

}

\keyword{package}
\keyword{distribution}
\keyword{datagen}
\keyword{htest}
\keyword{hplot}

