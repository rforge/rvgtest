\name{get.subrange}
\alias{get.subrange}
\title{Extract object of class "rvgt.range" for a subset of parameter values
(internal function)}
\usage{
  get.subrange(obj, sub.params = list(), drop = TRUE)
}
\arguments{
  \item{obj}{object of class \code{"rvgt.range"}.}

  \item{sub.params}{list that contains the subset of
  parameter values for the distribution parameters or for
  additional parameters of the random variate generator.
  These can be either indices given as integers like
  \code{c(1L,3L)} or \code{2:5}, or values given by doubles
  like \code{c(1,3,5)}.  Alternatively, lower and upper
  bound can be given as a pair of numbers when the name of
  the parameter is postfixed by \code{.lim}.}

  \item{drop}{if TRUE then dimensions with 1 entry are
  removed (boolean).}
}
\value{
  An object of class \code{"rvgt.range"} that contains the
  subset of parameter values.
}
\description{
  Extract an object of class \code{"rvgt.range"} from a
  given one. The new object corresponds to a test with the
  given subset of parameters. -- (internal function)
}
\details{
  Function \code{get.subrange} extracts an object of class
  \code{"rvgt.range"} from a given one. The new object
  corresponds to a test with the given subset of
  parameters. This can be useful if one wants to
  \dQuote{zoom} into a region of interest, or if the number
  of dimensions have to be reduces (using \code{drop=TRUE})
  in order to visualize the results.

  The subset of parameters is given by argument
  \code{sub.params}. Each of its entry must correspond to a
  name in list \code{obj$dist.params} or list
  \code{obj$r.params}. The subset can be selected by either
  \itemize{ \item a list of indices given as a vector of
  type \code{"integer"}, or \item a list of values given as
  a vector of type \code{"double"}. } Alternatively, a name
  in argument \code{sub.params} can also be one of the
  names in \code{obj$dist.params} or \code{obj$r.params}
  with suffix \code{.lim} appended and the corresponding
  entries must be pairs of type \code{"double"} for the
  lower and upper bound, resp., for the parameter values.

  If a parameter from \code{obj$dist.params} or
  \code{obj$r.params} is omitted in \code{sub.params}, then
  all its values are included in the new object.
}
\note{
  Function \code{get.subrange} can be called recursively if
  \code{drop=FALSE}.
}
\examples{
## create an initial object with timing data
mgt <- rvgt.range.marginal(rdist = rbeta,
                           dist.params = list(
                             shape1=c(0.1,1,10),
                             shape2=c(0.1,1,10)),
                           duration = 0.01, gen.time = 1e-5)

## extract an object where shape1 is the second entry
rvgtest:::get.subrange(mgt, sub.params=list(shape1=2L))

## extract an object where shape1 is the first and third entry
## and shape2 is 0.1 and 1
rvgtest:::get.subrange(mgt, sub.params=list(shape1=c(1L,3L), shape2=c(0.1,1)))

## extract an object where shape2 is in closed interval 0.1, 1
rvgtest:::get.subrange(mgt, sub.params=list(shape2.lim=c(0.1,1)))
}
\author{
  Josef Leydold \email{josef.leydold@wu.ac.at}
}
\seealso{
  \code{\link{rvgt.range.engine}} for a description of
  class \code{"rvgt.range"}.
}

