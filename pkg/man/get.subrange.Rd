\name{get.subrange}
\alias{get.subrange}
\title{Get subset of distribution parameters in object of class
"rvgt.range" (internal function)}
\usage{
  get.subrange(obj, sub.params = list(), drop = TRUE)
}
\arguments{
  \item{obj}{object of class \code{"rvgt.range"}.}

  \item{sub.params}{subset of parameter values (list).}

  \item{drop}{if TRUE then dimensions with 1 entry are
  removed (boolean).}
}
\value{
  An object of class \code{"rvgt.range"} that contains the
  subset of parameter values.
}
\description{
  Get subset of distribution parameters in object of class
  \code{"rvgt.range"}. -- (internal function)
}
\details{
  An object of class \code{"rvgt.range"} contains the
  results of some test that has been performed on a set of
  parameter values. Function \code{get.subrange} extracts a
  subset of these parameter values. This can be useful if
  one wants to \dQuote{zoom} into a region of interest, or
  if the number of dimensions have to be reduces (using
  \code{drop=TRUE}) in order to visualize the results by
  means of plots.

  Each list entry in argument \code{sub.params} must
  correspond to a name in list \code{obj$dist.params}. Each
  member of this list entry is either \itemize{ \item a
  (non-empty) vector of integers, or \item a pair of
  numeric values. } Integer vectors give the indices of the
  elements in the vector of values of the corresponding
  parameter as listed in \code{obj$dist.params}. These are
  then included in the subset.

  Pairs of numeric values are interpreted as lower and
  upper bound, resp., for the values to be included in the
  object with subsets.

  If a parameter from \code{obj$dist.params} is omitted in
  \code{sub.params}, then all its values are included in
  the new object.
}
\note{
  Function \code{get.subrange} can be called recursively if
  \code{drop=FALSE}.
}
\author{
  Josef Leydold \email{josef.leydold@wu.ac.at}
}
\seealso{
  \code{\link{rvgt.range.engine}} for a description of
  class \code{"rvgt.range"}.
}

