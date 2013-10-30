## --------------------------------------------------------------------------
##
##' Plot result of range tests
## 
## --------------------------------------------------------------------------
##
##  @description
##' 
##' \code{plot} method for class \code{"rvgt.range"}.
##' 
## --------------------------------------------------------------------------
## 
##  @details
## 
##' Method \code{plot.rvgt.range} allows to visualize the output of routine
##' \code{rvgt.range.engine}. The plot depends on the number of parameters
##' where a range of values if given.
##' If there is one such parameter then the output of the test routine are
##' plotted against this single parameter. If two such parameters with ranges
##' of values are given then the output is presented by means of an image plot.
##' Three or more such parameters are not supported.
##'
##' Argument \code{sub.params} allows to restrict the given parameter
##' values before plotting. This allows to \dQuote{zoom} into a region of
##' interest. It also allows to restrict a vector of parameter values to one
##' of its entries. Thus one can reduce the number of dimensions for the plot.
##'
##' The entries of \code{sub.params} correspond to the parameters of the
##' distribution as given in argument \code{x}.
##' Each member of this list is either
##' \itemize{
##'   \item a (non-empty) vector of integers, or
##'   \item a pair of numeric values.
##' }
##' Integer vectors give the indices of the elements in the vector of values
##' of the corresponding parameter as listed in \code{x$dist.params}.
##' These are then included in the subset.
##' Pairs of numeric values are interpreted as lower and upper bound, resp.,
##' for the values to be included in the object with subsets.
##' If a parameter from \code{x$dist.params} is omitted in \code{sub.params},
##' then all its values are included in the new object.
##' (The subset of parameters is actually created by means of function
##' \code{\link{get.subrange}}.)
##' 
##' Arguments \code{xscale}, \code{yscale}, and \code{zscale} allow to select
##' a particular scaling (\code{"linear"} or \code{"logarithmic"}) for the
##' given data. Notice, the z-axis always corresponds to the data.
##' So for an 1-dimensional plot we only have an x-axis and an z-axis and
##' the value of \code{yscale} is ignored.
##' 
## --------------------------------------------------------------------------
##'
##' @seealso
##' \code{\link{rvgt.range.engine}} for a description of objects of class
##' \code{"rvgt.range"}.
##' 
## --------------------------------------------------------------------------
##'
##' @author Josef Leydold \email{josef.leydold@@wu.ac.at}
##'
## --------------------------------------------------------------------------
##'
##' @examples
##' ## Simple "test" that just samples one random number
##' create.sample <- function (rdist, dist.params, ...) {
##'    cl <- as.call(c(list(name=rdist,n=1),dist.params))
##'    eval(cl)
##' }
##' samp <- rvgt.range.engine(rdist=rnorm,
##'                           dist.params=list(mean=0:10, sd=1:10),
##'                           test.routine=create.sample,
##'                           test.class="sample")
##'
##' ## Plot all samples
##' plot(samp)
##'
##' ## Plot all samples, use logarithmic scale for parameter 'sd'
##' plot(samp, yscale="logarithmic")
##'
##' ## Plot samples where 'sd' is the 5th entry (of 1:10).
##' ## It is important that index 5 is given as integer 5L. 
##' plot(samp, sub.params=list(sd=5L))
##' 
##' ## Plot samples where 'sd' is restricted to the 5th, 6th and 7th entry.
##' ## Notice that 5:7 creates a vector of integers.
##' plot(samp, sub.params=list(sd=5:7))
##' 
##' ## Plot samples where 'mean' is between 7 and 9.
##' ## This time we have to give a pair of numeric values which represent
##' ## the lower and upper bound, resp.
##' ## Notice that exact comparisons of floating point numbers can sometimes
##' ## have surprising results. So it is recommended to add some tolerance.
##' plot(samp, sub.params=list(mean=c(6.99,9.01)))
##'
##' ## Plot samples where 'mean' equals 7
##' ## (again we have to use a pair of numeric values).
##' plot(samp, sub.params=list(mean=c(6.99,7.01)))
##'
##' ## Zoom into the region with smalls values for 'mean' and 'sd'.
##' plot(samp, sub.params=list(mean=c(0,5.01), sd=c(0,5.01)))
##' 
## --------------------------------------------------------------------------
##'
##' @method plot rvgt.range
##' 
## --------------------------------------------------------------------------
##'
##  Arguments:
##' @param x
##'        object of class \code{"rvgt.range"} to be plotted.
##' 
##' @param sub.params
##'        subset of parameter values which should be plotted (list).
##'        This allows to restrict the plotting region.
##' 
##' @param xscale
##'        type of scale used for first coordinate.
##' @param yscale
##'        type of scale used for second coordinate.
##' @param zscale
##'        type of scale used for third coordinate.
##'
##' @param ...
##'        graphical parameters (see \code{\link{plot}} and
##'        \code{\link{par}}).
##' 
## --------------------------------------------------------------------------

plot.rvgt.range <- function (x, sub.params=list(),
                             xscale=c("linear","logarithmic"),
                             yscale=c("linear","logarithmic"),
                             zscale=c("auto","linear","logarithmic"),
                             ...) {
        ## ..................................................................
        
        ## --- maybe we only want to have a subset of parameter values 
        obj <- get.subrange(x, sub.params, drop=TRUE)

        ## --- get number of dimensions
        dims <- length(dim(obj$data))

        ## --- read parameters for plot
        xscale <- match.arg(xscale)
        yscale <- match.arg(yscale)
        zscale <- match.arg(zscale)

        ## --- choose scale type automatically
        if (zscale=="auto") {
                zdata <- obj$data[is.finite(obj$data)]
                zmin <- min(zdata)
                zmax <- max(zdata)
                zscale <- ifelse(isTRUE(zmax/zmin > 100), "logarithmic", "linear")
        }

        ## --- plot
        if (identical(dims, 1L)) {

                xname <- names(dimnames(obj$data))[1]
                if (xscale=="linear") {
                        xlabel <- xname
                        x <- obj$dist.params[[xname]]
                } else if (xscale=="logarithmic") {
                        xlabel <- paste("log(",xname,")",sep="")
                        x <- log10(obj$dist.params[[xname]])
                } else {
                        stop("unknown xscale")
                }
                
                if (zscale=="linear") {
                        zlabel <- obj$test.class ## FIXME
                        z <- obj$data
                } else if (zscale=="logarithmic") {
                        zlabel <- paste("log(",obj$test.class,")",sep="") ## FIXME
                        z <- log10(obj$data)
                } else {
                        stop("unknown zscale")
                }
                
                plot(x=x, y=z,
                     xlab=xlabel, ylab=zlabel,
                     type="l", ...)

        } else if (identical(dims, 2L)) {
                pnames <- names(dimnames(obj$data))

                xname <- pnames[1]
                if (xscale=="linear") {
                        xlabel <- xname
                        x <- obj$dist.params[[xname]]
                } else if (xscale=="logarithmic") {
                        xlabel <- paste("log(",xname,")",sep="")
                        x <- log10(obj$dist.params[[xname]])
                } else {
                        stop("unknown xscale")
                }
                
                yname <- pnames[2]
                if (yscale=="linear") {
                        ylabel <- yname
                        y <- obj$dist.params[[yname]]
                } else if (yscale=="logarithmic") {
                        ylabel <- paste("log(",yname,")",sep="")
                        y <- log10(obj$dist.params[[yname]])
                } else {
                        stop("unknown yscale")
                }
                
                if (zscale=="linear") {
                        z <- obj$data
                } else if (zscale=="logarithmic") {
                        z <- log10(obj$data)
                } else {
                        stop("unknown zscale")
                }

                filled.contour(x=x, y=y, z=z,
                               xlab=xlabel, ylab=ylabel,
                               color.palette=topo.colors,
                               ...
                               )
                ## FIXME
        } else {
                stop("plot.rvgt.range() handles arrays of rank 1 or 2 only")
        }
        
        ## FIXME
}

## --- End ------------------------------------------------------------------
