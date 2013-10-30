## --------------------------------------------------------------------------
##'
##' Summarizing result of range tests
##
## --------------------------------------------------------------------------
##
##  @description
##'
##' \code{summary} method for class \code{"rvgt.range"}.
##' 
## --------------------------------------------------------------------------
## 
##  @details
## 
##' \code{summary.range.engige} tries to give a comprehensive overview
##' of the test results.
##' 
## --------------------------------------------------------------------------
##'
##' @seealso
##' \code{\link{rvgt.range.engine}} a description of class
##' \code{"rvgt.range"}.
##' 
## --------------------------------------------------------------------------
##'
##' @author Josef Leydold \email{josef.leydold@@wu.ac.at}
##'
## --------------------------------------------------------------------------
##'
##' @method summary rvgt.range
##' 
## --------------------------------------------------------------------------
##'
##  Arguments:
##' @param object
##'        object of class \code{"rvgt.range"} to be summarized.
##' @param ...
##'        further arguments to be passed to summary method
##'        (ignored in this function).
##'
## --------------------------------------------------------------------------

summary.rvgt.range <- function(object,...) {
        cat("\n * Test ranges of parameters - Summary:\n")

        cat("\n Test:\n  ", object$test.class, "\n")
        if (! is.null(object$test.name))
                cat("\n  ", object$test.name, "\n")

        cat("\n RVG:\n  ", object$rdist.name, "\n")

        cat("\n Parameters:\n")
        for (i in 1:length(object$dist.params)) {
                cat("  ",names(object$dist.params)[i],
                    "(", length(object$dist.params[[i]]), ")\n")
                print(object$dist.param[[i]])
        }

        if (length(object$r.params)>0) {
                cat("\n Additional Parameters:\n")
                for (i in 1:length(object$r.params)) {
                        cat("  ",names(object$r.params)[i],"\n")
                        print(object$r.param[[i]])
                }
        }

        cat("\n Results:\n")
        print(summary.default(object$data[is.finite(object$data)]))

        cat("\n Tests started at", format(object$started),"\n")
        cat("\n Total runtime:",format(object$runtime),"\n")
        cat("\n")
}

## --- End ------------------------------------------------------------------
