## --------------------------------------------------------------------------
##'
##' Print result of range tests
##
## --------------------------------------------------------------------------
##
##  @description
##'
##' \code{print} method for class \code{"rvgt.range"}.
##' 
## --------------------------------------------------------------------------
## 
##  @details
## 
##' Method \code{print.range.engige} tries to give a comprehensive overview
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
##' @method print rvgt.range
##' 
## --------------------------------------------------------------------------
##'
##  Arguments:
##' @param x
##'        object of class \code{"rvgt.range"} to be printed.
##' @param ...
##'        further arguments to be passed to print method
##'        (ignored in this function).
##'
## --------------------------------------------------------------------------

print.rvgt.range <- function(x,...) {
        cat("\n * Test ranges of parameters - Summary:\n")
        cat("\n Test:\n  ", x$test.class, "\n")
        if (! is.null(x$test.name))
                cat("\n  ", x$test.name, "\n")
        cat("\n RVG:\n  ", x$rdist.name, "\n")
        cat("\n Parameters:\n")
        for (i in 1:length(x$dist.params)) {
                cat("  ",names(x$dist.params)[i],
                    "(", length(x$dist.params[[i]]), ")\n")
                print(x$dist.param[[i]])
        }
        cat("\n Results:\n")
        print(summary.default(x$data[is.finite(x$data)]))
        cat("\n Tests started at", format(x$started),"\n")
        cat("\n Total runtime:",format(x$runtime),"\n")
        cat("\n")
}

## --- End ------------------------------------------------------------------
