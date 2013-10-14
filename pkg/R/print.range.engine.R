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
##  Arguments:
##' @param obj
##'        object of class \code{"rvgt.range"}.
##'
## --------------------------------------------------------------------------

print.rvgt.range <- function(obj,...) {
        cat("\n * Test ranges of parameters - Summary:\n")
        cat("\n Test:\n  ", obj$test.class, "\n")
        if (! is.null(obj$test.name))
                cat("\n  ", obj$test.name, "\n")
        cat("\n RVG:\n  ", obj$rdist.name, "\n")
        cat("\n Parameters:\n")
        for (i in 1:length(obj$dist.params)) {
                cat("  ",names(obj$dist.params)[i],
                    "(", length(obj$dist.params[[i]]), ")\n")
                print(obj$dist.param[[i]])
        }
        cat("\n Results:\n")
        print(summary.default(obj$data[is.finite(obj$data)]))
        cat("\n Tests started at", format(obj$started),"\n")
        cat("\n Total runtime:",format(obj$runtime),"\n")
        cat("\n")
}

## --- End ------------------------------------------------------------------
