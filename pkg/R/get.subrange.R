## --------------------------------------------------------------------------
##'
##' Get subset of distribution parameters in object of class
##' "rvgt.range" (internal function) 
## 
## --------------------------------------------------------------------------
##
##  @description
##'
##' Get subset of distribution parameters in object of class
##' \code{"rvgt.range"}.
##' -- (internal function)
##' 
## --------------------------------------------------------------------------
##
##  @details
## 
##' An object of class \code{"rvgt.range"} contains the results of some test
##' that has been performed on a set of parameter values.
##' Function \code{get.subrange} extracts a subset of these parameter values.
##' This can be useful if one wants to \dQuote{zoom} into a region of interest,
##' or if the number of dimensions have to be reduces (using \code{drop=TRUE})
##' in order to visualize the results by means of plots.
##' 
##' Each list entry in argument \code{sub.params} must correspond to a name
##' in list \code{obj$dist.params}.
##' Each member of this list entry is either
##' \itemize{
##'   \item a (non-empty) vector of integers, or
##'   \item a pair of numeric values.
##' }
##' Integer vectors give the indices of the elements in the vector of values
##' of the corresponding parameter as listed in \code{obj$dist.params}.
##' These are then included in the subset.
##'
##' Pairs of numeric values are interpreted as lower and upper bound, resp.,
##' for the values to be included in the object with subsets.
##' 
##' If a parameter from \code{obj$dist.params} is omitted in \code{sub.params},
##' then all its values are included in the new object.
##' 
## --------------------------------------------------------------------------
##'
##' @note
##' Function \code{get.subrange} can be called recursively if
##' \code{drop=FALSE}.
##' 
## --------------------------------------------------------------------------
##'
##' @seealso
##' \code{\link{rvgt.range.engine}} for a description of class
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
##' @param sub.params
##'        subset of parameter values (list).
##' @param drop
##'        if TRUE then dimensions with 1 entry are removed (boolean).
##'
## --------------------------------------------------------------------------
##'
##' @return
##'        An object of class \code{"rvgt.range"} that contains the subset
##'        of parameter values.
##' 
## --------------------------------------------------------------------------

get.subrange <- function (obj, sub.params=list(), drop=TRUE) {
        ## ..................................................................
        
        ## --- check object
        
        if (missing(obj) || ! is(obj, "rvgt.range"))
                stop("Argument 'obj' is missing or invalid.")

        ## --- all indices of parameter lists

        idx.params <- lapply(obj$dist.params, function(x){1:length(x)})

        ## --- missing sub.params

        ## we have to handle the case where there is just one entry
        ## in a dimension.

        if (identical(sub.params, list()))
                sub.params <- idx.params
        
        ## --- subset of parameters

        if (! is.list(sub.params))
                stop("Argument 'sub.params' is invalid.")
        if (is.null(names(sub.params)) || "" %in% names(sub.params))
                stop("Parameters in 'sub.params' must have names.")
        if (! all(names(sub.params) %in% names(obj$dist.params))) 
                stop("Parameter names in 'sub.params' must occur in 'obj'.")

        ## --- find sub ranges
        
        for (i in 1:length(sub.params)) {

                ## read input
                param <- sub.params[[i]]
                param.name <- names(sub.params[i])
                orig.param <- obj$dist.params[[param.name]]

                ## get indices for particular parameter
                if (is.integer(param)) {
                        ## case: integer vector --> list of indices
                        iparam <- param
                } else if (is.numeric(param)) {
                        ## case: pair of numerics --> lower and uppper bound 
                        if (! identical(length(param), 2L))
                                stop(paste("Argument 'sub.params$", param.name,
                                           "' must be integer vector or a pair of numerics.", sep=""))
                        iparam <- which(orig.param >= param[1] & orig.param <= param[2])
                } else {
                        stop(paste("Argument 'sub.params$", param.name,
                                   "' has invalid type (requires integer or numeric).", sep=""))
                }

                ## check input
                if (! all( iparam >= 1L & iparam <= length(orig.param))) {
                        stop(paste("Argument 'sub.params$", param.name, "' out of range.", sep=""))
                }
                if (identical(length(iparam), 0L))
                        stop(paste("Argument 'sub.params$", param.name, "' has no valid entries.", sep=""))
                
                ## update list of indices
                idx.params[[param.name]] <- iparam

                ## update 'obj$dist.params'
                obj$dist.params[[param.name]] <- (obj$dist.params[[param.name]])[iparam]
        }

        ## copy data for test result
        cmd <- paste("obj$data[")
        for (i in 1:length(obj$dist.params))
                cmd <- paste(cmd, "idx.params[[",i,"]],")
        cmd <- paste(cmd, "drop=drop]")
        result <- eval(parse(text=cmd))

        ## unfortunately [,drop=TRUE] discards the 'dim' and 'dimnames' attribute
        ## if the resulting array is a vector.
        ## so we have to set it again.
        if (is.null(dim(result)) && length(result) > 1L) {
                dim(result) <- length(result)
                pname <- names(obj$dist.params)[lapply(obj$dist.params, length)>1L]
                dimnames(result) <- obj$dist.params[pname]
        }
        
        ## return result
        obj$data <- result
        obj
}

## --- End ------------------------------------------------------------------
