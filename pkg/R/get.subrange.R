## --------------------------------------------------------------------------
##'
##' Get subset of parameters in object of class "rvgt.range"
##' (internal function) 
## 
## --------------------------------------------------------------------------
##
##  @description
##'
##' Get subset of parameters in object of class \code{"rvgt.range"}.
##' -- (internal function)
##' 
## --------------------------------------------------------------------------
##
##  @details
## 
##' Function \code{get.subrange} extracts a subset of parameter values from
##' an object of class \code{"rvgt.range"}.
##' This can be useful if one wants to \dQuote{zoom} into a region of interest,
##' or if the number of dimensions have to be reduces (using \code{drop=TRUE})
##' in order to visualize the results by means of plots.
##'
##' Each list entry in argument \code{sub.params} must correspond to a name
##' in list \code{obj$dist.params} or list \code{obj$r.params}.
##' Each member of this list entry is either
##' \itemize{
##'   \item
##'   a (non-empty) vector of integers which represents indices, or 
##'   \item
##'   a (non-empty) vector of floats which represent values.
##' }
##' The corresponding elements with the given indices and values, resp.,
##' are then included in the subset.
##'
##' Alternatively, a name in argument \code{sub.params} can also be one
##' of the names in \code{obj$dist.params} or \code{obj$r.params} with
##' suffix \code{.lim} appended and the corresponding entry must be
##' pairs of numeric values that are interpreted as lower and upper bound,
##' resp., for the values to be included in the object with subsets.
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

        params <- c(obj$dist.params, obj$r.params)
        idx.params <- lapply(params, function(x){1:length(x)})

        ## --- missing sub.params

        if (identical(sub.params, list()))
                sub.params <- idx.params
        
        ## --- subset of parameters

        if (! is.list(sub.params))
                stop("Argument 'sub.params' is invalid.")

        if (is.null(names(sub.params)) || "" %in% names(sub.params))
                stop("Parameters in 'sub.params' must have names.")

        pnames <- str_replace(names(sub.params), "\\.lim$", "")
        if (! all(pnames %in% names(params))) 
                stop("Parameter names in 'sub.params' must occur in 'obj'.")

        ## --- find sub ranges
        
        for (i in 1:length(sub.params)) {

                ## read input
                param <- sub.params[[i]]
                param.name <- names(sub.params[i])
                orig.param <- params[[param.name]]

                ## get indices for particular parameter
                if (isTRUE(str_detect(param.name, "\\.lim$"))) {
                        ## case: lower and uppper bound 
                        if (! (is.numeric(param) && identical(length(param), 2L)))
                                stop(paste("Argument 'sub.params$", param.name,
                                           "' must be a pair of numerics.", sep=""))
                        ## strip postfix '.lim'
                        param.name <- str_replace(param.name, "\\.lim$", "")
                        orig.param <- params[[param.name]]
                        ## get indices
                        iparam <- which(orig.param >= param[1] & orig.param <= param[2])
                        
                } else if (is.integer(param)) {
                        ## case: integer vector --> list of indices
                        iparam <- param

                } else if (is.numeric(param)) {
                        ## case: numerics --> list of values
                        iparam <- findidx (orig.param, param)
                } else {
                        stop(paste("Argument 'sub.params$", param.name,
                                   "' has invalid type (requires integer or numeric).", sep=""))
                }

                ## check result
                if (! all( iparam >= 1L & iparam <= length(orig.param))) {
                        stop(paste("Argument 'sub.params$", param.name, "' out of range.", sep=""))
                }
                if (identical(length(iparam), 0L))
                        stop(paste("Argument 'sub.params$", param.name, "' has no valid entries.", sep=""))
                
                ## update list of indices
                idx.params[[param.name]] <- iparam

                ## update 'obj$dist.params' or 'obj$r.params'
                if (param.name %in% names(obj$dist.params)) {
                        obj$dist.params[[param.name]] <- (obj$dist.params[[param.name]])[iparam]
                } else if (param.name %in% names(obj$r.params)) {
                        obj$r.params[[param.name]] <- (obj$r.params[[param.name]])[iparam]
                } else {
                        stop("internal error")
                }
        }

        ## copy data for test result
        cmd <- paste("obj$data[")
        for (i in 1:length(c(obj$dist.params, obj$r.params)))
                cmd <- paste(cmd, "idx.params[[",i,"]],")
        cmd <- paste(cmd, "drop=drop]")
        result <- eval(parse(text=cmd))

        ## unfortunately [,drop=TRUE] discards the 'dim' and 'dimnames' attribute
        ## if the resulting array is a vector.
        ## so we have to set it again.
        if (is.null(dim(result)) && length(result) > 1L) {
                dim(result) <- length(result)
                params <- c(obj$dist.params, obj$r.params)
                pname <- names(params)[lapply(params, length)>1L]
                dimnames(result) <- params[pname]
        }

        ## return result
        obj$data <- result
        obj
}

## --- Extract subvector  ---------------------------------------------------

findidx <- function(vec, s) {
        ## find positions of entries in 's' in vector 'vec'.
        ## 'vec' and 's' may be floats.

        ## tolerance for comparing floats.
        ## it is assumed that differences are just due to truncation
        ## errors when storing the numbers.
        tol <- 8 * .Machine$double.eps

        ## find positions
        sapply(s, function(e){which(sapply(vec, function(v){.unur.FP.equal(v,e)}))})
}

## --- Compare two floats ---------------------------------------------------

.unur.FP.cmp <- function(x,y,eps) {
        ## Compare two floats:
        ##  x eq y iff |x-y| <= min(|x|,|y|) * eps
        ## 
        ## parameters:
        ##   x   ... numeric
        ##   y   ... numeric
        ##   eps ... maximal relative deviation
        ##
        ## return:
        ##   -1 if x < y
        ##    0 if x eq y
        ##   +1 if x > y
        ##
        ## remark:
        ##   This is similar to Knuth's algorithm. However, we use
        ##   min(|x|,|y|) instead of max(|x|,|y|).
        ##   We also have to deal with +-Inf correctly.
        ##
        ##   (For an implementation of Knuth's algorithm see
        ##   fcmp 1.2.2 Copyright (c) 1998-2000 Theodore C. Belding
        ##   University of Michigan Center for the Study of Complex Systems
        ##   Ted.Belding@umich.edu)
        ## ..................................................................

        fx = ifelse(x>=0, x, -x)
        fy = ifelse(y>=0, y, -y)
        delta = eps * min(fx,fy)
        difference = x - y

        ## we have to take care about Inf
        if (is.infinite(delta)) {
                delta = eps * .Machine$double.xmax
        }

        ## denormalized numbers (close to zero) may cause problems.
        ## so we check this special case.
        if (isTRUE(fx <= 2 * .Machine$double.xmin && fy <= 2 * .Machine$double))
                return(0)

        if (difference > delta) { ## x > y
                return (+1)
        } else if (difference < -delta) { ## x < y
                return (-1)
        } else { ## -delta <= difference <= delta
                return (0)   
        }

} ## end of .unur.FP.cmp() ##

.unur.FP.equal <- function(x,y) {
        ## tolerance for comparing floats.
        ## it is assumed that differences are just due to truncation
        ## errors when storing the numbers.
        ## so these may differ in the 3 least significant bits
        tol <- 8 * .Machine$double.eps

        ifelse(.unur.FP.cmp(x,y,tol)==0, TRUE, FALSE)
}

## --- End ------------------------------------------------------------------
