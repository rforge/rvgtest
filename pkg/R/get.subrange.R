## --------------------------------------------------------------------------
##'
##' Extract submatrix from an object of class "rvgt.range"
##' (internal function) 
## 
## --------------------------------------------------------------------------
##
##  @description
##'
##' Extract a submatrix from an object of class \code{"rvgt.range"}.
##' The new object also is of class \code{"rvgt.range"} and 
##' corresponds to a test with the given subset of parameters.
##' -- (internal function)
##' 
## --------------------------------------------------------------------------
##
##  @details
## 
##' Function \code{get.subrange} extracts an object of class \code{"rvgt.range"}
##' from a given one. The new object corresponds to a test with the given subset
##' of parameters.
##' This can be useful if one wants to \dQuote{zoom} into a region of interest,
##' or if the number of dimensions have to be reduces (using \code{drop=TRUE})
##' in order to visualize the results.
##'
##' The subset of parameters is given by argument \code{sub.params}.
##' Each of its entry must correspond to a name in list
##' \code{obj$dist.params} or list \code{obj$r.params}.
##' The subset can be selected by either
##' \itemize{
##'   \item
##'   a list of indices given as a vector of type \code{"integer"}, or
##'   \item
##'   a list of values given as a vector of type \code{"double"}.
##' }
##' Alternatively, a name in argument \code{sub.params} can also be one
##' of the names in \code{obj$dist.params} or \code{obj$r.params} with
##' suffix \code{.lim} appended and the corresponding entries must be
##' pairs of type \code{"double"} for the lower and upper bound,
##' resp., for the parameter values.
##'
##' If a parameter from \code{obj$dist.params} or \code{obj$r.params}
##' is omitted in \code{sub.params}, then all its values are included
##' in the new object.
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
##' @examples
##' ## create an initial object with timing data
##' mgt <- rvgt.range.marginal(rdist = rbeta,
##'                            dist.params = list(
##'                              shape1=c(0.1,1,10),
##'                              shape2=c(0.1,1,10)),
##'                            duration = 0.01, gen.time = 1e-5)
##'
##' ## extract an object where shape1 is the second entry
##' rvgtest:::get.subrange(mgt, sub.params=list(shape1=2L))
##' 
##' ## extract an object where shape1 is the first and third entry
##' ## and shape2 is 0.1 and 1
##' rvgtest:::get.subrange(mgt, sub.params=list(shape1=c(1L,3L), shape2=c(0.1,1)))
##' 
##' ## extract an object where shape2 is in closed interval 0.1, 1
##' rvgtest:::get.subrange(mgt, sub.params=list(shape2.lim=c(0.1,1)))
##' 
## --------------------------------------------------------------------------
##'
##  Arguments:
##' @param obj
##'        object of class \code{"rvgt.range"}.
##' @param sub.params
##'        list that contains the subset of parameter values for the
##'        distribution parameters or for additional parameters of the
##'        random variate generator.
##'        These can be
##'        either indices given as integers like \code{c(1L,3L)}
##'        or \code{2:5},
##'        or values given by doubles like \code{c(1,3,5)}.
##'        Alternatively, lower and upper bound can be given as a pair of
##'        numbers when the name of the parameter is postfixed by
##'        \code{.lim}.
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
                        iparam <- floatmatch (param, orig.param, param.name)
                } else {
                        stop(paste("Argument 'sub.params$", param.name,
                                   "' has invalid type (requires integer or numeric).", sep=""))
                }

                ## check result
                if (! all( iparam >= 1L & iparam <= length(orig.param))) {
                        stop(paste("Argument 'sub.params$", param.name, "' out of range.", sep=""))
                }
                if (identical(length(iparam), 0L)) {
                        stop(paste("Argument 'sub.params$", param.name, "' has no valid entries.", sep=""))
                }
                
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

floatmatch <- function(s, vec, msg) {
        ## 'floatmatch' returns a vector of the positions of matches of
        ## its first argument in its second.

        idx <- integer(0)
        for (x in s) {
                pos <- which(sapply(vec, function(v){.unur.FP.equal(v,x)}))
                if (identical(length(pos), 0L)) {
                        warning("Cannot find entry '",x,"' in parameter '",msg,"'.")
                }
                idx <- c(idx, pos)
        }
        idx
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
