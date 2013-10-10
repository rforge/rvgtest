## --------------------------------------------------------------------------
##'
##' Convert array indices into position in underlying vector and vice versa.
##' 
## --------------------------------------------------------------------------
##
##  @description
##
##' Convert the indices of an element in an array with a given
##' \code{"dim"} attribute into the position in the underlying vector
##' or, vice versa, convert position of an element in vector into the
##' indices of a derived array with a given \code{"dim"} attribute.
##' -- (internal functions)
##'
## --------------------------------------------------------------------------
##
##  @details
##
##' Recall that an array (and in particular a matrix) is a vector with a
##' \code{"\link{dim}"} attribute.
##' For a given array (with given \code{"dim"} attribute) function
##' \code{coord2pos} converts the indices of an element in an array
##' into the position in the underlying vector.
##'
##' Vice versa, function \code{pos2coord} converts the position of an
##' element in a vector into the corresponding indices of an array
##' with given attribute \code{"dim"}.
##' 
## --------------------------------------------------------------------------
##'
##' @author Josef Leydold \email{josef.leydold@@wu.ac.at}
##'
## --------------------------------------------------------------------------
##' 
##' @examples
##' 
##' ## get indices of forth entry in a 2x3 matrix
##' rvgtest:::pos2coord(pos=4L, dims=c(2L,3L))
##' 
##' ## get position of element with indices c(2,3) in a 4x6 matrix
##' rvgtest:::coord2pos(coord=c(2L,3L), dims=c(4L,6L))
##' 
## --------------------------------------------------------------------------
##
##  Arguments:
##
##' @param coord
##'        vector of coordinates (vector of integers)
##' @param dims 
##'        dimensions of the array (vector of integers).
##' 
## --------------------------------------------------------------------------
##'
##' @return
##' 
##' Function \code{coord2pos} returns the position of the element in
##' the underlying vector (integer). 
##' 
## --------------------------------------------------------------------------

coord2pos <- function (coord, dims) {
        ## ..................................................................

        if (missing(coord))
                stop("Argument 'coord' missing")
        if (! (is.integer(coord) && length(coord)>0L))
                stop("Argument 'coord' invalid")
        if (! all(coord>0L)) 
                stop("Argument 'coord' invalid")

        if (missing(dims))
                stop("Argument 'dims' missing")

        ## special case
        if (is.null(dims)) return(NULL)

        if (! identical(length(coord), length(dims)))
                stop("Arguments 'coord' and 'dims' have different length")

        if (! (is.integer(dims) && length(dims)>0L))
                stop("Argument 'dims' invalid")
        if (! all(dims>0L))
                stop("Argument 'dims' invalid")

        if (! all(coord<=dims))
                stop("Argument 'coord' out of range")

        ## compute position
        pos <- coord[1]
        if (length(coord) > 1L)
                pos <- pos + sum( (coord[-1] - 1) * cumprod(dims[-(length(dims))]) )

        ## return value
        as.integer(pos)
}

## ==========================================================================

## --------------------------------------------------------------------------
##'
##' @rdname coord2pos
##' 
## --------------------------------------------------------------------------
##
##  Arguments:
##
##' @param pos
##'        position in underlying vector (integer).
##' @inheritParams pos2coord
##'
## --------------------------------------------------------------------------
##
##' @return
##' 
##' Function \code{pos2coord} returns the indices of the element in
##' the corresponding array
##' (an integer vector of the same length as \var{dims}).
##' 
## --------------------------------------------------------------------------

pos2coord <- function (pos, dims) {
        ## ..................................................................

        if (missing(pos))
                stop("Argument 'pos' missing")
        if (! (is.integer(pos) && identical(length(pos), 1L)))
                stop("Argument 'pos' invalid")

        if (missing(dims))
                stop("Argument 'dims' missing")

        ## special case
        if (is.null(dims)) return(1L)

        if (! (is.integer(dims) && length(dims)>0L))
                stop("Argument 'dims' invalid")
        if (! all(dims>0L)) 
                stop("Argument 'dims' invalid")

        if (! pos<=prod(dims))
                stop("Argument 'pos' out of range")

        ## compute array coordinates
        ndims <- length(dims)
        pdims <- as.integer(cumprod(dims))
        coord <- integer(ndims)
        tmp <- pos - 1L
        if (ndims > 1L) {
                for (i in ndims:2) {
                        coord[i] <- 1L + tmp %/% pdims[i-1]
                        tmp <- tmp %% pdims[i-1]
                }
        }
        coord[1] <- 1L + tmp

        ## return value
        coord
}

## --- End ------------------------------------------------------------------
