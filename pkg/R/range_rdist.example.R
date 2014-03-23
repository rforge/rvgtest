## --------------------------------------------------------------------------
##'
##' Example of a RVG that can be used with all range tests
## 
## --------------------------------------------------------------------------
##
##  @description
##' 
##' Prototype implementation for a random variate generator \code{rdist}
##' that can be used with all range tests.
##' 
## --------------------------------------------------------------------------
## 
##  @details
## 
##' Function \code{rvgt.range.rdist.example} is a prototype
##' implementation of a random variate generator \code{rdist} that can
##' be used with all range tests that make use of engine
##' \code{\link{rvgt.range.engine}}.
##'
##' This prototype shows all obligatory and optional argument to 
##' \code{rdist} as well as all possible return values and attributes that
##' are required by at least one of some tests.
##'
##' In this example \code{shape1} and \code{shape2} are the parameters
##' for the distribution and their values have to be passed by
##' means of argument \code{dist.params} to routine
##' \code{\link{rvgt.range.engine}}. Of course there can be only one
##' parameter or more than two such parameters.
##' Argument \code{kind} is an additional parameter for the generator
##' function. Its values have to be passed by 
##' means of argument \code{r.params} to routine
##' \code{\link{rvgt.range.engine}}. Of course there need not be such
##' a parameter and there also can be no more than one such parameter.
##'
##' Argument \code{show.properties} is used to enable storing properties
##' of the generator as attributes of the returned random sample.
##'
##' Although this example generator returns beta random variates it only demonstrates
##' the API for \code{rdist}.
##'
## --------------------------------------------------------------------------
##'
##' @seealso
##' \code{\link{rvgt.range.engine}} for a description of the engine
##' that runs all these test.
##'
## --------------------------------------------------------------------------
##'
##' @author Josef Leydold \email{josef.leydold@@wu.ac.at}
##'
## --------------------------------------------------------------------------
##
##  @examples
##
## --------------------------------------------------------------------------
##'
##  Arguments:
##' @param n
##'        size of random sample (number).
##' @param shape1
##'        parameter 1 for distribution (number).
##' @param shape2
##'        parameter 2 for distribution (number).
##' @param kind
##'        parameter for generator (independent from the
##'        parameter for the distribution (number).
##' @param show.properties
##'        if TRUE properties of the generator and the generated
##'        samples (like the rejection constant) should be stored
##'        as attributes of the returned vector.
##' 
## --------------------------------------------------------------------------
##'
##' @return
##' The function returns an array of size \code{n} that contains the
##' requested random sample. If \code{show.properties=TRUE} then
##' properties of the generator are stored as attributes of the
##' returened vector. These are properties are required by (or at least
##' optional for) some range tests.
##' Currently the following properties (attributes) are set:
##' \describe{
##'   \item{\code{"trc"}}{
##'         theoretical rejection constant
##'         (required by \code{\link{rvgt.range.trc}})}
##'   \item{\code{"orc"}}{
##'         observed rejection constant
##'         (required by \code{\link{rvgt.range.orc}})}
##' }
##' 
## --------------------------------------------------------------------------

rvgt.range.rdist.example <- function (n, shape1, shape2, kind, show.properties=FALSE) {

    ## parameter 'kind' should demonstrate how additional paramters
    ## can be passed to function 'rdist'.
    ## It could be used, e.g., to select a particular algorithm for
    ## the drawing a random sample.
    ## The parameter is not used yet.

    ## check arguments
    if (n < 0)
        stop("Argument 'n' invalid")
        
    ## check parameters of distribution
    if (shape1 <= 1 || shape2 <= 1)
        stop("Parameter 'shape1' or 'shape2' out of range")

    ## setup
    mode <- (shape1 - 1) / (shape1 + shape2 - 2)
    fmode <- dbeta(mode,shape1,shape2)

    ## generation
    res <- numeric(n) 
    trials <- 0

    for (i in 1:n) {
        while(n>0) {
            trials <- trials + 1
            X <- runif(1)
            Y <- fmode * runif(1)
            if (Y <= dbeta(X,shape1,shape2)) {
                res[n] <- X
                break
            }
        }
    }

    ## store properties of generator as attributes
    if (isTRUE(show.properties)) {
        ## "trc" - theoretical rejection constant
        ##         (required by rvgt.range.trc)
        attr(res,"trc") <- fmode

        ## "orc" - observed rejection constant
        ##         (required by rvgt.range.orc)
        attr(res,"orc") <- trials / n
    }

    ## return result
    res
}

## --- End ------------------------------------------------------------------
