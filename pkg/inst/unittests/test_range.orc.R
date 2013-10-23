## --------------------------------------------------------------------------
##
## Check function rvgt.range.orc()
##
## --------------------------------------------------------------------------

context("[rvgt.range.orc] - test rejection constant")

## --------------------------------------------------------------------------

test_that("[orc-001] calling rvgt.range.orc", {

        dp <- list(shape1=2:7, shape2=2)
        
        ## a simple generator for the beta distribution 
        myrbeta <- function(n, shape1, shape2, show.properties=FALSE) {
                if (shape1 <= 1 || shape2 <= 1 || n < 0) stop("arguments invalid")
                mode <- (shape1 - 1) / (shape1 + shape2 - 2)
                fmode <- dbeta(mode,shape1,shape2)
                trials <- 0
                res <- numeric(n) 
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
                if (isTRUE(show.properties)) {
                        trc <- fmode
                        if (isTRUE(all.equal(shape1,5))) trc <- 0.1
                        attr(res,"trc") <- trc
                        attr(res,"orc") <- trials / n
                }
                res
        }

        ## first we need the marginal generation times
        mgt <- rvgt.range.marginal(rdist = myrbeta, dist.params=dp,
                                   duration = 0.01)

        mgt$data[1] <- NA
        mgt$data[2] <- Inf
        mgt$data[3] <- 1e10


        ## test rejection constants
        msg <- "ERROR: rejection constant too small!!!!"
        expect_warning(orc <- rvgt.range.orc(rdist=myrbeta, dist.params=dp,
                              duration=0.01, gen.time=mgt, verbose=FALSE),  msg)

        expect_true(is.na(orc$data[1]))
        expect_true(is.infinite(orc$data[2]) && orc$data[2] > 0)
        expect_true(is.infinite(orc$data[3]) && orc$data[3] > 0)
        expect_true(is.nan(orc$data[4]))
        expect_true(all(orc$data[5:6] > 1e-5) && all(orc$data[5:6] < 0.99999)) 

})

## --------------------------------------------------------------------------

context("[rvgt.range.orc] - Invalid arguments")

## --------------------------------------------------------------------------

test_that("[orc-i01] calling rvgt.range.orc with invalid arguments: show.properties", {

        dp <- list(mean=c(1,2),sd=c(3,4))

        ## create object of class "rvgt.range.time"
        emgt <- rvgt.range.engine(rdist=rnorm,
                                  dist.params=dp,
                                  test.routine=function(emgt,...){emgt},
                                  test.class="time.marginal",
                                  duration=0.01, gen.time=0.01
                                  )

        ## show.properties
        msg <- "'rdist' must have argument 'show.properties'."
        expect_error( rvgt.range.orc(
          rdist=rnorm, dist.params=dp, gen.time=emgt), msg)

        ## property 'trc'
        msg <- "returned value of 'rdist' must have attribute 'trc'."
        myrnorm <- function(n,mean,sd,show.properties) { rnorm(n,mean,sd) }
        expect_error( rvgt.range.orc(
          rdist=myrnorm, dist.params=dp, gen.time=emgt),  msg)

        ## property 'orc'
        msg <- "returned value of 'rdist' must have attribute 'orc'."
        myrnorm <- function(n,mean,sd,show.properties) {
                X <- rnorm(n,mean,sd); attr(X,"trc") <- 1.23; X }
        expect_error( rvgt.range.orc(
          rdist=myrnorm, dist.params=dp, gen.time=emgt),  msg)
})

test_that("[orc-i02] calling rvgt.range.orc with invalid arguments: gen.time", {
        ## gen.time

        myrnorm <- function(n,mean,sd,show.properties) {
                X <- rnorm(n,mean,sd);
                attr(X,"trc") <- 1.23; attr(X,"orc") <- 1.23;
                X }

        msg <- "argument \"gen.time\" is missing, with no default"
        expect_error( rvgt.range.orc(
          rdist=myrnorm, dist.params=list(mean=1:2,sd=1:2)), msg)

        msg <- "Argument 'gen.time' must be of class \"rvgt.range.time\"."
        expect_error( rvgt.range.orc(
          rdist=myrnorm, dist.params=list(mean=1:2,sd=1:2), gen.time=1), msg)

})

## --------------------------------------------------------------------------
