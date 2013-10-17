## --------------------------------------------------------------------------
##
## Check function rvgt.range.erc()
##
## --------------------------------------------------------------------------

context("[rvgt.range.erc] - test rejection constant")

## --------------------------------------------------------------------------

#test_that("[erc-001] calling rvgt.range.erc", {
#        rtest <- function(n,a,b,show.properties) {
#                if (isTRUE(all.equal(c(a,b),c(1,2)))) stop()
#                X <- 1
#                attr(X,"erc") <- a+b
#                X
#        }

#        erc <- rvgt.range.erc(rdist = rtest,
#                              dist.params = list(a=1:2,b=1:2),
#                              verbose = FALSE)

        ## plot(mgt, xscale="lin", yscale="lin", main="[erc-001]")

#        expect_equal(erc$data[1,1], 2)
#        expect_identical(erc$data[1,2], NA_real_)
#        expect_equal(erc$data[2,1], 3)
#        expect_equal(erc$data[2,2], 4)
#})


## --------------------------------------------------------------------------

context("[rvgt.range.erc] - Invalid arguments")

## --------------------------------------------------------------------------

test_that("[erc-i01] calling rvgt.range.erc with invalid arguments: show.properties", {

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
        expect_error( rvgt.range.erc(
          rdist=rnorm, dist.params=dp, gen.time=emgt), msg)

        ## property 'trc'
        msg <- "returned value of 'rdist' must have attribute 'trc'."
        myrnorm <- function(n,mean,sd,show.properties) { rnorm(n,mean,sd) }
        expect_error( rvgt.range.erc(
          rdist=myrnorm, dist.params=dp, gen.time=emgt),  msg)

        ## property 'erc'
        msg <- "returned value of 'rdist' must have attribute 'erc'."
        myrnorm <- function(n,mean,sd,show.properties) {
                X <- rnorm(n,mean,sd); attr(X,"trc") <- 1.23; X }
        expect_error( rvgt.range.erc(
          rdist=myrnorm, dist.params=dp, gen.time=emgt),  msg)
})

test_that("[erc-i02] calling rvgt.range.erc with invalid arguments: gen.time", {
        ## gen.time

        myrnorm <- function(n,mean,sd,show.properties) {
                X <- rnorm(n,mean,sd);
                attr(X,"trc") <- 1.23; attr(X,"erc") <- 1.23;
                X }

        msg <- "argument \"gen.time\" is missing, with no default"
        expect_error( rvgt.range.erc(
          rdist=myrnorm, dist.params=list(mean=1:2,sd=1:2)), msg)

        msg <- "Argument 'gen.time' must be of class \"rvgt.range.time\"."
        expect_error( rvgt.range.erc(
          rdist=myrnorm, dist.params=list(mean=1:2,sd=1:2), gen.time=1), msg)

})

## --------------------------------------------------------------------------
