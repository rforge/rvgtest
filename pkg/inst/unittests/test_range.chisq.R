## --------------------------------------------------------------------------
##
## Check function rvgt.range.chisq()
##
## --------------------------------------------------------------------------

context("[rvgt.range.chisq] - chisquare gof test")

## --------------------------------------------------------------------------

test_that("[chisq-001] calling rvgt.range.chisq", {
        dp <- list(shape1=1:3, shape2=2)

        ## marginal generation times
        ##        mgt <- rvgt.range.marginal(rdist = rbeta, dist.params = dp,
        ##                                   duration = 0.01, gen.time = 1e-5)
        ## we use a time saving shortcut:
        mgt <- rvgt.range.engine(rdist=rbeta, dist.params=dp,
                                 test.routine=function(emgt,...){emgt*1e-10},
                                 test.class="time.marginal")

        ## run chisquare gof test with qbeta()
        chisq.q <- rvgt.range.chisq(rdist = rbeta, dist.params = dp,
                                    n = 1e4, breaks = 100, qdist=qbeta,
                                    duration = 1, gen.time = mgt, verbose=FALSE)
        expect_true(all(chisq.q$data > 1e-5) && all(chisq.q$data < 0.99999)) 

        ## run chisquare gof test with pbeta()
        chisq.q <- rvgt.range.chisq(rdist = rbeta, dist.params = dp,
                                    n = 1e4, breaks = 100, pdist=pbeta,
                                    duration = 1, gen.time = mgt, verbose=FALSE)
        expect_true(all(chisq.q$data > 1e-5) && all(chisq.q$data < 0.99999)) 
})

test_that("[chisq-002] calling rvgt.range.chisq", {
        dp <- list(mean=1:10)

        ## marginal generation times
        ## we use a time saving shortcut:
        mgt <- rvgt.range.engine(rdist=rnorm, dist.params=dp,
                                 test.routine=function(emgt,...){emgt*1e-10},
                                 test.class="time.marginal")
        mgt$data[1] <- NA
        mgt$data[2] <- Inf
        mgt$data[3] <- 1e10

        ## run chisquare gof test with 'qdist'
        myqnorm <- function(p,mean) {
                if (isTRUE(all.equal(mean,4))) {
                        NA
                } else if (isTRUE(all.equal(mean,5))) {
                        NaN
                } else if (isTRUE(all.equal(mean,6))) {
                        Inf
                } else {
                        qnorm(p,mean)
                }
        }
        chisq.q <- rvgt.range.chisq(rdist = rnorm, dist.params = dp,
                                    n = 1e4, breaks = 100, qdist=myqnorm,
                                    duration = 1, gen.time = mgt, verbose=FALSE)

        expect_true(is.na(chisq.q$data[1]))
        expect_true(is.infinite(chisq.q$data[2]) && chisq.q$data[2] > 0)
        expect_true(is.infinite(chisq.q$data[3]) && chisq.q$data[3] > 0)
        expect_true(is.nan(chisq.q$data[4]))
        expect_true(is.nan(chisq.q$data[5]))
        expect_true(is.nan(chisq.q$data[6]))
        expect_true(all(chisq.q$data[7:10] > 1e-5) && all(chisq.q$data[7:10] < 0.99999)) 

        rm(chisq.q)
        
        ## run chisquare gof test with 'pdist'
        mypnorm <- function(q,mean) {
                if (isTRUE(all.equal(mean,4))) {
                        NA
                } else if (isTRUE(all.equal(mean,5))) {
                        NaN
                } else if (isTRUE(all.equal(mean,6))) {
                        Inf
                } else {
                        pnorm(q,mean)
                }
        }
        chisq.p <- rvgt.range.chisq(rdist = rnorm, dist.params = dp,
                                    n = 1e4, breaks = 100, pdist=mypnorm,
                                    duration = 1, gen.time = mgt, verbose=FALSE)

        expect_true(is.na(chisq.p$data[1]))
        expect_true(is.infinite(chisq.p$data[2]) && chisq.p$data[2] > 0)
        expect_true(is.infinite(chisq.p$data[3]) && chisq.p$data[3] > 0)
        expect_true(is.nan(chisq.p$data[4]))
        expect_true(is.nan(chisq.p$data[5]))
        expect_true(is.nan(chisq.p$data[6]))
        expect_true(all(chisq.p$data[7:10] > 1e-5) && all(chisq.p$data[7:10] < 0.99999)) 

})

## --------------------------------------------------------------------------

context("[rvgt.range.chisq] - Invalid arguments")

## --------------------------------------------------------------------------

## create object of class "rvgt.range.time"
dp <- list(mean=c(1,2),sd=c(3,4))
mgt <- rvgt.range.engine(rdist=rnorm, dist.params=dp,
                         test.routine=function(emgt,...){emgt*1e-6},
                         test.class="time.marginal")

test_that("[chisq-i01] calling rvgt.range.erc with invalid arguments: n", {
        ## n

        msg <- "Argument 'n' is missing or invalid"
        expect_error( rvgt.range.chisq(
          rdist=rnorm, dist.params=dp),  msg)
        expect_error( rvgt.range.chisq(
          rdist=rnorm, dist.params=dp, n=1),  msg)
        expect_error( rvgt.range.chisq(
          rdist=rnorm, dist.params=dp, n="a"),  msg)
})

test_that("[chisq-i02] calling rvgt.range.erc with invalid arguments: breaks", {
        ## breaks

        msg <- "Argument 'breaks' is missing or invalid"
        expect_error( rvgt.range.chisq(
          rdist=rnorm, dist.params=dp, n=1000),  msg)
        expect_error( rvgt.range.chisq(
          rdist=rnorm, dist.params=dp, n=1000, breaks=0),  msg)
        expect_error( rvgt.range.chisq(
          rdist=rnorm, dist.params=dp, n=1000, breaks="a"),  msg)
})

test_that("[chisq-i03] calling rvgt.range.erc with invalid arguments: gen.time", {
        ## gen.time

        msg <- "Argument 'gen.time' is missing"
        expect_error( rvgt.range.chisq(
          rdist=rnorm, dist.params=dp, n=1000, breaks=10),  msg)

        msg <- "Argument 'gen.time' must be of class \"rvgt.range.time\"."
        expect_error( rvgt.range.chisq(
          rdist=rnorm, dist.params=dp, gen.time=1, n=1000, breaks=10),  msg)

})

test_that("[chisq-i04] calling rvgt.range.erc with invalid arguments: qdist, pdist", {
        ## qdist and pdist

        msg <- "Argument 'qdist' or 'pdist' required."
        expect_error( rvgt.range.chisq(
          rdist=rnorm, dist.params=dp, gen.time=mgt, n=1000, breaks=10),  msg)

        msg <- "Argument 'pdist' invalid."
        expect_error( rvgt.range.chisq(
          rdist=rnorm, dist.params=dp, gen.time=mgt, n=1000, breaks=10,
          pdist=1),  msg)

        msg <- "Argument 'qdist' invalid."
        expect_error( rvgt.range.chisq(
          rdist=rnorm, dist.params=dp, gen.time=mgt, n=1000, breaks=10,
          qdist=1),  msg)
})

## --------------------------------------------------------------------------
