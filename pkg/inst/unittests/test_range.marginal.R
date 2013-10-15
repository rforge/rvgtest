## --------------------------------------------------------------------------
##
## Check function rvgt.range.marginal()
##
## --------------------------------------------------------------------------

context("[rvgt.range.marginal] - marginal generation times")

## --------------------------------------------------------------------------

## The following test is rather expensive.
## Rprof shows that this is mainly due to gc().
#test_that("[mgt-101] calling rvgt.range.mgt", {
#        mgt <- rvgt.range.marginal(rdist = rbeta,
#                                   dist.params
#                                   = list(shape1=c(0.01,0.1,1,10,100),shape2=c(0.01,0.1,1,10,100)),
#                                   duration = 0.01, gen.time=1e-6, verbose=FALSE)
#
#        summary(mgt)
#        plot(mgt, xscale="log", yscale="log", main="[mgt-101]")
#})

test_that("[mgt-102] calling rvgt.range.marginal", {
        rtest <- function(n,a,b,show.properties) {
                if (isTRUE(all.equal(c(a,b),c(1,2)))) stop()
                Sys.sleep(0.01)
        }

        mgt <- rvgt.range.marginal(rdist = rtest,
                                   dist.params = list(a=1:2,b=1:2),
                                   duration = 0.01, verbose = FALSE)

        ## plot(mgt, xscale="lin", yscale="lin", main="[mgt-102]")

        expect_identical(mgt$data[1,2], NA_real_)
        expect_true(mgt$data[1,1]>0.001 && mgt$data[1,1]<0.1)
        expect_true(mgt$data[2,1]>0.001 && mgt$data[1,1]<0.1)
        expect_true(mgt$data[2,2]>0.001 && mgt$data[1,1]<0.1)

})


## --------------------------------------------------------------------------

context("[rvgt.range.marginal] - Invalid arguments")

## --------------------------------------------------------------------------

test_that("[mgt-i01] calling rvgt.range.marginal with invalid arguments: el.time", {
        ## el.time
        msg <- "Argument 'el.time' invalid"
        expect_error( rvgt.range.marginal(
          rdist=rnorm, dist.params=list(mean=1:2,sd=1:2), el.time="a"), msg)
        expect_error( rvgt.range.marginal(
          rdist=rnorm, dist.params=list(mean=1:2,sd=1:2), el.time=1:2), msg)
        expect_error( rvgt.range.marginal(
          rdist=rnorm, dist.params=list(mean=1:2,sd=1:2), el.time=-1), msg)

        msg <- "Argument 'el.time' ignored \\('rdist' requires parameter 'show.properties'\\)"
        myrnorm <- function(n,mean,sd) { Sys.sleep(0.01); rnorm(n,mean,sd) }
        expect_warning( rvgt.range.marginal(
          rdist=myrnorm, dist.params=list(mean=1,sd=1), duration=0.01, el.time=0.01),  msg)

        msg <- "Argument 'el.time' cannot be used \\(returned value of 'rdist' must have attribute 'trc'\\)"
        myrnorm <- function(n,mean,sd,show.properties) { Sys.sleep(0.01); rnorm(n,mean,sd) }
        expect_error( rvgt.range.marginal(
          rdist=myrnorm, dist.params=list(mean=1,sd=1), duration=0.01, el.time=0.01),  msg)
})

test_that("[mgt-i02] calling rvgt.range.marginal with invalid arguments: repetitions", {
        ## repetitions
        msg <- "Argument 'repetitions' invalid"
        expect_error( rvgt.range.marginal(
          rdist=rnorm, dist.params=list(mean=1:2,sd=1:2), repetitions="a"), msg)
        expect_error( rvgt.range.marginal(
          rdist=rnorm, dist.params=list(mean=1:2,sd=1:2), repetitions=0.99), msg)
        expect_error( rvgt.range.marginal(
          rdist=rnorm, dist.params=list(mean=1:2,sd=1:2), repetitions=1:2), msg)
})


## --------------------------------------------------------------------------

