## --------------------------------------------------------------------------
##
## Check function rvgt.range.trc()
##
## --------------------------------------------------------------------------

context("[rvgt.range.trc] - rejection constant")

## --------------------------------------------------------------------------

test_that("[trc-001] calling rvgt.range.trc", {
    rtest <- function(n,a,b,show.properties) {
        if (isTRUE(all.equal(c(a,b),c(1,2)))) stop()
        X <- 1
        attr(X,"trc") <- a+b
        X
    }
    
    trc <- rvgt.range.trc(rdist = rtest,
                          dist.params = list(a=1:2,b=1:2),
                          verbose = FALSE)
    
    ## plot(mgt, xscale="lin", yscale="lin", main="[trc-001]")
    
    expect_equal(trc$data[1,1], 2)
    expect_identical(trc$data[1,2], NA_real_)
    expect_equal(trc$data[2,1], 3)
    expect_equal(trc$data[2,2], 4)
})

test_that("[trc-002] calling rvgt.range.trc", {

    rtest <- function(n,a,b,show.properties) {
        if (isTRUE(all.equal(c(a,b),c(1,2)))) stop()
        X <- 1
        attr(X,"trc") <- a+b
        X
    }

    ## create object of class "rvgt.range.time"
    emgt <- rvgt.range.engine(rdist = rtest,
                              dist.params = list(a=1:2,b=1:2),
                              test.routine=function(emgt, ...) { emgt },
                              test.class="time.marginal",
                              gen.time=12345
                              )

    trc <- rvgt.range.trc(gen.data = emgt,verbose = FALSE)

    ## plot(mgt, xscale="lin", yscale="lin", main="[trc-001]")

    expect_equal(trc$data[1,1], 2)
    expect_identical(trc$data[1,2], NA_real_)
    expect_equal(trc$data[2,1], 3)
    expect_equal(trc$data[2,2], 4)
})


## --------------------------------------------------------------------------

context("[rvgt.range.trc] - Invalid arguments")

## --------------------------------------------------------------------------

test_that("[trc-i01] calling rvgt.range.trc with invalid arguments: show.properties", {
        ## show.properties
        msg <- "'rdist' must accept argument 'show.properties'."
        expect_error( rvgt.range.trc(
          rdist=rnorm, dist.params=list(mean=1:2,sd=1:2)), msg)

        msg <- "returned value of 'rdist' must have attribute 'trc'."
        myrnorm <- function(n,mean,sd,show.properties) { rnorm(n,mean,sd) }
        expect_error( rvgt.range.trc(
          rdist=myrnorm, dist.params=list(mean=1,sd=1)),  msg)
})

## --------------------------------------------------------------------------

