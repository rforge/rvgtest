## --------------------------------------------------------------------------
##
## Check function rvgt.range.setup()
##
## --------------------------------------------------------------------------

context("[rvgt.range.setup] - setup time")

## --------------------------------------------------------------------------

test_that("[sut-101] calling rvgt.range.setup: argument 'rep'", {
        rtest <- function(n,a,b,rep) {
                if (isTRUE(all.equal(c(a,b),c(1,2)))) stop()
                Sys.sleep(0.01)
        }

        sut <- rvgt.range.setup(rdist = rtest,
                              dist.params = list(a=1:2,b=1:2),
                              duration = 0.01, verbose = FALSE)

        ## plot(sut, xscale="lin", yscale="lin", main="[sut-101]")

        expect_identical(sut$data[1,2], NA_real_)
        expect_true(sut$data[1,1]>0.001 && sut$data[1,1]<0.1)
        expect_true(sut$data[2,1]>0.001 && sut$data[1,1]<0.1)
        expect_true(sut$data[2,2]>0.001 && sut$data[1,1]<0.1)
})

test_that("[sut-102] calling rvgt.range.setup: no argument 'rep'", {
        rtest <- function(n,a,b) {
                if (isTRUE(all.equal(c(a,b),c(1,2)))) stop()
                Sys.sleep(0.01)
        }

        sut <- rvgt.range.setup(rdist = rtest,
                              dist.params = list(a=1:2,b=1:2),
                              duration = 0.01, verbose = FALSE)

        ## plot(sut, xscale="lin", yscale="lin", main="[sut-101]")

        expect_identical(sut$data[1,2], NA_real_)
        expect_true(sut$data[1,1]>0.001 && sut$data[1,1]<0.1)
        expect_true(sut$data[2,1]>0.001 && sut$data[1,1]<0.1)
        expect_true(sut$data[2,2]>0.001 && sut$data[1,1]<0.1)
})

## --------------------------------------------------------------------------

context("[rvgt.range.setup] - Invalid arguments")

## --------------------------------------------------------------------------

test_that("[rsut-i01] calling rvgt.range.setup with invalid arguments: repetitions", {
       ## repetitions
       msg <- "Argument 'repetitions' invalid"
       expect_error( rvgt.range.setup(
          rdist=rnorm, dist.params=list(mean=1:2,sd=1:2), repetitions="a"), msg)
       expect_error( rvgt.range.setup(
          rdist=rnorm, dist.params=list(mean=1:2,sd=1:2), repetitions=0.99), msg)
       expect_error( rvgt.range.setup(
          rdist=rnorm, dist.params=list(mean=1:2,sd=1:2), repetitions=1:2), msg)
})


## --------------------------------------------------------------------------

