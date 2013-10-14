## --------------------------------------------------------------------------
##
## Check function plot.rvgt.range()
##
## --------------------------------------------------------------------------

trunit2 <- function(rdist,dist.params,...) { prod(as.numeric(dist.params))^2 }

## --------------------------------------------------------------------------

context("[plot.rvgt.range] - Plot test results")

## --------------------------------------------------------------------------

test_that("[prr-101] calling plot.rvgt.range (dim=1)", {
        dp <- list(alpha=1:100)
        res <- rvgt.range.engine(rdist=rnorm,
                                 dist.params=dp,
                                 test.routine=trunit2,
                                 test.class="unittest"
                                 )
        plot(res, main="[prr-101] A")
        plot(res, sub.params=list(alpha=c(30L:50L)), main="[prr-101] B")
        plot(res, sub.params=list(alpha=c(40,70)), main="[prr-101] C")

        plot(res, xscale="log", main="[prr-101] D")
        plot(res, yscale="log", main="[prr-101] E")
        plot(res, xscale="log", yscale="log", main="[prr-101] F")
})
        
test_that("[prr-102] calling plot.rvgt.range (dim=2)", {
        dp <- list(alpha=1:10, beta=2:5)
        res <- rvgt.range.engine(rdist=rnorm,
                                 dist.params=dp,
                                 test.routine=trunit2,
                                 test.class="unittest"
                                 )
        plot(res, main="[prr-102] A")
        plot(res, sub.params=list(alpha=c(2,5)), main="[prr-102] B")

        plot(res, xscale="log", main="[prr-102] C")
        plot(res, yscale="log", main="[prr-102] D")
        plot(res, xscale="log", yscale="log", main="[prr-102] E")

        plot(res, zscale="log", main="[prr-102] F")
})
        
test_that("[prr-103] calling plot.rvgt.range (dim=3)", {
        dp <- list(alpha=c(1,2),beta=c(3),gamma=c(11,13,17,19))
        res <- rvgt.range.engine(rdist=rnorm,
                                 dist.params=dp,
                                 test.routine=trunit2,
                                 test.class="unittest"
                                 )
        plot(res, main="[prr-103] A")
})
        
test_that("[prr-104] calling plot.rvgt.range (dim=3)", {
        dp <- list(alpha=1:10, beta=1:10, gamma=1:10)
        res <- rvgt.range.engine(rdist=rnorm,
                                 dist.params=dp,
                                 test.routine=trunit2,
                                 test.class="unittest"
                                 )
        plot(res, sub.params=list(beta=3L), main="[prr-104] A")
        plot(res, sub.params=list(gamma=c(1.5,2.5)), main="[prr-104] B")

        plot(res, sub.params=list(alpha=5L,gamma=c(1.5,2.5)), main="[prr-104] C")
})
        
## --------------------------------------------------------------------------

context("[plot.rvgt.range] - Invalid arguments")

## --------------------------------------------------------------------------

test_that("[prr-i01] calling plot.rvgt.range with invalid arguments", {
        dp <- list(alpha=c(1,2),beta=c(3,5,7),gamma=c(11,13,17,19))
        res <- rvgt.range.engine(rdist=rnorm,
                                 dist.params=dp,
                                 test.routine=trunit2,
                                 test.class="unittest"
                                 )

        msg <- "plot.rvgt.range\\(\\) handles arrays of rank 1 or 2 only."
        expect_error(plot(res),  msg)
        expect_error(plot(res, sub.params=list(alpha=1L,beta=3L,gamma=2L)),  msg)

        msg <- "'arg' must be NULL or a character vector"
        expect_error(plot(res, sub.params=list(alpha=1L), xscale=1),  msg)

        msg <- "'arg' should be one of \"linear\", \"logarithmic\""
        expect_error(plot(res, sub.params=list(alpha=1L), xscale="nix"),  msg)
        expect_error(plot(res, sub.params=list(alpha=1L), yscale="nix"),  msg)
        expect_error(plot(res, sub.params=list(alpha=1L), zscale="nix"),  msg)
})

## Other tests are implicitly included in test_get.subrange.R.

## --------------------------------------------------------------------------

remove(trunit2)

## --------------------------------------------------------------------------
