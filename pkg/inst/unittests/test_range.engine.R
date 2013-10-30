## --------------------------------------------------------------------------
##
## Check function rvgt.range.engine()
##
## --------------------------------------------------------------------------

trunit <- function(rdist,dist.params,...) { prod(as.numeric(dist.params)) }
tremgt <- function(emgt,...) { emgt }

trwait <- function(rdist,dist.params,...) {
        if (dist.params[[1]] > 9 && dist.params[[1]] < 100) {
                wait.while.process(dist.params[[1]]) }
        return (dist.params[[1]]^2)
}

## --------------------------------------------------------------------------

context("[rvgt.range.engine] - List entries")

## --------------------------------------------------------------------------

test_that("[pre-001] list entries returned by rvgt.range.engine: length(dist.params)=1", {
        dp <- list(alpha=c(1,2))
        res <- rvgt.range.engine(rdist=rnorm,
                                 dist.params=dp,
                                 test.routine=trunit,
                                 test.class="unittest",
                                 test.name="run unit test"
                                 )
        expect_is(res, "rvgt.range.unittest")
        expect_is(res, "rvgt.range")

        expect_output(summary(res),
                      paste("^ \\* Test ranges of parameters - Summary:\\s+",
                            "Test:\\s+unittest\\s+",
                            "run unit test\\s+",
                            "RVG:\\s+rnorm\\s+",
                            "Parameters:\\s+alpha \\( 2 \\)\\[1\\] 1 2\\s+",
                            "Results:\\s+",
                            "Min. 1st Qu.  Median    Mean 3rd Qu.    Max.\\s+",
                            "1\\.00\\s+1\\.25\\s+1\\.50\\s+1\\.50\\s+1\\.75\\s+2\\.00\\s*", 
                            "Tests started at.*",
                            "Total runtime:.*$",
                            sep=""))
        
        p <- res$dist.params
        expect_identical(p, dp)

        r <- res$data
        expect_identical(length(r), 2L)
        expect_identical(dim(r), 2L)
        expect_identical(dimnames(r), lapply(dp,as.character))

        re <- c(1,2)
        dim(re) <- 2L
        dimnames(re) <- dp
        expect_identical(r, re)
})

if (.Platform$OS.type == "unix") {
test_that("[pre-001mc1] list entries returned by rvgt.range.engine: length(dist.params)=1", {
        dp <- list(alpha=101:110)
        res0 <- rvgt.range.engine(rdist=rnorm,
                                  dist.params=dp,
                                  test.routine=trunit,
                                  test.class="unittest"
                                  )
        ## remove volatile parts
        res0$started <- NA
        res0$runtime <- NA

        res1 <- rvgt.range.engine(rdist=rnorm,
                                  dist.params=dp,
                                  test.routine=trunit,
                                  test.class="unittest",
                                  ncores=1L
                                  )
        res1$started <- NA
        res1$runtime <- NA
        expect_identical(res0, res1)

        res2 <- rvgt.range.engine(rdist=rnorm,
                                  dist.params=dp,
                                  test.routine=trunit,
                                  test.class="unittest",
                                  ncores=2L
                                  )
        res2$started <- NA
        res2$runtime <- NA
        expect_identical(res0, res2)
})
}

if (.Platform$OS.type == "unix") {
test_that("[pre-001mc1to] list entries returned by rvgt.range.engine: timeout", {
        dp <- list(s=c(2,10,11,101))
        timeout.val = 12345

        expect_that(res1 <- rvgt.range.engine(rdist=rnorm,
                                              dist.params=dp,
                                              test.routine=trwait,
                                              test.class="unittest",
                                              ncores=1L, timeout=0.02, timeout.val=timeout.val
                                              ),
                    takes_less_than(1))
        
        r <- res1$data
        re <- c(4,timeout.val,timeout.val,10201)
        dim(re) <- 4L
        dimnames(re) <- dp
        expect_identical(r, re)

        expect_that(res2 <- rvgt.range.engine(rdist=rnorm,
                                              dist.params=dp,
                                              test.routine=trwait,
                                              test.class="unittest",
                                              ncores=2L, timeout=0.02, timeout.val=timeout.val
                                              ),
                    takes_less_than(1))
        
        r <- res2$data
        re <- c(4,timeout.val,timeout.val,10201)
        dim(re) <- 4L
        dimnames(re) <- dp
        expect_identical(r, re)
        ## remark:
        ## when the computer is very busy then r[1]==timeout.val (instead of 4)
})
}

if (.Platform$OS.type == "unix") {
test_that("[pre-001mc1tonocores] list entries returned by rvgt.range.engine: timeout", {
        dp <- list(s=c(2,10,11,101))
        timeout.val = 12345

        msg <- "Timeout requires multicore support. Argument 'ncores' set to 1L."
        expect_message(rvgt.range.engine(rdist=rnorm,
                                         dist.params=dp,
                                         test.routine=trwait,
                                         test.class="unittest",
                                         timeout=0.02, timeout.val=timeout.val
                                         ),  msg)

        expect_that(res1 <- rvgt.range.engine(rdist=rnorm,
                                              dist.params=dp,
                                              test.routine=trwait,
                                              test.class="unittest",
                                              timeout=0.02, timeout.val=timeout.val
                                              ),
                    takes_less_than(1))
        
        r <- res1$data
        re <- c(4,timeout.val,timeout.val,10201)
        dim(re) <- 4L
        dimnames(re) <- dp
        expect_identical(r, re)

        expect_that(res2 <- rvgt.range.engine(rdist=rnorm,
                                              dist.params=dp,
                                              test.routine=trwait,
                                              test.class="unittest",
                                              ncores=2L, timeout=0.02, timeout.val=timeout.val
                                              ),
                    takes_less_than(1))
        
        r <- res2$data
        re <- c(4,timeout.val,timeout.val,10201)
        dim(re) <- 4L
        dimnames(re) <- dp
        expect_identical(r, re)
        ## remark:
        ## when the computer is very busy then r[1]==timeout.val (instead of 4)
})
}

test_that("[pre-002] list entries returned by rvgt.range.engine: length(dist.params)=2", {
        dp <- list(alpha=c(2,3),beta=c(5,7,11))
        res <- rvgt.range.engine(rdist=rnorm,
                                 dist.params=dp,
                                 r.params=list(gamma=99),
                                 test.routine=trunit,
                                 test.class="unit.test",
                                 test.name="Unit Test"
                                 )
        expect_is(res, "rvgt.range.unit.test")
        expect_is(res, "rvgt.range.unit")
        expect_is(res, "rvgt.range")

        expect_output(summary(res),
                      paste("^ \\* Test ranges of parameters - Summary:\\s+",
                            "Test:\\s+unit\\.test\\s+Unit Test\\s+",
                            "RVG:\\s+rnorm\\s+",
                            "Parameters:\\s+alpha \\( 2 \\)\\[1\\] 2 3\\s+",
                            "beta \\( 3 \\)\\[1\\]\\s+5\\s+7\\s+11\\s+",
                            "Additional Parameters:\\s+gamma\\s+\\[1\\]\\s+99\\s+",
                            "Results:\\s+",
                            "Min. 1st Qu.  Median    Mean 3rd Qu.    Max.\\s+",
                            "10\\.00\\s+14\\.25\\s+18\\.00\\s+19\\.17\\s+21\\.75\\s+33.00\\s*",
                            "Tests started at.*",
                            "Total runtime:.*$",
                            sep=""))
        
        p <- res$dist.params
        expect_identical(p, dp)

        r <- res$data
        expect_identical(length(r), 6L)
        expect_identical(dim(r), c(2L,3L))
        expect_identical(dimnames(r), lapply(dp,as.character))

        re <- c(2,3) %o% c(5,7,11)
        dim(re) <- c(2L,3L)
        dimnames(re) <- dp
        expect_identical(r, re)
})

if (.Platform$OS.type == "unix") {
test_that("[pre-002mc1] list entries returned by rvgt.range.engine: length(dist.params)=2", {
        dp <- list(alpha=c(2,3),beta=c(5,7,11))
        res0 <- rvgt.range.engine(rdist=rnorm,
                                  dist.params=dp,
                                  test.routine=trunit,
                                  test.class="unittest"
                                  )
        ## remove volatile parts
        res0$started <- NA
        res0$runtime <- NA

        res1 <- rvgt.range.engine(rdist=rnorm,
                                  dist.params=dp,
                                  test.routine=trunit,
                                  test.class="unittest",
                                  ncores=1L
                                  )
        res1$started <- NA
        res1$runtime <- NA
        expect_identical(res0, res1)

        res2 <- rvgt.range.engine(rdist=rnorm,
                                  dist.params=dp,
                                  test.routine=trunit,
                                  test.class="unittest",
                                  ncores=2L
                                  )
        res2$started <- NA
        res2$runtime <- NA
        expect_identical(res0, res2)
})
}

test_that("[pre-003] list entries returned by rvgt.range.engine: length(dist.params)=3", {
        dp <- list(alpha=c(2,3),beta=c(5,7,11),gamma=c(13,17,19,23))
        res <- rvgt.range.engine(rdist=rnorm,
                                 dist.params=dp,
                                 test.routine=trunit,
                                 test.class="unit.test.a"
                                 )
        expect_is(res, "rvgt.range.unit.test.a")
        expect_is(res, "rvgt.range.unit.test")
        expect_is(res, "rvgt.range.unit")
        expect_is(res, "rvgt.range")

        expect_output(summary(res),
                      paste("^ \\* Test ranges of parameters - Summary:\\s+",
                            "Test:\\s+unit\\.test\\.a\\s+",
                            "RVG:\\s+rnorm\\s+",
                            "Parameters:\\s+alpha \\( 2 \\)\\[1\\] 2 3\\s+",
                            "beta \\( 3 \\)\\[1\\]\\s+5\\s+7\\s+11\\s+",
                            "gamma \\( 4 \\)\\[1\\]\\s+13\\s+17\\s+19\\s+23\\s+",
                            "Results:\\s+",
                            "Min. 1st Qu.  Median    Mean 3rd Qu.    Max.\\s+",
                            "130\\.0\\s+236\\.0\\s+304\\.0\\s+345\\.0\\s+420\\.8\\s+759\\.0\\s*",
                            "Tests started at.*",
                            "Total runtime:.*$",
                            sep=""))
        
        p <- res$dist.params
        expect_identical(p, dp)

        r <- res$data
        expect_identical(length(r), 24L)
        expect_identical(dim(r), c(2L,3L,4L))
        expect_identical(dimnames(r), lapply(dp,as.character))

        re <- c(2,3) %o% c(5,7,11) %o% c(13,17,19,23)
        dim(re) <- c(2L,3L,4L)
        dimnames(re) <- dp
        expect_identical(r, re)
})

if (.Platform$OS.type == "unix") {
test_that("[pre-003mc1] list entries returned by rvgt.range.engine: length(dist.params)=3", {
        dp <- list(alpha=c(2,3),beta=c(5,7,11),gamma=c(13,17,19,23))
        res0 <- rvgt.range.engine(rdist=rnorm,
                                  dist.params=dp,
                                  test.routine=trunit,
                                  test.class="unittest"
                                  )
        ## remove volatile parts
        res0$started <- NA
        res0$runtime <- NA

        res1 <- rvgt.range.engine(rdist=rnorm,
                                  dist.params=dp,
                                  test.routine=trunit,
                                  test.class="unittest",
                                  ncores=1L
                                  )
        res1$started <- NA
        res1$runtime <- NA
        expect_identical(res0, res1)

        res2 <- rvgt.range.engine(rdist=rnorm,
                                  dist.params=dp,
                                  test.routine=trunit,
                                  test.class="unittest",
                                  ncores=2L
                                  )
        res2$started <- NA
        res2$runtime <- NA
        expect_identical(res0, res2)
})
}

## --------------------------------------------------------------------------

context("[rvgt.range.engine] - expected marginal generation times")

## --------------------------------------------------------------------------

test_that("[pre-101] emgt is default", {
        dp <- list(alpha=c(1,2),beta=c(3,4,5))
        res <- rvgt.range.engine(rdist=rnorm,
                                 dist.params=dp,
                                 test.routine=tremgt,
                                 test.class="testemgt",
                                 duration=12345
                                 )
        r <- res$data
        re <- rep(12345,6)
        dim(re) <- c(2L,3L)
        dimnames(re) <- dp
        expect_identical(r, re)

})

test_that("[pre-102] emgt is numeric", {
        dp <- list(alpha=c(1,2),beta=c(3,4,5))
        res <- rvgt.range.engine(rdist=rnorm,
                                 dist.params=dp,
                                 test.routine=tremgt,
                                 test.class="testemgt",
                                 gen.time=12345
                                 )
        r <- res$data
        re <- rep(12345,6)
        dim(re) <- c(2L,3L)
        dimnames(re) <- dp
        expect_identical(r, re)

})

test_that("[pre-103] emgt is object of class 'rvgt.range.mgt'", {
        dp <- list(mean=c(1,2),sd=c(3,4,5))

        ## create object of class "rvgt.range.time"
        emgt <- rvgt.range.engine(rdist=rnorm,
                                 dist.params=dp,
                                 test.routine=tremgt,
                                 test.class="time.marginal",
                                 gen.time=12345
                                 )

        ## now rerun with 'emgt' as expected generation times
        res <- rvgt.range.engine(rdist=rnorm,
                                 dist.params=dp,
                                 test.routine=tremgt,
                                 test.class="testemgt",
                                 gen.time=emgt
                                 )
        r <- res$data
        re <- rep(12345,6)
        dim(re) <- c(2L,3L)
        dimnames(re) <- dp
        expect_identical(r, re)

})

## --------------------------------------------------------------------------

context("[rvgt.range.engine] - Invalid arguments")

## --------------------------------------------------------------------------

test_that("[pre-i01] calling rvgt.range.engine with invalid arguments: rdist", {
        ## RVG 'rdist'
        msg <- "RVG 'rdist' is missing or invalid"
        expect_error(rvgt.range.engine(
          dist.params=list(a=1:2)), msg)
        expect_error(rvgt.range.engine(
          rdist=1:2, dist.params=list(a=1:2)),  msg)
})

test_that("[pre-i02] calling rvgt.range.engine with invalid arguments: dist.params", {
        ## parameters for distribution
        msg <- "Argument 'dist.params' missing or invalid"
        expect_error(rvgt.range.engine(
          rdist=rnorm),  msg)
        expect_error(rvgt.range.engine(
          rdist=rnorm, dist.params=c(1:2,3:4)),  msg)
        expect_error(rvgt.range.engine(
          rdist=rnorm, dist.params=list()),  msg)

        msg <- "List entries in 'dist.params' must have names"
        expect_error(rvgt.range.engine(
          rdist=rnorm, dist.params=list(1:2,3:4)),  msg)
        expect_error(rvgt.range.engine(
          rdist=rnorm, dist.params=list(a=1:2,3:4)),  msg)
        expect_error(rvgt.range.engine(
          rdist=rnorm, dist.params=list(1:2,b=3:4)),  msg)

        msg <- "List entries in 'dist.params' must be sorted"
        expect_error(rvgt.range.engine(
          rdist=rnorm, dist.params=list(a=c(3,2,1))),  msg)
})

test_that("[pre-i03] calling rvgt.range.engine with invalid arguments: r.params", {
        ## r.params
        msg <- "Argument 'r.params' invalid"
        expect_error(rvgt.range.engine(
          rdist=rnorm, dist.params=list(a=1:2,b=3:4), r.params=1),  msg)
})

test_that("[pre-i04] calling rvgt.range.engine with invalid arguments: gen.time", {
        ## gen.time
        msg <- "Argument 'gen.time' invalid"
        expect_error(rvgt.range.engine(
          rdist=rnorm, dist.params=list(a=1:2,b=3:4), gen.time="a",
          test.routine=trunit, test.class="test"),  msg)
        expect_error(rvgt.range.engine(
          rdist=rnorm, dist.params=list(a=1:2,b=3:4), gen.time=c(1,2),
          test.routine=trunit, test.class="test"),  msg)
        expect_error(rvgt.range.engine(
          rdist=rnorm, dist.params=list(a=1:2,b=3:4), gen.time=0,
          test.routine=trunit, test.class="test"),  msg)

        msg <- "Object 'gen.time' contains incompatible element 'dist.params'"

        ## create object of class "rvgt.range.time"
        dp0 <- list(mean=c(1,2),sd=c(3,4,5))
        emgt <- rvgt.range.engine(
          rdist=rnorm, dist.params=dp0, test.routine=tremgt,
          test.class="time.marginal", gen.time=12345 )

        dp1 <- list(mean=c(1,2,11),sd=c(3,4,5))
        expect_error(rvgt.range.engine(
          rdist=rnorm, dist.params=dp1, test.routine=tremgt,
          test.class="dummy", gen.time=emgt),  msg)

        dp2 <- list(mu=c(1,2),sd=c(3,4,5))
        expect_error(rvgt.range.engine(
          rdist=rnorm, dist.params=dp2, test.routine=tremgt,
          test.class="dummy", gen.time=emgt),  msg)

        dp3 <- list(sd=c(3,4,5),mean=c(1,2))
        expect_error(rvgt.range.engine(
          rdist=rnorm, dist.params=dp3, test.routine=tremgt,
          test.class="dummy", gen.time=emgt),  msg)
})

test_that("[pre-i05] calling rvgt.range.engine with invalid arguments: test.routine", {
        ## test.routine
        msg <- "Argument 'test.routine' is missing or invalid"
        expect_error(rvgt.range.engine(
          rdist=rnorm, dist.params=list(a=1), test.class="a"),  msg)
        expect_error(rvgt.range.engine(
          rdist=rnorm, dist.params=list(a=1), test.routine=1,test.class="a"),  msg)
})

test_that("[pre-i06] calling rvgt.range.engine with invalid arguments: test.class", {
        ## test.class
        msg <- "Argument 'test.class' is missing or invalid"
        expect_error(rvgt.range.engine(
          rdist=rnorm, dist.params=list(a=1), test.routine=trunit),  msg)
        expect_error(rvgt.range.engine(
          rdist=rnorm, dist.params=list(a=1), test.routine=trunit, test.class=1),  msg)
})

test_that("[pre-i07] calling rvgt.range.engine with invalid arguments: test.name", {
        ## test.name
        msg <- "Argument 'test.name' invalid"
        expect_error(rvgt.range.engine(
          rdist=rnorm, dist.params=list(a=1), test.routine=trunit, test.class="dummy", test.name=1),  msg)
})

test_that("[pre-i08] calling rvgt.range.engine with invalid arguments: test.params", {
        ## test.params
        msg <- "Argument 'test.params' invalid"
        expect_error(rvgt.range.engine(
          rdist=rnorm, dist.params=list(a=1:2,b=3:4),
          test.routine=trunit, test.class="a", test.params=1),  msg)
})

test_that("[pre-i08] calling rvgt.range.engine with invalid arguments: duration", {
        ## duration
        msg <- "Argument 'duration' invalid"
        expect_error(rvgt.range.engine(
          rdist=rnorm, dist.params=list(a=1:2,b=3:4),
          test.routine=trunit, test.class="a", duration="a"),  msg)
        expect_error(rvgt.range.engine(
          rdist=rnorm, dist.params=list(a=1:2,b=3:4),
          test.routine=trunit, test.class="a", duration=c(1,2)),  msg)
        expect_error(rvgt.range.engine(
          rdist=rnorm, dist.params=list(a=1:2,b=3:4),
          test.routine=trunit, test.class="a", duration=-1),  msg)
})

if (.Platform$OS.type == "unix") {
test_that("[pre-i10] calling rvgt.range.engine with invalid arguments: ncores", {
        ## ncores
        msg <- "Argument 'ncores' must be non-negative."
        expect_error(rvgt.range.engine(
          rdist=rnorm, dist.params=list(a=1:2,b=3:4),
          test.routine=trunit, test.class="a", ncores=-1L),  msg)
})
}

if (.Platform$OS.type != "unix") {
test_that("[pre-i11] calling rvgt.range.engine with invalid arguments: ncores", {
        ## ncores
        msg <- "Multicore supported is not available on this plattform."
        expect_error(rvgt.range.engine(
          rdist=rnorm, dist.params=list(a=1:2,b=3:4),
          test.routine=trunit, test.class="a", ncores=1L),  msg)
})
}

test_that("[pre-i12] calling rvgt.range.engine with invalid arguments: timeout", {
        ## timeout
        msg <- "Argument 'timeout' invalid."
        expect_error(rvgt.range.engine(
          rdist=rnorm, dist.params=list(a=1:2,b=3:4),
          test.routine=trunit, test.class="a", ncores=1L, timeout="a"),  msg)
        expect_error(rvgt.range.engine(
          rdist=rnorm, dist.params=list(a=1:2,b=3:4),
          test.routine=trunit, test.class="a", ncores=1L, timeout=-1),  msg)
})

if (.Platform$OS.type != "unix") {
test_that("[pre-i13] calling rvgt.range.engine with invalid arguments: timeout", {
        ## ncores
        msg <- "Timeout is not supported on this plattform."
        expect_error(rvgt.range.engine(
          rdist=rnorm, dist.params=list(a=1:2,b=3:4),
          test.routine=trunit, test.class="a", ncores=1L, timeout=1),  msg)
})
}

test_that("[pre-i14] calling rvgt.range.engine with invalid arguments: verbose", {
        ## verbose
        msg <- "Argument 'verbose' invalid"
        expect_error(rvgt.range.engine(
          rdist=rnorm, dist.params=list(a=1:2,b=3:4),
          test.routine=trunit, test.class="a", verbose=1),  msg)
})

if (.Platform$OS.type == "unix") {
test_that("[pre-i15] calling rvgt.range.engine with invalid arguments: verbose", {
        ## verbose
        msg <- "Argument 'verbose' ignored when 'ncores' > 1"
        expect_message(rvgt.range.engine(
          rdist=rnorm, dist.params=list(a=1:2,b=3:4),
          test.routine=trunit, test.class="a", ncores=2L, verbose=TRUE),  msg)
})
}

## --------------------------------------------------------------------------

remove(trunit)

## --------------------------------------------------------------------------
