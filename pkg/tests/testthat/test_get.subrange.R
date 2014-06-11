## --------------------------------------------------------------------------
##
## Check function get.subrange()
##
## --------------------------------------------------------------------------

get.subrange <- rvgtest:::get.subrange

## --------------------------------------------------------------------------

trunit <- function(rdist,dist.params,...) { prod(as.numeric(dist.params)) }

## --------------------------------------------------------------------------

context("[get.subrange] - (internal function)")

## --------------------------------------------------------------------------

test_that("[gsr-101] calling get.subrange", {
        dp <- list(alpha=c(1,2), beta=c(5), gamma=c(11,13,17,19))
        res <- rvgt.range.engine(rdist=rnorm,
                                 dist.params=dp,
                                 test.routine=trunit,
                                 test.class="unittest"
                                 )

        expect_identical(res, get.subrange(res, drop=FALSE))
        expect_identical(res, get.subrange(res, sub.params=list(), drop=FALSE))

        res1 <- res
        res1$data <- res1$data[,,,drop=TRUE]
        res1t <- get.subrange(res)
        expect_identical(res1, res1t)
})

test_that("[gsr-102] calling get.subrange", {
        dp <- list(alpha=c(1,2), beta=c(3,5,7), gamma=c(11,13,17,19))
        res <- rvgt.range.engine(rdist=rnorm,
                                 dist.params=dp,
                                 test.routine=trunit,
                                 test.class="unittest"
                                 )

        expect_identical(res, get.subrange(res))
        expect_identical(res, get.subrange(res, sub.params=list()))
})

test_that("[gsr-103] calling get.subrange", {
        dp <- list(alpha=c(1,2), beta=c(3,5,7), gamma=c(11,13,17,19))
        res <- rvgt.range.engine(rdist=rnorm,
                                 dist.params=dp,
                                 test.routine=trunit,
                                 test.class="unittest"
                                 )
        ## remove volatile parts
        res$started <- NA
        res$runtime <- NA
        
        dp1 <- list(alpha=c(1,2), beta=c(3,7), gamma=c(17))
        res1 <- rvgt.range.engine(rdist=rnorm,
                                  dist.params=dp1,
                                  test.routine=trunit,
                                  test.class="unittest"
                                  )
        res1$started <- NA
        res1$runtime <- NA

        res1ff <- get.subrange(res, sub.params=list(gamma.lim=c(15.0,18.0),beta=c(1L,3L)),
                              drop=FALSE, asdouble=FALSE)
        expect_identical(res1, res1ff)

        res1ft <- get.subrange(res, sub.params=list(gamma.lim=c(15.0,18.0),beta=c(3L,7L)),
                              drop=FALSE) ## asdouble=TRUE
        expect_identical(res1, res1ft)

        res1tf <- get.subrange(res, sub.params=list(gamma.lim=c(15.0,18.0),beta=c(1L,3L)),
                              drop=TRUE, asdouble=FALSE)
        res1$data <- res1$data[,,,drop=TRUE]
        expect_identical(res1, res1tf)

        res1tt <- get.subrange(res, sub.params=list(gamma.lim=c(15.0,18.0),beta=c(3L,7L)),
                              drop=TRUE) ## asdouble=TRUE
        expect_identical(res1, res1tt)
})

test_that("[gsr-104] calling get.subrange", {
        dp <- list(alpha=c(1,2), beta=c(3,5,7), gamma=c(11,13,17,19))
        res <- rvgt.range.engine(rdist=rnorm,
                                 dist.params=dp,
                                 test.routine=trunit,
                                 test.class="unittest"
                                 )
        ## remove volatile parts
        res$started <- NA
        res$runtime <- NA

        dp1 <- list(alpha=c(1), beta=c(3,7), gamma=c(17))
        res1 <- rvgt.range.engine(rdist=rnorm,
                                  dist.params=dp1,
                                  test.routine=trunit,
                                  test.class="unittest"
                                  )
        res1$started <- NA
        res1$runtime <- NA

        res1f <- get.subrange(res, sub.params=list(alpha=1L,gamma.lim=c(15.0,18.0),beta=c(1L,3L)),
                              drop=FALSE, asdouble=FALSE)
        expect_identical(res1, res1f)

        res1t <- get.subrange(res, sub.params=list(alpha=1L,gamma.lim=c(15.0,18.0),beta=c(1L,3L)),
                              drop=TRUE, asdouble=FALSE)
        res1$data <- res1$data[,,,drop=TRUE]
        dim(res1$data) <- 2
        dimnames(res1$data) <- list(beta=c(3,7))
        expect_identical(res1, res1t)
})

test_that("[gsr-114] calling get.subrange", {
        dp <- list(alpha=c(1,2), beta=c(3,5,7))
        rp <- list(gamma=c(11,13,17,19))
        res <- rvgt.range.engine(rdist=rnorm,
                                 dist.params=dp,
                                 r.params=rp,
                                 test.routine=trunit,
                                 test.class="unittest"
                                 )
        ## remove volatile parts
        res$started <- NA
        res$runtime <- NA

        dp1 <- list(alpha=c(1), beta=c(3,7))
        rp1 <- list(gamma=c(17))
        res1 <- rvgt.range.engine(rdist=rnorm,
                                  dist.params=dp1,
                                  r.params=rp1,
                                  test.routine=trunit,
                                  test.class="unittest"
                                  )
        res1$started <- NA
        res1$runtime <- NA

        res1f <- get.subrange(res, sub.params=list(alpha=1L,gamma.lim=c(15.0,18.0),beta=c(1L,3L)),
                              drop=FALSE, asdouble=FALSE)
        expect_identical(res1, res1f)

        res1t <- get.subrange(res, sub.params=list(alpha=1L,gamma.lim=c(15.0,18.0),beta=c(1L,3L)),
                              drop=TRUE, asdouble=FALSE)
        res1$data <- res1$data[,,,drop=TRUE]
        dim(res1$data) <- 2
        dimnames(res1$data) <- list(beta=c(3,7))
        expect_identical(res1, res1t)
})

test_that("[gsr-201] calling get.subrange", {
        dp <- list(alpha=c(1,2), beta=c(3,5,7))
        rp <- list(gamma=c(11,13,17,19))
        res <- rvgt.range.engine(rdist=rnorm,
                                 dist.params=dp,
                                 r.params=rp,
                                 test.routine=trunit,
                                 test.class="unittest"
                                 )
        ## remove volatile parts
        res$started <- NA
        res$runtime <- NA

        dp1 <- list(alpha=c(2), beta=c(3,7))
        rp1 <- list(gamma=c(13,19))
        res1 <- rvgt.range.engine(rdist=rnorm,
                                  dist.params=dp1,
                                  r.params=rp1,
                                  test.routine=trunit,
                                  test.class="unittest"
                                  )
        res1$started <- NA
        res1$runtime <- NA

        res1f <- get.subrange(res, sub.params=list(alpha=c(2),gamma=c(13,19),beta=c(1L,3L)),
                              drop=FALSE, asdouble=FALSE)
        expect_identical(res1, res1f)

        res1t <- get.subrange(res, sub.params=list(alpha=c(2),gamma=c(13,19),beta=c(1L,3L)),
                              drop=TRUE, asdouble=FALSE)
        res1$data <- res1$data[,,,drop=TRUE]
        expect_identical(res1, res1t)
})

## --------------------------------------------------------------------------

context("[get.subrange] - Invalid arguments")

## --------------------------------------------------------------------------

test_that("[gsr-i01] calling get.subrange with invalid arguments: obj", {
        msg <- "Argument 'obj' is missing or invalid."
        expect_error(get.subrange(sub.params=list(a=1)),  msg)
        expect_error(get.subrange(obj=1, sub.params=list(a=1)),  msg)
})

test_that("[gsr-i02] calling get.subrange with invalid arguments: drop, asdouble", {
    dp <- list(alpha=c(1,2), beta=c(5), gamma=c(11,13,17,19))
    res <- rvgt.range.engine(rdist=rnorm,
                             dist.params=dp,
                             test.routine=trunit,
                             test.class="unittest"
                             )

    msg <- "Argument 'drop' invalid."
    expect_error(get.subrange(obj=res, sub.params=list(alpha=1), drop=1),  msg)

    msg <- "Argument 'asdouble' invalid."
    expect_error(get.subrange(obj=res, sub.params=list(alpha=1), asdouble=1),  msg)
})

test_that("[gsr-i03] calling get.subrange with invalid arguments: sub.params", {
        dp <- list(alpha=c(1,2), beta=c(3,5,7), gamma=c(11,13,17,19))
        rp <- list(delta=c(23,29))
        res <- rvgt.range.engine(rdist=rnorm,
                                 dist.params=dp,
                                 r.params=rp,
                                 test.routine=trunit,
                                 test.class="unittest"
                                 )

        msg <- "Argument 'sub.params' is invalid"
        expect_error(get.subrange(res, sub.params=1),  msg)

        msg <- "Parameters in 'sub.params' must have names."
        expect_error(get.subrange(res, sub.params=list(1:2), asdouble=FALSE),  msg)
        expect_error(get.subrange(res, sub.params=list(alpha=1:2, 3:4), asdouble=FALSE),  msg)

        msg <- "Parameter names in 'sub.params' must occur in 'obj'."
        expect_error(get.subrange(res, sub.params=list(alpha=1:2, omega=1:2), asdouble=FALSE),  msg)

        msg <- "Argument 'sub.params\\$alpha' has invalid type"
        expect_error(get.subrange(res, sub.params=list(alpha="a")),  msg)

        msg <- "Argument 'sub.params\\$alpha' out of range"
        expect_error(get.subrange(res, sub.params=list(alpha=0L), asdouble=FALSE),  msg)
        expect_error(get.subrange(res, sub.params=list(alpha=c(1L,2L,3L)), asdouble=FALSE),  msg)

        msg <- "Argument 'sub.params\\$gamma\\.lim' must be a pair of doubles."
        expect_error(get.subrange(res, sub.params=list(gamma.lim=1.0)),  msg)
        expect_error(get.subrange(res, sub.params=list(gamma.lim=c(1.0,2.0,3.0))),  msg)
        expect_error(get.subrange(res, sub.params=list(gamma.lim=c("a","b"))),  msg)
        expect_error(get.subrange(res, sub.params=list(gamma.lim=c(10L,15L)), asdouble=FALSE),  msg)

        msg <- "Argument 'sub.params\\$delta\\.lim' must be a pair of doubles."
        expect_error(get.subrange(res, sub.params=list(delta.lim=1.0)),  msg)
        expect_error(get.subrange(res, sub.params=list(delta.lim=c(1.0,2.0,3.0))),  msg)
        expect_error(get.subrange(res, sub.params=list(delta.lim=c("a","b"))),  msg)

        msg <- "Argument 'sub.params\\$gamma' has no valid entries."
        expect_error(get.subrange(res, sub.params=list(gamma=integer())),  msg)
        expect_error(get.subrange(res, sub.params=list(gamma.lim=c(100.0,200.0))),  msg)

        msg <- "Argument 'sub.params\\$delta' has no valid entries."
        expect_error(get.subrange(res, sub.params=list(delta=integer())),  msg)
        expect_error(get.subrange(res, sub.params=list(delta.lim=c(100.0,200.0))),  msg)

        msg <- "Cannot find entry '6' in parameter 'beta'."
        expect_warning(get.subrange(res, sub.params=list(beta=c(5,6,7))),  msg)
})
        
## --------------------------------------------------------------------------

remove(trunit)

## --------------------------------------------------------------------------
