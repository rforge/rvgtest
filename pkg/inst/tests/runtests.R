require("testthat")
require("rvgtest")

## store R options
opt.save <- options()

## auxiliary routines -------------------------------------------------------

## check for warning but suppress warning message
expect_warning_suppress <- function (x, y) {
        opt.store <- options()
        options(warn=-1)
        expect_warning(x,y)
        options(opt.store)
}


## Print warnings immediately
options(warn=1)

test_dir(".",reporter = "summary")
##test_dir(".",reporter = "tap")

## restore R options
options(opt.save)
