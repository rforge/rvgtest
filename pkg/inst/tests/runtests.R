
## --- Setup ----------------------------------------------------------------

## Load package 'testthat' 
if(! require("testthat", quietly=TRUE)) {
  message("\ncannot run unit tests -- package 'testthat' is not available\n")
  quit(save="no",runLast=FALSE)
}

## Load package 'rvgtest' 
library("rvgtest")

## Store R options
opt.save <- options()

## Print warnings immediately
## options(warn=1)

## --- Auxiliary routines ---------------------------------------------------

## check for warning but suppress warning message
expect_warning_suppress <- function (x, y) {
        opt.store <- options()
        options(warn=-1)
        expect_warning(x,y)
        options(opt.store)
}

## --- Run tests ------------------------------------------------------------

## Path to unit test files
if(Sys.getenv("RCMDCHECK") == "FALSE") {
        ## Path to unit tests for standalone running under Makefile (not R CMD check)
        ## PKG/inst/tests
        unittest.dir <- file.path(getwd())
} else {
        ## Path to unit tests for R CMD check
        ## PKG.Rcheck/tests/../PKG/unitTests
        unittest.dir <- system.file(package="rvgtest", "tests")
}

## Use summary reporter
test_dir(unittest.dir,reporter = "summary")

## A possible alternative is the TAP reporter that uses
## the Test Anything Protocol (TAP) for the output result.
## test_dir(".",reporter = "tap")

## --- End ------------------------------------------------------------------

## Restore R options
options(opt.save)

## --------------------------------------------------------------------------
