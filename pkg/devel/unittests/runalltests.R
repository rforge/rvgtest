
## Standalone test suite

## --- Setup ----------------------------------------------------------------

## Load package 'testthat' 
if(! require("testthat", quietly=TRUE)) {
  message("\nCannot run unit tests -- package 'testthat' is not available!\n")
  quit(save="no",runLast=FALSE)
}

## Load package 'rvgtest' 
library("rvgtest")

## Store R options
opt.save <- options()

## Print warnings immediately
## options(warn=1)

## Path to unit test files
##unittest.dir <- file.path(getwd(),"..","..","tests","testthat")
unittest.dir <- file.path("..","..","tests","testthat")

## --- Run tests ------------------------------------------------------------

## Print header
cat(rep("=",45),"\n",
    " Run test suite in directory\n ",
    unittest.dir,"\n",
    rep("=",45),"\n", sep="")

## Use summary reporter
reporter <- SummaryReporter$new()

## A possible alternative is the TAP reporter that uses
## the Test Anything Protocol (TAP) for the output result.
### reporter <- TapReporter$new()

## Run tests 
test_dir(unittest.dir, reporter=reporter)

## --- End ------------------------------------------------------------------

## Restore R options
options(opt.save)

## check for failed tests
if (reporter$failed) {
        stop("Test failures", call. = FALSE)
}

## --------------------------------------------------------------------------
