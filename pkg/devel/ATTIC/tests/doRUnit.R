
## location of unitTest files
runit.dir <- system.file(package="rvgtest", "unitTests")

## name of R file with test suite
testsuite.file <- "doRUnit.R"

## path to R file with test suite
testsuite <- file.path(runit.dir,testsuite.file)

## load and test suite
cat("Running testsuite",testsuite,"\n") 
source(testsuite)

## end
                     
