
.onLoad <- function(lib, pkg) {
	library.dynam("rvgtest", pkg, lib)
}

.onUnload <- function(libpath) {
	library.dynam.unload("rvgtest", libpath)
}

## We need some (exported and non-exported) functions from
## package 'parallel'.
rvgt.mcparallel     <- parallel::mcparallel
rvgt.processID      <- parallel:::processID
rvgt.selectChildren <- parallel:::selectChildren
rvgt.children       <- parallel:::children
rvgt.mckill         <- parallel:::mckill
rvgt.readChild      <- parallel:::readChild
