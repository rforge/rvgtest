TOP=../..                       ## top directory of package
PKG=${shell cd ${TOP};pwd}      ## name of top directory
SUITE=runalltests.R
R=R

help:
	@echo "test  ... run test suite"
	@echo "check ... run test suite"
	@echo "inst  ... install package"
	@echo "all   ... install package and run test suite"
	@echo "clean ... clear working space"

all: inst test

inst: # Install package
	cd ${TOP}/..; \
	${R} CMD INSTALL ${PKG}

test: # Run unit tests
	${R} --vanilla --slave < ${SUITE}

check: # same as 'test'
	make test

clean:
	@echo "clear working space ..."
	@rm -vf Rplots.pdf *~

.PHONY: all check clean inst test


