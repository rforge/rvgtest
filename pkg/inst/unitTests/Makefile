TOP=../..
PKG=${shell cd ${TOP};pwd}
SUITE=doRUnit.R
R=R

help:
	@echo "test  ... run test suite"
	@echo "clean ... clear working space"

#all: inst test

#inst: # Install package
#	cd ${TOP}/..;\
#	${R} CMD INSTALL ${PKG}

test: # Run unit tests
	export RCMDCHECK=FALSE; \
	${R} --vanilla --slave < ${SUITE}

clean:
	@echo "clear working space ..."
	@rm -vf Rplots.pdf report.* *~

.PHONY: clean inst test


