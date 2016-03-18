# variables
pkgname = manifestoR
pkgversion = 1.2

# default target
all: pack check

doc:
	R -e "library(devtools); document()"
	R -e "library(devtools); document()"

Rmdvignette:
	sed -i '/VignetteBuilder: R.rsp/c\VignetteBuilder: knitr' DESCRIPTION
	R -e "library(devtools); build_vignettes();"
	cp inst/doc/manifestoRworkflow.pdf vignettes/
	sed -i '/VignetteBuilder: knitr/c\VignetteBuilder: R.rsp' DESCRIPTION

pack: doc
	(cd ../; R CMD build $(pkgname))

check: pack
	(cd ../; R CMD check --as-cran $(pkgname)_$(pkgversion).tar.gz)

testcheck: test pack check

install: all
	R -e "install.packages('../$(pkgname)_$(pkgversion).tar.gz')"
	
test:
	R -e "library(devtools); library(testthat); with_reporter('stop', test())"
	
pushdeploy:
	git checkout deploy
	git merge master
	git rm -f --ignore-unmatch man/*
	make doc
	git add -f NAMESPACE
	git add -f man/*
	git add -f inst/doc/*
	git commit -m "Auto-creation of documentation"
	git push origin deploy	
	git checkout master
