# variables
pkgname = manifestoR
pkgversion = 0.9-3

# default target
all: pack check

doc:
	R -e "library(devtools); document()"
	R -e "library(devtools); document()"
	R -e "library(devtools); install(); builld_vignettes();"

pack: doc test
	(cd ../; R CMD build $(pkgname))

check:
	R -e "library(devtools); check();"
checktest:
	(cd ../; R CMD check $(pkgname)_$(pkgversion).tar.gz)

install: all
	R -e "install.packages('../$(pkgname)_$(pkgversion).tar.gz')"
	
test:
	R -e "library(devtools); library(testthat); test()"
	
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
