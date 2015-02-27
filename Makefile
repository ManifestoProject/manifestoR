# variables
pkgname = manifestoR
pkgversion = 0.9-2

# default target
all: pack check

workflowvignette: vignettes/manifestoRworkflow.Rmd
	(cd vignettes; R -e "library(knitr); knit('manifestoRworkflow.Rmd'); library(markdown); markdownToHTML('manifestoRworkflow.md', 'manifestoRworkflow.html')"; pandoc manifestoRworkflow.html -o manifestoRworkflow.pdf)

vignettes: workflowvignette

doc:
	R -e "library(devtools); document()"
# TODO run roxygen2

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
	
scalingtest:
	(cd tests; R -f scaling.R)
	
withvignettes: vignettes all

pushdeploy:
	git checkout deploy
	git merge master
	git rm -f --ignore-unmatch man/*
	make doc
	git add -f NAMESPACE
	git add -f man/*
	git commit -m "Auto-creation of documentation"
	git push origin deploy	
	git checkout master
