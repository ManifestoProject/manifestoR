# variables
pkgname = manifestoR
pkgversion = 0.6-1

# default target
all: pack check

workflowvignette: vignettes/manifestoRworkflow.Rmd
	(cd vignettes; R -e "library(knitr); knit('manifestoRworkflow.Rmd'); library(markdown); markdownToHTML('manifestoRworkflow.md', 'manifestoRworkflow.html')"; pandoc manifestoRworkflow.html -o manifestoRworkflow.pdf)

vignettes: workflowvignette

doc:
	R -e "library(devtools); library(roxygen2); document(clean = TRUE, roclets = c('namespace', 'rd'))"
# TODO run roxygen2

pack: doc
	(cd ../; R CMD build $(pkgname))

check:
	(cd ../; R CMD check $(pkgname)_$(pkgversion).tar.gz --no-tests)

checktest:
	(cd ../; R CMD check $(pkgname)_$(pkgversion).tar.gz)

install: all
	R -e "install.packages('../$(pkgname)_$(pkgversion).tar.gz')"
	
test: install cachetest metadatatest corpustest

cachetest: 
	(cd tests; R -f cache.R)
	
metadatatest: 
	(cd tests; R -f metadata.R)
	
corpustest:
	(cd tests; R -f corpus.R)
	
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
