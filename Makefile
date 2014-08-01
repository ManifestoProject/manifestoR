# variables
pkgname = manifestoR
pkgversion = 0.1

# default target
all: pack check


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
	
test: install cachetest

cachetest: 
	(cd tests; R -f cache.R)