# ManifestoR

An R package for accessing the [Manifesto Project](https://manifesto-project.wzb.eu)'s
Data and Corpus of election programmes.


## Quick Start Guide

You can install the package from CRAN (very soon):
```
install.packages("manifestoR")
```

Then a typical script of session with `manifestoR` starts like this:
```
library(manifestoR)
mp_setapikey("manifesto_apikey.txt") ## create and download your API key at https://manifesto-project.wzb.eu before

## download election programmes texts and codings
election_programmes <- mp_corpus(countryname == "Bulgaria")

## for example:
head(content(election_programmes[[1]])) ## view beginning of text of first manifesto
table(codes(election_programmes)) ## count codes of all manifestos

## ...
```

## Documentation

The main user documentation is the vignette `manifestoRworkflow`. It walks you
through the package's central functions giving many example code bits. For detailed
information about all functions and parameters, pleaser refer to the functions'
documentations with R's `?` or the packages Reference Manual.

## Contributing

If you want to contribute to the development of `manifestoR` by reporting bugs,
proposing features or writing program code, you are invited to do this on the
package's github page: [https://github.com/ManifestoProject/manifestoR](https://github.com/ManifestoProject/manifestoR).
Developers, please also note the information on packing and testing `manifestoR` below.


## Alternative versions and installation methods

Stable major versions of `manifestoR` will be provided on CRAN, such that you can install
and update them via R's base functions `install.packages` and `update.packages`.
A development version is available on github, which you can install to get the most recent
features and bugfixes.

### Installation from github

With the R package `devtools` you can install the package directly from its sources on github:
```
library(devtools)
install_github("ManifestoProject/manifestoR", ref = "deploy")
```

Note that only the branch `deploy` contains the full documentation, such that 
direct installation in R is preferable from this branch.

### Developing: from a local source copy

If you clone the master branch of this repository, a `Makefile` will come with the source code.

You can pack and install the package from this repository via the `Makefile`:

`make install` packs and installs the package (and documentation) to your default R installation.

`make` (=`make all`) packs the package (and documentation) to a tarball in the parent directory.

You can run the tests provided together with the source code with `make test`. Note that this
requires a file with a valid Manifesto Project DB API Key in the file `tests/manifesto_apikey.txt`.

**Build dependencies:**

To pack the source and documentation from this folder into a package tarball your system needs to have installed:

* R packages `devtools` and `roxygen2`
* R packages `knitr` (for documentation)
* `texlive-fonts-extra` (for documentation)