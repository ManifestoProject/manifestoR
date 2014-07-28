# ManifestoR package

R package for accessing manifesto data and original documents directly from R.

## Installation

At the moment you can pack and install the package from this repository via the `Makefile`:

`make install` packs and installs the package (and documentation) to your default R installation.

`make` (=`make all`) packs the package (and documentation) to a tarball in the parent directory to this.

Both targets also check the tarball, which can be done manually by `make check`.

Tarballs will be provided as soon as this makes sense.


## Build dependencies:

To pack the source and documentation from this folder into a package tarball your system needs to have installed:

* R packages `devtools` and `roxygen2`
* `texlive-fonts-extra` (for documentation)

