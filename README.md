# ManifestoR package

R package for accessing manifesto data and original documents directly from R.

## Documentation

The main user documentation is the vignette `manifestoRworkflow`.
It is avialbale in different formats in the subfolder `vignettes` of this repository.

## Installation

### From tarball

A tarball of v0.5 is available in Jirka`s ownCloud folder:
[here](https://cloud.wzb.eu/public.php?service=files&t=8b30d7bd0e9a18062fbeea6cf8f2e3f3).
The password is manifestoeroe.

To install, download the tar.gz file and run the following R command, with the path adapted to your situation:

```r
install.packages("path/to/manifestoR_0.5.tar.gz", repos=NULL, type="source")
```

You might need to install dependencies before: `tm`, `httr`, `jsonlite`, `plyr`.

```r
install.packages(c("httr", "tm", "jsonlite", "plyr"))
```

The ownCloud folder also contains the current main documentation, the `manifestoRworkflow` vignette.


### From source repositories

At the moment you can pack and install the package from this repository via the `Makefile`:

`make install` packs and installs the package (and documentation) to your default R installation.

`make` (=`make all`) packs the package (and documentation) to a tarball in the parent directory to this.

Both targets also check the tarball, which can be done manually by `make check`.

Tarballs will be provided as soon as this makes sense.


**Build dependencies:**

To pack the source and documentation from this folder into a package tarball your system needs to have installed:

* R packages `devtools` and `roxygen2`
* `texlive-fonts-extra` (for documentation)