# ManifestoR package

R package for accessing manifesto data and original documents directly from R.

## Documentation

The main user documentation is the vignette `manifestoRworkflow`.
It is available in different formats in the subfolder `vignettes` of this repository.
A pdf version is available in Jirka`s ownCloud folder:
[here](https://cloud.wzb.eu/public.php?service=files&t=8b30d7bd0e9a18062fbeea6cf8f2e3f3).
The password is manifestoeroe.

## Installation

### Requirements

R version 3.1 or higher is required.

You might need to install dependencies before: `tm`, `httr`, `jsonlite`, `plyr`.

```r
install.packages(c("httr", "tm", "jsonlite", "plyr"))
```

**Note**: manifestoR depends on the most recent version 0.9.12 of `jsonlite`,
which is not on CRAN as of now. Using `devtools` it can be installed from GitHub:

```r
install.package("devtools") ## if necessary
install_github("jsonlite", user="jeroenooms", ref="master")
```

### From tarball

A tarball of v0.5 is available in Jirka`s ownCloud folder:
[here](https://cloud.wzb.eu/public.php?service=files&t=8b30d7bd0e9a18062fbeea6cf8f2e3f3).
The password is manifestoeroe.

To install, download the tar.gz file.

#### Linux and Max OS X

Run the following R command, with the path adapted to your situation:

```r
install.packages("path/to/manifestoR_0.5.tar.gz", repos=NULL, type="source")
```

#### Windows

1. Find out where your R.exe is (Usually in `C:/Program Files/R/R-3.0.0/bin/R.exe` or other version number).

2. Open a command line.

3. cd into the directory of your R.exe

4. Issue the following command (with the path adapted to where you downloaded the .tar.gz):

```
R.exe CMD INSTALL path/to/manifestoR_0.5.tar.gz
```

### From source repositories

At the moment you can pack and install the package from this repository via the `Makefile`:

`make install` packs and installs the package (and documentation) to your default R installation.

`make` (=`make all`) packs the package (and documentation) to a tarball in the parent directory to this.

Both targets also check the tarball, which can be done manually by `make check`.

Tarballs will be provided as soon as this makes sense.


**Build dependencies:**

To pack the source and documentation from this folder into a package tarball your system needs to have installed:

* R packages `devtools` and `roxygen2`
* R packages `knitr` (for documentation)
* `texlive-fonts-extra` (for documentation)