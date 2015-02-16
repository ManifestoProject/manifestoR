# ManifestoR package

R package for accessing manifesto data and original documents directly from R.

## Documentation

The main user documentation is the vignette `manifestoRworkflow`.
It is available in different formats in the subfolder `vignettes` of this repository.
A pdf version is available in Jirka`s ownCloud folder:
[here](https://cloud.wzb.eu/public.php?service=files&t=8b30d7bd0e9a18062fbeea6cf8f2e3f3).
The password is manifestoeroe.

## Installation

You should be able to install the most recent `manifestoR` version
using `devtools` by running in R:

```r
library(devtools)
install_git("https://<GITLABUSERNAME>:<GITLABPASSWORD>@gitlab.manifesto-project.wzb.eu/marpor/manifestor.git", branch="deploy")
```

- Replace `<GITLABUSERNAME>` with your gitlab user name and `<GITLABPASSWORD>` with your gitlab password
- All dependencies will be installed/updated automatically
- On Windows this probably requires `Rtools` and `git` to to be installed

For other installation methods see the following subsections.

Development note: To push your current master to the `deploy` branch (and create documentation etc.) run `make pushdeploy`

### Requirements

R version 3.1 or higher is required.

`manifesoR` depends on the packages `tm`, `httr`, `jsonlite`, `dplyr`, `functional` which are
automatically installed when using `install_git`. If this does not work: use

```r
install.packages(c("httr", "tm", "jsonlite", "dplyr", "functional"))
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