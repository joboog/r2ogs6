# r2ogs6

[![Pipeline Status](https://gitlab.opengeosys.org/ogs/tools/r2ogs6/badges/master/pipeline.svg)](https://gitlab.opengeosys.org/ogs/tools/r2ogs6/-/commits/master)
[![Test Coverage](https://gitlab.opengeosys.org/ogs/tools/r2ogs6/badges/master/coverage.svg)](https://gitlab.opengeosys.org/ogs/tools/r2ogs6/-/jobs)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7389626.svg)](https://doi.org/10.5281/zenodo.7389626)
[![JOSS Submission](https://joss.theoj.org/papers/08cb661c22ca8553e418acafeb9e7cb5/status.svg)](https://joss.theoj.org/papers/08cb661c22ca8553e418acafeb9e7cb5)

`r2ogs6` is an R-API to the multiphysics simulator [OpenGeoSys 6](https://gitlab.opengeosys.org/ogs/ogs).
`r2ogs6` allows pre-processing (preparing input files), executing simulation runs, retrieving, post-processing and visualizing output data in R.
Furthermore, the package comes with functionality to define [ensemble runs](vignettes/ensemble_workflow_vignette.Rmd).

## Setup

`r2ogs6` requires an installation of OpenGeoSys 6.
Furthermore, Python including the libraries `numpy` and `vtk` is required to read in the `.vtu` and `.pvd` files produced by OpenGeoSys 6.
These can be install after having installed `r2ogs6`.
`r2ogs6` was tested with the following environment: `python==3.10.12`, `vtk==9.3.0`, `numpy==1.26.4`


### r2ogs6 Installation

First, open a terminal and clone the `r2ogs6` repository to your local machine.

```
git clone https://gitlab.opengeosys.org/ogs/tools/r2ogs6.git
```

To install `r2ogs6`, you first need the R-packages `remotes` and `BiocManager`.
`remotes` is used for installing `r2ogs6` and `BiocManager` to set the URLs 
to search dependencies on *CRAN* and *bioconductor.org*.
For instance, the dependency `rhdf5` is only available on *bioconductor.org*.
Open a R console in your specific project:

```r
install.packages(c("remotes", "BiocManager"))
```

With `remotes`, you can now install `r2ogs6` in your preferred library.

```r
# Install r2ogs6. Change "path/to/r2ogs6" to that of the cloned repository!
remotes::install_local(
    path="path/to/r2ogs6", 
    dependencies="Imports",
    repos=BiocManager::repositories()
)
```

#### Set-up Python

OpenGeoSys 6 requires a Python installation of version 3.8--3.11.


##### Linux systems

If you do not have Python installed yet you can do so with the 
[reticulate](https://rstudio.github.io/reticulate/index.html) library from
your R console.

```r
reticulate::install_python(version = "3.10:latest")
```

Otherwise you can use the package manager of your linux distribution.


##### Windows

Ideally, install Python with a [Python Windows installer](https://www.python.org/downloads/windows/).
Of course you could also try:

```r
reticulate::install_python(version = "3.10:latest")
```

#### Install Python dependencies and OpenGeoSys 6

Create a Python virtual environment with a specific name (e.g. "r2ogs6) and the
 path to the Python executable you just installed.

```r
reticulate::virtualenv_create(
    envname="r2ogs6", 
    python="/path/to/python",
    packages = c("numpy", "vtk")
)
```

Make sure that `reticulate::py_config()$python` points to the python executable
in the newly installed Python virtual environment.
You can try to force `reticulate` to use the executable of your environment by:

```
reticulate::use_virtualenv(virtualenv="r2ogs6")
```

If that does not work, quit your R session and create a `.Renviron` file in 
your project folder where you can set the path to the created virtual python
environment:

```
# .Renviron linux
RETICULATE_PYTHON=/path/to/your/virtualenv/bin/python
```

This file is read at the start-up of R and forces `reticulate` to use your
specific Python executable. Double check if your python envrionment is now 
recognized:

```r
reticulate::py_config()
``` 

Then you can install OpenGeoSys 6 (the Python dependencies `numpy` and `vtk` 
will be installed alongside) as Python module.

```
library(r2ogs6)
install_ogs(ogs_version = "6.4.4", envname = "r2ogs6")
```
Be aware, to install the OpenGeoSys 6 version compatible with the `r2ogs6` version,
 see [releases](https://gitlab.opengeosys.org/ogs/tools/r2ogs6/-/releases).
Check if OpenGeoSys 6 was installed correctly.

```
reticulate::py_module_available("ogs")
```

The OpenGeoSys 6 binary should be located at `/path/to/your/python_env/bin/ogs` 
(Linux) or `/path/to/your/python_env/Scripts/ogs` (Windows). 
In your active R session you can temporarily set this path as default when 
using `r2ogs6` (sets `options("r2ogs6.default_ogs6_bin_path")`.)

```
set_ogs6_bin_path()
```

For setting this permanently, create a `config.yml` file in your project 
directory.

```yml
# config.yml for linux
default:
    r2ogs6.default_ogs6_bin_path: /path/to/python_env/bin/ogs
```


#### Alternative manual setup of Python environment

Alternatively, you can set up the python environment including OpenGeoSys 6
without `reticulate`; have a look [here](https://www.opengeosys.org/docs/userguide/basics/introduction/#install-via-pip).
Be aware, to download or install the OpenGeoSys 6 version compatible with the `r2ogs6` version, see [releases](https://gitlab.opengeosys.org/ogs/tools/r2ogs6/-/releases).

Nevertheless `r2ogs6` uses `reticulate` under the hood.
So make sure that `reticulate::py_config()` shows the correct Python
environment that you configured.


## Usage

For tutorials on how to use the `r2ogs6` package, have a look at its vignettes:

* [User Guide](vignettes/user_workflow_vignette.Rmd) 
* [Ensemble Guide](vignettes/ensemble_workflow_vignette.Rmd)
* [Developer Guide](vignettes/dev_workflow_vignette.Rmd)

Corresponding HTML versions can be found [here](inst/vignettes_built/).

## Links

* [OpenGeoSys Project](https://www.opengeosys.org/)
* [OpenGeoSys source code](https://gitlab.opengeosys.org/ogs/ogs)
* [OpenGeoSys documentation](https://www.opengeosys.org/docs/)
