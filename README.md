# r2ogs6

[![Pipeline Status](https://gitlab.opengeosys.org/ogs/tools/r2ogs6/badges/master/pipeline.svg)](https://gitlab.opengeosys.org/ogs/tools/r2ogs6/-/commits/master)
[![Test Coverage](https://gitlab.opengeosys.org/ogs/tools/r2ogs6/badges/master/coverage.svg)](https://gitlab.opengeosys.org/ogs/tools/r2ogs6/-/jobs)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7389626.svg)](https://doi.org/10.5281/zenodo.7389626)
[![JOSS Submission](https://joss.theoj.org/papers/08cb661c22ca8553e418acafeb9e7cb5/status.svg)](https://joss.theoj.org/papers/08cb661c22ca8553e418acafeb9e7cb5)

`r2ogs6` is an R-API to the multiphysics simulator [OpenGeoSys 6](https://gitlab.opengeosys.org/ogs/ogs).
`r2ogs6` allows pre-processing (preparing input files), executing simulation runs, retrieving, post-processing and visualizing output data in R.
Furthermore, the package comes with functionality to define [ensemble runs](vignettes/ensemble_workflow_vignette.Rmd).

## Setup

### Preparing your environment

`r2ogs6` requires an installation of OpenGeoSys 6.
Furthermore, Python including the libraries `numpy` and `vtk` is required to read in the `.vtu` and `.pvd` files produced by OpenGeoSys 6.


You can install OpenGeoSys 6 and Python including `numpy` and `vtk` via a 
convenient function after having installed `r2ogs6`.
Just proceed with [Installation]#installation.
Or do the manual installation.


#### Manual Installation of Python and OpenGeoSys 6

Ideally, you do this in a specific virutal python environment. 

```
# Create python environment
python -m ogs_env ~/.ogs_env
source ~/.ogs_env/bin/activate

# Use the ogs version appropriate for the r2ogs6 version, see releases
pip install ogs==6.4.4

# Install python dependencies
pip install numpy vtk
```

Alternatively, can download OpenGeoSys 6 executables [here](https://www.opengeosys.org/releases/) and set up the python 
environment with `numpy` and `vtk` only.
For more information on how to install OpenGeoSys 6 look [here](https://www.opengeosys.org/docs/userguide/basics/introduction/#install-via-pip).
Be aware, to download or install the OpenGeoSys 6 version compatible with the `r2ogs6` version, see [releases](https://gitlab.opengeosys.org/ogs/tools/r2ogs6/-/releases).
`r2ogs6` was tested with the following environment: `python==3.10.12`, `vtk==9.3.0`, `numpy==1.26.4`
If you don't have any Python installation on your system, you can download the latest release of Python from [here](https://www.python.org/downloads/). 


### r2ogs6 Installation

First, open a terminal and clone the `r2ogs6` repository to your local machine.

```
git clone https://gitlab.opengeosys.org/ogs/tools/r2ogs6.git
cd r2ogs6

# Now checkout the latest stable release
git checkout tags/v0.4.643
```

To install `r2ogs6`, you first need the R-packages `remotes` and `BiocManager`.
`remotes` is used for installing `r2ogs6` and `BiocManager` to set the URLs 
to search dependencies on *CRAN* and *bioconductor.org*.
For instance, the dependency `rhdf5` is only available on *bioconductor.org*.
In your R console:

```r
install.packages(c("remotes", "BiocManager"))
```

With `remotes`, you can now install `r2ogs6`.

```r
# Install r2ogs6. Change the path to that of the cloned repository!
remotes::install_local(
    path="path/to/r2ogs6", 
    dependencies="Imports",
    repos=BiocManager::repositories()
)
```

Now install OpenGeoSys 6 and Python including `numpy` and `vtk`.

```
library(r2ogs6)
library(reticulate)

# Create a python virtual environment "r2ogs6" and install ogs, numpy, vtk
install_ogs(ogs_version = "6.4.4", envname = "r2ogs6")
```

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
