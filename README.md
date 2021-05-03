# r2ogs6

`r2ogs6` is an R-API to the multiphysics simulator [OpenGeoSys 6](https://gitlab.opengeosys.org/ogs/ogs).
`r2ogs6` allows pre-processing (preparing input files), executing simulation runs, retrieving, post-processing and visualizing output data in R.
Furthermore, the package comes with functionality to define [ensemble runs](vignettes/ensemble_workflow_vignette.Rmd).

## Setup

### Preparing your environment

Before you install and load `r2ogs6`, there are two prerequisites that need to be met so you can use `r2ogs6` to its full extent later.

The most important one is having OpenGeoSys 6 installed on your system. You can download the current version of OpenGeoSys 6 from [here](https://www.opengeosys.org/releases/). 

Secondly, to read in the `.vtu` and `.pvd` files produced by OpenGeoSys 6, you need a Python installation including the libraries `numpy` and `vtk`. If you have an Anaconda installation on your system, setting up a Python environment with `numpy` and `vtk` will work. `r2ogs6` was tested with the following environment:

```
$ conda create -n r2ogs6 python=3.7
$ conda install -c anaconda numpy, vtk==8.2.0
```

If you don't have any Python installation on your system, you can download the latest release of Python from [here](https://www.python.org/downloads/). Instructions on how to install `numpy` and `vtk` can be found [here](https://numpy.org/install/) and  [here](https://pypi.org/project/vtk/).


### Installation

First, open a terminal and clone the `r2ogs6` repository to your local machine.

```
$ git clone https://gitlab.opengeosys.org/ag-hydinf/boog-group/r2ogs6.git
```

After that, open R from your local (anaconda) environment.
To install `r2ogs6`, you first need the R-package `devtools`.

```r
# Install devtools
install.packages("devtools")
```

With `devtools`, you can now install `r2ogs6`.

```r
# Install r2ogs6. Change the path to that of the cloned repository!
devtools::install("path/to/r2ogs6")
```

This will install all necessary R dependencies.

## Usage

For tutorials on how to use the `r2ogs6` package, have a look at its vignettes:

* [User Guide](vignettes/user_workflow_vignette.Rmd) 
* [Ensemble Guide](vignettes/ensemble_workflow_vignette.Rmd)
* [Developer Guide](vignettes/dev_workflow_vignette.Rmd)

## Links

* [OpenGeoSys Project](https://www.opengeosys.org/)
* [OpenGeoSys source code](https://gitlab.opengeosys.org/ogs/ogs)
* [OpenGeoSys documentation](https://www.opengeosys.org/docs/)