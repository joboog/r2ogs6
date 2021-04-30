# r2ogs6

An *r2ogs6* is an R-API to the multiphysics simulator [OpenGeoSys 6](https://gitlab.opengeosys.org/ogs/ogs).
*r2ogs6* allows to preprocess (preparing input files), execute simulation runs, retrieve, post-process and visualize output data in R.
Furthermore, the package comes with functionality to define [ensemble runs](vignettes/ensemble_workflow_vignette.Rmd)

## Install *r2ogs6*

### Set-Up your Environment

Apart from having the library installed and loaded, there are two more prerequisites that need to be met for you to be able to use `r2ogs6` to its full extent.

The most important one is having OpenGeoSys 6 installed on your system. You can download the current version of OpenGeoSys 6 from [here](https://www.opengeosys.org/releases/). 

Secondly, to read in the `.vtu` and `.pvd` files produced by OpenGeoSys 6, you need a Python installation including the libraries `numpy` and `vtk`. If you have an Anaconda installation on your system, setting up a Python environment with `numpy` and `vtk` will work. *r2ogs6* was tested with the following environment.

```
$ conda create -n r2ogs6 python=3.7
$ conda install -c anaconda numpy, vtk==8.2.0
```

If you don't have any Python installation on your system, you can download the latest release of Python from [here](https://www.python.org/downloads/). Instructions on how to install `numpy` and `vtk` can be found [here](https://numpy.org/install/) and  [here](https://pypi.org/project/vtk/).


### Install the *r2ogs6* Package

Just clone the repository to your local machine `git clone https://gitlab.opengeosys.org/ag-hydinf/boog-group/r2ogs6.git`
Open *R* from your local (anaconda) environment.
For installing *r2ogs6* you need the R-package `devtools`.
If you do not have `devtools` installed, install it via `install.packages("devtools")`.
Install *r2ogs6* with `devtools::install("path/to/r2ogs6")` referring to the path of the cloned repository.
All R-dependencies will be installed if not present.

## Use *r2ogs6*

Just have a look to the vignettes showing:

* [basic features](vignettes/user_workflow_vignette.Rmd) 
* [creating ensemble runs](vignettes/ensemble_workflow_vignette.Rmd)
* [further development](vignettes/dev_workflow_vignette.Rmd)

## Links

* [OpenGeoSys Project](https://www.opengeosys.org/)
* [OpenGeoSys source code](https://gitlab.opengeosys.org/ogs/ogs)
* [OpenGeoSys documentation](https://www.opengeosys.org/docs/)