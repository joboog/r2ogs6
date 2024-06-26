% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/config_ogs.R
\name{install_ogs}
\alias{install_ogs}
\title{Install specific version of OpenGeoSys 6 (OGS) along with necessary Python
packages `vtk` and `numpy` into a specified Python virtual environment.
Note, this does not work with `conda` environments.}
\usage{
install_ogs(..., ogs_version = "6.4.4", envname = "r2ogs6")
}
\arguments{
\item{...}{Rest of the arguments passed to "py_install()"}

\item{ogs_version}{The version of OGS to install (default: 6.4.4).}

\item{envname}{The name of the Python virtual environment (default: r2ogs6).}
}
\value{
None.
}
\description{
Install specific version of OpenGeoSys 6 (OGS) along with necessary Python
packages `vtk` and `numpy` into a specified Python virtual environment.
Note, this does not work with `conda` environments.
}
