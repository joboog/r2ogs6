% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_utils.R
\name{ogs6_export_sim_files}
\alias{ogs6_export_sim_files}
\title{ogs6_export_sim_files}
\usage{
ogs6_export_sim_files(
  ogs6_obj,
  overwrite = T,
  copy_ext_files = F,
  test_mode = F
)
}
\arguments{
\item{ogs6_obj}{OGS6: Simulation object}

\item{overwrite}{flag: Should existing files be overwritten?}

\item{copy_ext_files}{flag: Should external files that are references in the
\code{ogs6_obj} be  be copied to \code{ogs6_obj$sim_path}?}

\item{test_mode}{flag: If \code{TRUE}, Will not check status of
\code{ogs6_obj} before exporting files. Defaults to \code{FALSE}}
}
\description{
Creates \code{ogs6$sim_path} directory if it does not exist yet
  and exports and / or copies all simulation files to it.
}
