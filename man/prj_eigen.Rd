% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prj_linear_solver.R
\name{prj_eigen}
\alias{prj_eigen}
\title{prj_eigen}
\usage{
prj_eigen(
  solver_type,
  precon_type = NULL,
  max_iteration_step = NULL,
  error_tolerance = NULL,
  scaling = NULL,
  restart = NULL
)
}
\arguments{
\item{solver_type}{string:}

\item{precon_type}{Optional: string:}

\item{max_iteration_step}{Optional: string | double: ...}

\item{error_tolerance}{Optional: string | double: ...}

\item{scaling}{Optional: string | double: ...}

\item{restart}{Optional: string | double: ...}
}
\description{
tag: eigen
}
\examples{
prj_eigen(
    solver_type = "BiCGSTAB",
    precon_type = "ILUT",
    max_iteration_step = 10000,
    error_tolerance = 1e-16
)
}
