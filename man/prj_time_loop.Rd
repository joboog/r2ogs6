% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prj_time_loop.R
\name{prj_time_loop}
\alias{prj_time_loop}
\title{prj_time_loop}
\usage{
prj_time_loop(
  processes,
  output,
  global_process_coupling = NULL,
  submesh_residuum_output = NULL
)
}
\arguments{
\item{processes}{list, prj_tl_process:}

\item{output}{prj_output:}

\item{global_process_coupling}{Optional: prj_global_process_coupling:}

\item{submesh_residuum_output}{Optional: prj_submesh_residuum_output:}
}
\description{
tag: time_loop
}
\examples{
prj_time_loop(
    processes = list(
        process = prj_tl_process(
            ref = "HM",
            nonlinear_solver = "basic_newton",
            convergence_criterion = prj_convergence_criterion(
                type = "DeltaX",
                norm_type = "NORM2",
                reltol = 1e-08
            ),
            time_discretization = list(type = "BackwardEuler"),
            time_stepping = prj_time_stepping(
                type = "FixedTimeStepping",
                t_initial = 0,
                t_end = 10000,
                timesteps = list(pair = list(1000,
                                             delta_t = 10))
            )
        )
    ),
    output = prj_output(
        type = "VTK",
        prefix = "flow_free_expansion",
        variables = list(
            variable = "displacement",
            variable = "pressure"
        ),
        suffix = "_ts_{:timestep}_t_{:time}",
        timesteps = list(pair = list(1,
                                     each_steps = 1000))
    )
)
}
