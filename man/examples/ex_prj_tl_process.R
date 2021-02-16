r2ogs6_tl_process(
    ref = "HM",
    nonlinear_solver = "basic_newton",
    convergence_criterion = r2ogs6_convergence_criterion(
        type = "DeltaX",
        norm_type = "NORM2",
        reltol = 1e-08
    ),
    time_discretization = list(type = "BackwardEuler"),
    time_stepping = r2ogs6_time_stepping(
        type = "FixedTimeStepping",
        t_initial = 0,
        t_end = 10000,
        timesteps = list(pair = list(1000,
                                     delta_t = 10))
    )
)
