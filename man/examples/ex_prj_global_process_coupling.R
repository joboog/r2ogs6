r2ogs6_global_process_coupling(
    max_iter = 100,
    convergence_criteria = list(
        convergence_criterion = r2ogs6_convergence_criterion(
            type = "DeltaX",
            norm_type = "INFINITY_N",
            abstol = 1e-08,
            reltol = 1e-10
        )
    )
)
