prj_linear_solver(
    name = "general_linear_solver",
    eigen = prj_eigen(
        solver_type = "BiCGSTAB",
        precon_type = "ILUT",
        max_iteration_step = 10000,
        error_tolerance = 1e-16
    ),
    lis = "-i bicgstab -p ilu -tol 1e-16 -maxiter 10000"
)