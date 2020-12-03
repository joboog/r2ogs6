

#processes



test_that("read_in works for medium objects", {

    prj_path <- (system.file("extdata/flow_free_expansion",
                             "flow_free_expansion.prj", package = "r2ogs6"))

    ogs6_obj <- OGS6$new(sim_name = "sim",
                         sim_id = 1,
                         sim_path = "sim_path",
                         ogs_bin_path = "ogs_bin_path",
                         test_mode = TRUE)

    read_in(ogs6_obj, prj_path, "media", "medium",
            subclasses_names = c(phase = "r2ogs6_medium_phase",
                                 property = "r2ogs6_medium_property"))

    expect_equal(length(ogs6_obj$media), 1)
})


test_that("read_in works for time_loop objects", {

    prj_path <- (system.file("extdata/flow_free_expansion",
                             "flow_free_expansion.prj", package = "r2ogs6"))

    ogs6_obj <- OGS6$new(sim_name = "sim",
                         sim_id = 1,
                         sim_path = "sim_path",
                         ogs_bin_path = "ogs_bin_path",
                         test_mode = TRUE)

    read_in(ogs6_obj, prj_path, "time_loop", "time_loop",
            subclasses_names =
                c(process = "r2ogs6_tl_process",
                  output = "r2ogs6_tl_output",
                  global_processes_coupling =
                      "r2ogs6_global_processes_coupling"))

    expect_equal(is.null(ogs6_obj$time_loop), FALSE)
})


test_that("read_in works for parameter objects", {

    prj_path <- (system.file("extdata/flow_free_expansion",
                             "flow_free_expansion.prj", package = "r2ogs6"))

    ogs6_obj <- OGS6$new(sim_name = "sim",
                         sim_id = 1,
                         sim_path = "sim_path",
                         ogs_bin_path = "ogs_bin_path",
                         test_mode = TRUE)

    read_in(ogs6_obj, prj_path, "parameters", "parameter")

    expect_equal(length(ogs6_obj$parameters), 7)
    expect_equal(ogs6_obj$parameters[[1]]$name, "E")
    expect_equal(ogs6_obj$parameters[[1]]$type, "Constant")
    expect_equal(ogs6_obj$parameters[[1]]$values, 10e9)
})

test_that("read_in works for process_variable objects", {

    prj_path <- (system.file("extdata/flow_free_expansion",
                             "flow_free_expansion.prj", package = "r2ogs6"))

    ogs6_obj <- OGS6$new(sim_name = "sim",
                         sim_id = 1,
                         sim_path = "sim_path",
                         ogs_bin_path = "ogs_bin_path",
                         test_mode = TRUE)

    read_in(ogs6_obj, prj_path, "process_variables", "process_variable",
            subclasses_names = c(boundary_condition =
                                     "r2ogs6_boundary_condition"))

    expect_equal(length(ogs6_obj$process_variables), 2)
})

test_that("read_in works for nonlinear_solver objects", {

    prj_path <- (system.file("extdata/flow_free_expansion",
                             "flow_free_expansion.prj", package = "r2ogs6"))

    ogs6_obj <- OGS6$new(sim_name = "sim",
                         sim_id = 1,
                         sim_path = "sim_path",
                         ogs_bin_path = "ogs_bin_path",
                         test_mode = TRUE)

    read_in(ogs6_obj, prj_path, "nonlinear_solvers", "nonlinear_solver")

    expect_equal(length(ogs6_obj$nonlinear_solvers), 1)
    expect_equal(ogs6_obj$nonlinear_solvers[[1]]$name, "basic_newton")
    expect_equal(ogs6_obj$nonlinear_solvers[[1]]$type, "Newton")
    expect_equal(ogs6_obj$nonlinear_solvers[[1]]$max_iter, 50)
    expect_equal(ogs6_obj$nonlinear_solvers[[1]]$linear_solver, "general_linear_solver")
})


test_that("read_in works for linear_solver objects", {

    prj_path <- (system.file("extdata/flow_free_expansion",
                             "flow_free_expansion.prj", package = "r2ogs6"))

    ogs6_obj <- OGS6$new(sim_name = "sim",
                         sim_id = 1,
                         sim_path = "sim_path",
                         ogs_bin_path = "ogs_bin_path",
                         test_mode = TRUE)

    read_in(ogs6_obj, prj_path, "linear_solvers", "linear_solver")

    expect_equal(length(ogs6_obj$linear_solvers), 1)
    expect_equal(ogs6_obj$linear_solvers[[1]]$name, "general_linear_solver")
    expect_equal(ogs6_obj$linear_solvers[[1]]$lis, "-i bicgstab -p ilu -tol 1e-16 -maxiter 10000")
    expect_equal(ogs6_obj$linear_solvers[[1]]$eigen, list(solver_type = "BiCGSTAB",
                                                          precon_type = "ILUT",
                                                          max_iteration_step = 10000,
                                                          error_tolerance = 1e-16))
})
