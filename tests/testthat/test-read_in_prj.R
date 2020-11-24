

#processes



test_that("read_in_media works", {

    prj_path <- (system.file("extdata/flow_free_expansion",
                             "flow_free_expansion.prj", package = "r2ogs6"))

    ogs6_obj <- OGS6$new(sim_name = "sim",
                         sim_id = 1,
                         sim_path = "sim_path",
                         ogs_bin_path = "ogs_bin_path",
                         test_mode = TRUE)

    read_in_media(ogs6_obj, prj_path)

    expect_equal(length(ogs6_obj$media), 1)
})


test_that("read_in_time_loop works", {

    prj_path <- (system.file("extdata/flow_free_expansion",
                             "flow_free_expansion.prj", package = "r2ogs6"))

    ogs6_obj <- OGS6$new(sim_name = "sim",
                         sim_id = 1,
                         sim_path = "sim_path",
                         ogs_bin_path = "ogs_bin_path",
                         test_mode = TRUE)

    read_in_time_loop(ogs6_obj, prj_path)

    expect_equal(is.null(ogs6_obj$time_loop), FALSE)
})


test_that("read_in_parameters works", {

    prj_path <- (system.file("extdata/flow_free_expansion",
                             "flow_free_expansion.prj", package = "r2ogs6"))

    ogs6_obj <- OGS6$new(sim_name = "sim",
                         sim_id = 1,
                         sim_path = "sim_path",
                         ogs_bin_path = "ogs_bin_path",
                         test_mode = TRUE)

    read_in_parameters(ogs6_obj, prj_path)

    expect_equal(length(ogs6_obj$parameters), 7)
    expect_equal(ogs6_obj$parameters[[1]]$name, "E")
    expect_equal(ogs6_obj$parameters[[1]]$type, "Constant")
    expect_equal(ogs6_obj$parameters[[1]]$values, 10e9)
})

test_that("read_in_process_variables works", {

    prj_path <- (system.file("extdata/flow_free_expansion",
                             "flow_free_expansion.prj", package = "r2ogs6"))

    ogs6_obj <- OGS6$new(sim_name = "sim",
                         sim_id = 1,
                         sim_path = "sim_path",
                         ogs_bin_path = "ogs_bin_path",
                         test_mode = TRUE)

    read_in_process_variables(ogs6_obj, prj_path)

    expect_equal(length(ogs6_obj$process_variables), 2)
})

test_that("read_in_nonlinear_solvers works", {

    prj_path <- (system.file("extdata/flow_free_expansion",
                             "flow_free_expansion.prj", package = "r2ogs6"))

    ogs6_obj <- OGS6$new(sim_name = "sim",
                         sim_id = 1,
                         sim_path = "sim_path",
                         ogs_bin_path = "ogs_bin_path",
                         test_mode = TRUE)

    read_in_nonlinear_solvers(ogs6_obj, prj_path)

    expect_equal(length(ogs6_obj$nonlinear_solvers), 1)
    expect_equal(ogs6_obj$nonlinear_solvers[[1]]$name, "basic_newton")
    expect_equal(ogs6_obj$nonlinear_solvers[[1]]$type, "Newton")
    expect_equal(ogs6_obj$nonlinear_solvers[[1]]$max_iter, 50)
    expect_equal(ogs6_obj$nonlinear_solvers[[1]]$linear_solver, "general_linear_solver")
})


test_that("read_in_linear_solvers works", {

    prj_path <- (system.file("extdata/flow_free_expansion",
                             "flow_free_expansion.prj", package = "r2ogs6"))

    ogs6_obj <- OGS6$new(sim_name = "sim",
                         sim_id = 1,
                         sim_path = "sim_path",
                         ogs_bin_path = "ogs_bin_path",
                         test_mode = TRUE)

    read_in_linear_solvers(ogs6_obj, prj_path)

    expect_equal(length(ogs6_obj$linear_solvers), 1)
    expect_equal(ogs6_obj$linear_solvers[[1]]$name, "general_linear_solver")
    expect_equal(ogs6_obj$linear_solvers[[1]]$lis, "-i bicgstab -p ilu -tol 1e-16 -maxiter 10000")
    expect_equal(ogs6_obj$linear_solvers[[1]]$eigen, list(solver_type = "BiCGSTAB",
                                                          precon_type = "ILUT",
                                                          max_iteration_step = 10000,
                                                          error_tolerance = 1e-16))
})
