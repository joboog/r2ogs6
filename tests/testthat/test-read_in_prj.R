

test_that("read_in works for process objects", {

    prj_path <- (system.file("extdata/benchmarks/flow_free_expansion",
                             "flow_free_expansion.prj", package = "r2ogs6"))

    ogs6_obj <- OGS6$new(sim_name = "sim",
                         sim_path = "sim_path")

    read_in(ogs6_obj,
            prj_path,
            "/OpenGeoSysProject/processes/process")

    expect_equal(length(ogs6_obj$processes), 1)

    process <- ogs6_obj$processes[[1]]

    expect_equal(process$name, "HM")
    expect_equal(process$type, "HYDRO_MECHANICS")
    expect_equal(process$integration_order, 3)
    expect_equal(length(process$process_variables), 2)
})



test_that("read_in works for medium objects", {

    prj_path <- (system.file("extdata/benchmarks/flow_free_expansion",
                             "flow_free_expansion.prj", package = "r2ogs6"))

    ogs6_obj <- OGS6$new(sim_name = "sim",
                         sim_path = "sim_path")

    read_in(ogs6_obj,
            prj_path,
            "/OpenGeoSysProject/media/medium")

    expect_equal(length(ogs6_obj$media), 1)
})


test_that("read_in works for class objects with ellipsis", {

    prj_path <- (system.file("extdata/benchmarks/theis_well_pumping",
                             "theis.prj", package = "r2ogs6"))

    ogs6_obj <- OGS6$new(sim_name = "sim",
                         sim_path = "sim_path")

    read_in(ogs6_obj,
            prj_path,
            "/OpenGeoSysProject/media/medium")

    expect_equal(ogs6_obj$
                     media[[1]]$
                     phases[[1]]$
                     properties[[1]]$
                     independent_variable[[1]][["variable_name"]],
                 "concentration")

    expect_equal(ogs6_obj$
                     media[[1]]$
                     phases[[1]]$
                     properties[[1]]$
                     independent_variable[[2]][["variable_name"]],
                 "phase_pressure")
})


test_that("read_in works for time_loop objects", {

    prj_path <- (system.file("extdata/benchmarks/flow_free_expansion",
                             "flow_free_expansion.prj", package = "r2ogs6"))

    ogs6_obj <- OGS6$new(sim_name = "sim",
                         sim_path = "sim_path")

    read_in(ogs6_obj,
            prj_path,
            "/OpenGeoSysProject/time_loop")

    expect_equal(is.null(ogs6_obj$time_loop), FALSE)
    expect_equal(ogs6_obj$time_loop$output$type, "VTK")
    expect_equal(names(ogs6_obj$time_loop$output$timesteps), c("pair"))
    expect_equal(ogs6_obj$time_loop$output$timesteps[[1]][["repeat"]], 1)
})


test_that("read_in works for parameter objects", {

    prj_path <- (system.file("extdata/benchmarks/flow_free_expansion",
                             "flow_free_expansion.prj", package = "r2ogs6"))

    ogs6_obj <- OGS6$new(sim_name = "sim",
                         sim_path = "sim_path")

    read_in(ogs6_obj,
            prj_path,
            "/OpenGeoSysProject/parameters/parameter")

    expect_equal(length(ogs6_obj$parameters), 7)
    expect_equal(ogs6_obj$parameters[[1]]$name, "E")
    expect_equal(ogs6_obj$parameters[[1]]$type, "Constant")
    expect_equal(ogs6_obj$parameters[[1]]$value, 10e9)
})

test_that("read_in works for process_variable objects", {

    prj_path <- (system.file("extdata/benchmarks/flow_free_expansion",
                             "flow_free_expansion.prj", package = "r2ogs6"))

    ogs6_obj <- OGS6$new(sim_name = "sim",
                         sim_path = "sim_path")

    read_in(ogs6_obj,
            prj_path,
            "/OpenGeoSysProject/process_variables/process_variable")

    expect_equal(length(ogs6_obj$process_variables), 2)
})

test_that("read_in works for nonlinear_solver objects", {

    prj_path <- (system.file("extdata/benchmarks/flow_free_expansion",
                             "flow_free_expansion.prj", package = "r2ogs6"))

    ogs6_obj <- OGS6$new(sim_name = "sim",
                         sim_path = "sim_path")

    read_in(ogs6_obj,
            prj_path,
            "/OpenGeoSysProject/nonlinear_solvers/nonlinear_solver")

    expect_equal(length(ogs6_obj$nonlinear_solvers), 1)
    expect_equal(ogs6_obj$nonlinear_solvers[[1]]$name, "basic_newton")
    expect_equal(ogs6_obj$nonlinear_solvers[[1]]$type, "Newton")
    expect_equal(ogs6_obj$nonlinear_solvers[[1]]$max_iter, 50)
    expect_equal(ogs6_obj$nonlinear_solvers[[1]]$linear_solver,
                 "general_linear_solver")
})


test_that("read_in works for linear_solver objects", {

    prj_path <- (system.file("extdata/benchmarks/flow_free_expansion",
                             "flow_free_expansion.prj", package = "r2ogs6"))

    ogs6_obj <- OGS6$new(sim_name = "sim",
                         sim_path = "sim_path")

    read_in(ogs6_obj,
            prj_path,
            "/OpenGeoSysProject/linear_solvers/linear_solver")

    expect_equal(length(ogs6_obj$linear_solvers), 1)
    expect_equal(ogs6_obj$linear_solvers[[1]]$name, "general_linear_solver")
    expect_equal(ogs6_obj$linear_solvers[[1]]$lis,
                 "-i bicgstab -p ilu -tol 1e-16 -maxiter 10000")
    expect_equal(ogs6_obj$linear_solvers[[1]]$eigen$error_tolerance, 1e-16)
})


test_that("read_in works with newline value separation", {

    prj_path <- (system.file("extdata/benchmarks/LiakopoulosHM",
                             "liakopoulos.prj", package = "r2ogs6"))

    ogs6_obj <- OGS6$new(sim_name = "sim",
                         sim_path = "sim_path")

    read_in(ogs6_obj,
            prj_path,
            "/OpenGeoSysProject/curves/curve")

    expect_equal(length(ogs6_obj$curves), 1)
    expect_equal(length(ogs6_obj$curves[[1]]$coords), 61)
    expect_equal(ogs6_obj$curves[[1]]$coords[[1]], 0.2)
    expect_equal(length(ogs6_obj$curves[[1]]$values), 61)
    expect_equal(ogs6_obj$curves[[1]]$values[[1]], 0.0)
})


test_that("read_in_prj works for processes/include tags", {

    prj_path <- (system.file("extdata/benchmarks/Elliptic/circle_radius_1",
                             "circle_1e1_axi.prj", package = "r2ogs6"))

    ogs6_obj <- OGS6$new(sim_name = "sim",
                         sim_path = "sim_path")

    read_in_prj(ogs6_obj,
                prj_path)

    expect_equal(length(ogs6_obj$processes), 1)
    expect_equal(names(ogs6_obj$processes)[[1]], "include")

})


test_that("read_in_prj works for EmbeddedFracturePermeability/cube.prj", {

    prj_path <- (system.file("extdata/benchmarks/EmbeddedFracturePermeability/",
                             "cube.prj", package = "r2ogs6"))

    ogs6_obj <- OGS6$new(sim_name = "sim",
                         sim_path = "sim_path")

    read_in_prj(ogs6_obj,
                prj_path)

    expect_equal(ogs6_obj$processes[[1]]$name, "HM")
})
