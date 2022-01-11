

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
    incl_path <- (system.file("extdata/benchmarks/Elliptic/other_dir",
                             "SteadyStateDiffusion.xml", package = "r2ogs6"))

    ogs6_obj <- OGS6$new(sim_name = "sim",
                         sim_path = "sim_path")

    read_in_prj(ogs6_obj,
                prj_path)

    expect_equal(length(ogs6_obj$processes), 1)
    expect_equal(names(ogs6_obj$processes)[[1]], "include")
    expect_equal(ogs6_obj$processes$include[["file"]], incl_path)
})


test_that("read_in_prj works for EmbeddedFracturePermeability/cube.prj", {

    prj_path <- (system.file("extdata/benchmarks/EmbeddedFracturePermeability/",
                             "cube.prj", package = "r2ogs6"))

    ogs6_obj <- OGS6$new(sim_name = "sim",
                         sim_path = "sim_path")

    read_in_prj(ogs6_obj,
                prj_path,
                read_in_gml = T)

    expect_equal(ogs6_obj$processes[[1]]$name, "HM")
})

test_that("read_in works for python_script objects", {

    prj_base_path <- system.file(
        "extdata/benchmarks/Elliptic/square_1x1_SteadyStateDiffusion_Python",
        package = "r2ogs6")
    prj_path <- paste0(prj_base_path, "/square_1e3_laplace_eq.prj")
    py_path <- paste0(prj_base_path, "/bcs_laplace_eq.py")

    ogs6_obj <- OGS6$new(sim_name = "sim",
                         sim_path = "sim_path")

    read_in_prj(ogs6_obj,
                prj_path)

    expect_equal(length(ogs6_obj$python_script), 1)
    expect_equal(ogs6_obj$python_script, py_path)

})

test_that("read_in works for geometry and vtu meshes", {

    # read gml, vtu
    prj_base_path <- system.file(
        "extdata/benchmarks/Elliptic/square_1x1_SteadyStateDiffusion_Python",
        package = "r2ogs6")
    prj_path <- paste0(prj_base_path, "/square_1e3_laplace_eq.prj")
    gml_path <- paste0(prj_base_path, "/square_1x1.gml")
    vtu_path <- paste0(prj_base_path, "/square_1x1_quad_1e3.vtu")

    ogs6_obj <- OGS6$new(sim_name = "sim",
                         sim_path = "sim_path")

    read_in_prj(ogs6_obj,
                prj_path,
                read_in_gml = F)

    expect_equal(ogs6_obj$geometry, gml_path)
    expect_equal(ogs6_obj$meshes$mesh$path, vtu_path)

    rm(ogs6_obj)

    # read vtus
    prj_base_path <- system.file("extdata/benchmarks/CationExchange/",
                                package = "r2ogs6")
    prj_path <- paste0(prj_base_path, "/exchange.prj")
    vtu_path1 <- paste0(prj_base_path, "/exchange.vtu")
    vtu_path2 <- paste0(prj_base_path, "/exchange_upstream.vtu")
    vtu_path3 <- paste0(prj_base_path, "/exchange_downstream.vtu")
    vtu_path4 <- paste0(prj_base_path, "/ReactiveDomain_exchange.vtu")

    ogs6_obj <- OGS6$new(sim_name = "sim",
                         sim_path = "sim_path")

    read_in_prj(ogs6_obj,
                prj_path,
                read_in_gml = F)

    expect_equal(ogs6_obj$meshes[[1]]$path, vtu_path1)
    expect_equal(ogs6_obj$meshes[[2]]$path, vtu_path2)
    expect_equal(ogs6_obj$meshes[[3]]$path, vtu_path3)
    expect_equal(ogs6_obj$meshes[[4]]$path, vtu_path4)
})

test_that("read_in works for chemical_system objects", {

    prj_base_path <- system.file("extdata/benchmarks/CationExchange/",
                                 package = "r2ogs6")
    prj_path <- paste0(prj_base_path, "/exchange.prj")
    dat_path <- paste0(prj_base_path, "/phreeqc.dat")

    ogs6_obj <- OGS6$new(sim_name = "sim",
                         sim_path = "sim_path")

    read_in_prj(ogs6_obj, prj_path)

    expect_equal(is.null(ogs6_obj$chemical_system$database), FALSE)
    expect_equal(ogs6_obj$chemical_system$chemical_solver, "Phreeqc")
    expect_equal(ogs6_obj$chemical_system$database, dat_path)
    expect_equal(class(ogs6_obj$chemical_system$solution), "prj_solution")
    expect_equal(length(ogs6_obj$chemical_system$solution), 8)
    expect_equal(ogs6_obj$chemical_system$solution$temperature, 25)
    expect_equal(length(ogs6_obj$chemical_system$knobs), 5)
})