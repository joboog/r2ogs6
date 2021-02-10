
# helper function to skip tests if we don't have OpenGeoSys 6 dependencies
skip_if_ogs6_missing <- function() {

    ogs_path <- paste0(unlist(options("r2ogs6.default_ogs_bin_path")),
                       "ogs.exe")

    if(!file.exists(ogs_path)){
        skip("ogs.exe not available for testing")
    }

    skip("Skipping tests that call ogs_run_simulation()")
}


test_that("ogs_run_simulation works for flow_no_strain.prj", {

    skip_if_ogs6_missing()

    extdata_path <- system.file("extdata/test_tempdirs/", package = "r2ogs6")
    sim_path <- paste0(extdata_path, "/run_simulation_test")
    dir.create(sim_path)

    prj_path <- system.file("extdata/benchmarks/flow_no_strain/",
                            "flow_no_strain.prj",
                            package = "r2ogs6")

    # Define OGS6 object
    ogs6_obj <- OGS6$new(sim_name = "sim",
                         sim_id = 1,
                         sim_path = sim_path)

    # Read in .prj data
    read_in_prj(ogs6_obj, prj_path)

    # Run simulation
    e <- ogs_run_simulation(ogs6_obj)

    # Check exit code
    expect_equal(e, 0)

    # Check if logfile was written
    expect_equal(file.exists(paste0(ogs6_obj$sim_path,
                                    "logfiles/sim_log.txt")),
                 TRUE)

    # Tidy up by deleting the folder we created
    unlink(sim_path, recursive = TRUE)
})


#===== Export utility =====


test_that("ogs_export_sim_files works", {

    extdata_path <- system.file("extdata/test_tempdirs/", package = "r2ogs6")
    test_path <- paste0(extdata_path, "/export_all_sim_files_test")
    dir.create(test_path)

    prj_path <- (system.file("extdata/benchmarks/Elliptic/circle_radius_1",
                             "circle_1e1_axi.prj", package = "r2ogs6"))

    ogs6_obj <- OGS6$new(sim_name = "circle_1e1_axi",
                         sim_path = test_path)

    read_in_prj(ogs6_obj,
                prj_path)

    # Now export all files
    ogs_export_sim_files(ogs6_obj)

    expect_equal(file.exists(paste0(test_path, "/circle_1e1_axi.prj")),
                 TRUE)

    expect_equal(file.exists(paste0(test_path,
                                    "/include/SteadyStateDiffusion.xml")),
                 TRUE)

    # Tidy up by deleting the folder we created
    unlink(test_path, recursive = TRUE)
})


#===== Test benchmarks =====


test_that("run_benchmark works for flow_free_expansion.prj", {

    skip_if_ogs6_missing()

    extdata_path <- system.file("extdata/test_tempdirs/", package = "r2ogs6")
    sim_path <- paste0(extdata_path, "/run_benchmark_test")
    dir.create(sim_path)

    prj_path <- system.file("extdata/benchmarks/flow_free_expansion/",
                            "flow_free_expansion.prj",
                            package = "r2ogs6")

    run_benchmark(prj_path = prj_path,
                  sim_path = sim_path)

    first_ts_file <- paste0(sim_path, "/",
                            "flow_free_expansion_ts_0_t_0.000000.vtu")
    last_ts_file <- paste0(sim_path, "/",
                           "flow_free_expansion_ts_1000_t_10000.000000.vtu")

    expect_equal(file.exists(first_ts_file), TRUE)
    expect_equal(file.exists(last_ts_file), TRUE)

    # Tidy up by deleting the folder we created
    unlink(sim_path, recursive = TRUE)
})


test_that("get_benchmark_paths works", {

    # Source: HeatConduction/Tests.cmake
    tests_cmake_path <- system.file("extdata/benchmarks/tests_cmake/",
                                    package = "r2ogs6")

    benchmark_paths <- get_benchmark_paths(tests_cmake_path)

    expect_equal(length(benchmark_paths), 21)
    expect_equal(benchmark_paths[[1]],
                 "Parabolic/T/1D_dirichlet/line_60_heat.prj")

    # Test file with foreach loop for mesh_size...

})

