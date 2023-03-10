
# helper function to skip tests if we don't have OpenGeoSys 6 dependencies
skip_if_ogs6_missing <- function() {

    ogs6_path <- unlist(options("r2ogs6.default_ogs6_bin_path"))
    print(ogs6_path)
    if(is.null(ogs6_path) || !file.exists(ogs6_path)){
        skip("ogs executable not available for testing")
    }

    #skip("Skipping tests that call ogs6_run_simulation()")
}


test_that("ogs6_run_simulation works for flow_no_strain.prj", {

    skip_if_ogs6_missing()

    #extdata_path <- system.file("extdata/test_tempdirs/", package = "r2ogs6")
    sim_path <- paste0(tmp_dir, "/run_simulation_test")
    dir.create(sim_path)

    prj_path <- system.file("extdata/benchmarks/flow_no_strain/",
                            "flow_no_strain.prj",
                            package = "r2ogs6")

    # Define OGS6 object
    ogs6_obj <- OGS6$new(sim_name = "sim", sim_path = sim_path)

    # Read in .prj data
    read_in_prj(ogs6_obj, prj_path, read_in_gml = T)

    # Run simulation
    e <- ogs6_run_simulation(ogs6_obj)

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


test_that("ogs6_export_sim_files works", {

    test_path <- paste0(tmp_dir, "/export_all_sim_files_test")
    dir.create(test_path)

    prj_path <- (system.file("extdata/benchmarks/Elliptic/circle_radius_1",
                             "circle_1e1_axi.prj", package = "r2ogs6"))

    ogs6_obj <- OGS6$new(sim_name = "circle_1e1_axi",
                         sim_path = test_path)

    read_in_prj(ogs6_obj,
                prj_path,
                read_in_gml = T)

    # Now export all files
    ogs6_export_sim_files(ogs6_obj, copy_ext_files = T)

    # Trying to overwrite should yield an error
    expect_error(ogs6_export_sim_files(ogs6_obj, overwrite = F))

    expect_equal(file.exists(paste0(test_path, "/circle_1e1_axi.prj")),
                 TRUE)

    expect_equal(file.exists(paste0(test_path,
                                    "/SteadyStateDiffusion.xml")),
                 TRUE)

    # Tidy up by deleting the folder we created
    unlink(test_path, recursive = TRUE)
})

test_that("Nonexistent *.pvd file yields an appropriate error", {
    sim_path <- paste0(tmp_dir, "/run_simulation_test")
    dir.create(sim_path)

    # create empty ogs6 object
    ogs6_obj <- OGS6$new(sim_name = "sim", sim_path = sim_path)
    expect_error(ogs6_read_output_files(ogs6_obj))

    unlink(sim_path, recursive = TRUE)
})


test_that("Small *.pvd file yields an appropriate error", {
    sim_path <- paste0(tmp_dir, "/run_simulation_test")
    dir.create(sim_path)
    d <- NULL
    save(d, file = paste0(sim_path, "/null.pvd"))

    # create empty ogs6 object
    ogs6_obj <- OGS6$new(sim_name = "sim", sim_path = sim_path)
    expect_error(ogs6_read_output_files(ogs6_obj))

    unlink(sim_path, recursive = TRUE)
})


#===== Test benchmarks =====


test_that("run_benchmark works for flow_free_expansion.prj", {

    skip_if_ogs6_missing()

    #extdata_path <- system.file("extdata/test_tempdirs/", package = "r2ogs6")
    sim_path <- paste0(tmp_dir, "/run_benchmark_test")
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

