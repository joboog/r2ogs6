
# This test may fail on your machine because ogs_bin_path is system dependent.
# Before you run it, alter ogs_bin_path to fit your system!


# test_that("run_simulation works", {
#
#     ogs_bin_path <- paste0("D:\\Programme\\OpenGeoSys\\",
#                            "ogs-6.3.2-Windows-10.0.14393-x64-python-3.7.2","
#                            -de-utils\\bin\\")
#
#     sim_path <- system.file("extdata/test_sim/", package = "r2ogs6")
#     sim_path <- validate_is_dir_path(sim_path)
#
#     # Define OGS6 object
#     ogs6_obj <- OGS6$new(sim_name = "sim",
#                          sim_id = 1,
#                          sim_path = simdir_path,
#                          ogs_bin_path = ogs_bin_path)
#
#     # Define path to .prj file
#     prj_path <- (system.file("extdata/flow_free_expansion",
#                              "flow_free_expansion.prj", package = "r2ogs6"))
#
#
#     # Read in .prj data
#     read_in_prj(ogs6_obj, prj_path)
#
#     # Run simulation
#     run_simulation(ogs6_obj)
#
#     # Check if output file was written
#     expect_equal(file.exists(paste0(sim_path, "flow_no_strain.prj")), TRUE)
#
#     # Check if log file was written
#     expect_equal(file.exists(paste0(sim_path, "sim_log.txt")), TRUE)
#
#     # Clean up folder
#     do.call(file.remove, list(list.files(sim_path, full.names = TRUE)))
# })


test_that("setup_logging works", {

    # Create placeholder sim parameters
    sim_name <- "test"
    ogs6_call <- "ogs6 call"

    # Create placeholder sim_path directory
    extdata_path <- system.file("extdata/", package = "r2ogs6")

    sim_path <- paste0(extdata_path, "/log_test")
    dir.create(sim_path)

    batch_str <- setup_logging(sim_name,
                               sim_path,
                               ogs6_call)

    # Check if logfile directory was written
    expect_equal(dir.exists(paste0(sim_path, "/logfiles")), TRUE)

    script_path <- paste0(sim_path, "/logfiles/sim_init.R")

    # Check if initialization script was written
    expect_equal(file.exists(script_path), TRUE)

    # Check contents of initialization script
    expect_equal(readLines(script_path), "system(command = \"ogs6 call\")")

    # Clean up folder
    unlink(sim_path, recursive = TRUE)
})


