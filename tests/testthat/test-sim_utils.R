
# This test may fail on your machine because ogs_bin_path is system dependent.
# Before you run it, alter ogs_bin_path to fit your system!


# test_that("run_simulation works", {
#
#     ogs_bin_path <- paste0("D:/Programme/OpenGeoSys/",
#                            "ogs-6.3.2-Windows-10.0.14393-x64-python-3.7.2",
#                            "-de-utils/bin/")
#
#     sim_path <- system.file("extdata/test_sim/", package = "r2ogs6")
#
#     # Define OGS6 object
#     ogs6_obj <- OGS6$new(sim_name = "sim",
#                          sim_id = 1,
#                          sim_path = "D:/OGS_Sim/",
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
#     expect_equal(file.exists(paste0(sim_path, "sim.prj")), TRUE)
#
#     # Check if log file was written
#     expect_equal(file.exists(paste0(sim_path, "sim_log.txt")), TRUE)
#
#     # Clean up folder
#     do.call(file.remove, list(list.files(sim_path, full.names = TRUE)))
# })


test_that("export_all_sim_files works", {

    extdata_path <- system.file("extdata/", package = "r2ogs6")
    test_path <- paste0(extdata_path, "/export_all_sim_files_test")
    dir.create(test_path)

    prj_path <- (system.file("extdata/benchmarks/Elliptic/circle_radius_1",
                             "circle_1e1_axi.prj", package = "r2ogs6"))

    ogs6_obj <- OGS6$new(sim_name = "circle_1e1_axi",
                         sim_id = 1,
                         sim_path = test_path,
                         ogs_bin_path = "ogs_bin_path",
                         test_mode = TRUE)

    read_in_prj(ogs6_obj,
                prj_path)

    # Now export all files
    export_all_sim_files(ogs6_obj)

    expect_equal(file.exists(paste0(test_path, "/circle_1e1_axi.prj")),
                 TRUE)

    expect_equal(file.exists(paste0(test_path,
                                    "/include/SteadyStateDiffusion.xml")),
                 TRUE)

    # Tidy up by deleting the folder we created
    unlink(test_path, recursive = TRUE)
})


