
test_that("export_prj works", {

    # Get extdata directory and create folder for the test
    extdata_path <- system.file("extdata/", package = "r2ogs6")
    test_path <- paste0(extdata_path, "/export_prj_test")
    dir.create(test_path)

    # Define prj_path and OGS6 object, then read in .prj file
    ogs6_obj <- OGS6$new(sim_name = "flow_free_expansion",
                         sim_path = test_path)


    prj_path <- (system.file("extdata/benchmarks/flow_free_expansion",
                             "flow_free_expansion.prj", package = "r2ogs6"))

    read_in_prj(ogs6_obj, prj_path, read_in_gml = T)

    # Now export it
    export_prj(ogs6_obj)

    expect_equal(file.exists(paste0(test_path, "/flow_free_expansion.prj")),
                 TRUE)

    # Tidy up by deleting the folder we created
    unlink(test_path, recursive = TRUE)
})
