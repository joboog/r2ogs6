
#
# test_that("scrape_benchmarks works", {
#
#     scrape_benchmarks()
#
#     expect_equal(dir.exists("extdata/benchmarks/Elliptic/"), TRUE)
#
#     do.call(file.remove, list(list.files("extdata/benchmarks", full.names = TRUE)))
# })


test_that("download_benchmark works", {

    # Get extdata directory and create folder for the test

    extdata_path <- system.file("extdata/", package = "r2ogs6")

    path <- paste0(extdata_path, "/dl_benchmark_test")

    dir.create(path)

    download_benchmark(
        prj_url = paste0("https://gitlab.opengeosys.org/ogs/ogs/-/raw/master/",
                         "Tests/Data/HydroMechanics/IdealGas/flow_no_strain/",
                         "flow_no_strain.prj"),
        path = path)

    expect_equal(file.exists(paste0(path, "/flow_no_strain.prj")), TRUE)
    expect_equal(file.exists(paste0(path, "/square_1x1.gml")), TRUE)
    expect_equal(file.exists(paste0(path, "/square_1x1_quad8_1e2.vtu")), TRUE)

    # Tidy up by deleting the folder we created

    unlink(path, recursive = TRUE)
})
