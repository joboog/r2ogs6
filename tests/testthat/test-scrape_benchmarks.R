
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

    path <- system.file("extdata/benchmarks/", package = "r2ogs6")

    download_benchmark(
        prj_url = paste0("https://gitlab.opengeosys.org/ogs/ogs/-/raw/master/",
                         "Tests/Data/HydroMechanics/IdealGas/flow_no_strain/",
                         "flow_no_strain.prj"),
        path = path)

    expect_equal(file.exists(paste0(path, "/flow_no_strain.prj")), TRUE)

    do.call(file.remove, list(list.files(path, full.names = TRUE)))
})