
test_that("read_gml_points correctly reads point tibble from file", {

    point_tibble_1 <- read_gml_points(system.file("extdata", "gml_points_with_names.tsv", package = "r2ogs6"))
    expect_equal(length(point_tibble_1), 4)
    expect_setequal(names(point_tibble_1), c("x", "y", "z", "name"))

    point_tibble_2 <- read_gml_points(system.file("extdata", "gml_points_without_names.tsv", package = "r2ogs6"))
    expect_equal(length(point_tibble_2), 3)
    expect_setequal(names(point_tibble_2), c("x", "y", "z"))
})

