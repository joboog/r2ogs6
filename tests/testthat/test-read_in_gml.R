
test_that("read_in_points correctly reads point tibble from file", {

    xml_doc <- validate_read_in_xml(system.file("extdata", "cube_1x1x1.gml", package = "r2ogs6"))

    point_tibble <- read_in_points(xml_doc)
    expect_equal(length(point_tibble), 4)
    expect_equal(length(point_tibble[[1]]), 8)
    expect_setequal(names(point_tibble), c("x", "y", "z", "name"))
})