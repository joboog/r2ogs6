
test_that("read_in_points correctly reads point tibble from file", {

    xml_doc <- validate_read_in_xml(system.file("extdata/flow_free_expansion",
                                                "cube_1x1x1.gml",
                                                package = "r2ogs6"))

    point_tibble <- read_in_points(xml_doc)
    expect_equal(length(point_tibble), 4)
    expect_equal(length(point_tibble[[1]]), 8)
    expect_setequal(names(point_tibble), c("x", "y", "z", "name"))
})


test_that("read_in_polylines correctly reads polyline list from file", {

    xml_doc <- validate_read_in_xml(system.file("extdata/flow_free_expansion",
                                                "cube_1x1x1.gml",
                                                package = "r2ogs6"))

    polyline_list <- read_in_polylines(xml_doc)

    expect_equal(length(polyline_list), 12)
    expect_equal(length(polyline_list[[1]]), 2)
    expect_equal(length(polyline_list[[1]][[2]]), 2)

    expect_equal(names(polyline_list[[1]][[2]]), c("pnt", "pnt"))
})


test_that("read_in_surfaces correctly reads surface list from file", {

    xml_doc <- validate_read_in_xml(system.file("extdata/flow_free_expansion",
                                                "cube_1x1x1.gml",
                                                package = "r2ogs6"))

    surface_list <- read_in_surfaces(xml_doc)

    expect_equal(length(surface_list), 6)
    expect_equal(length(surface_list[[1]]), 3)
    expect_equal(length(surface_list[[1]][[2]]), 3)
    expect_equal(length(surface_list[[1]][[3]]), 3)
})