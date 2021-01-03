

# test_that("order_parameters works", {
#
#     #...
#
# })


test_that("guess_structure works for simple r2ogs6 classes", {

    prj_path <- (system.file("extdata/flow_free_expansion",
                             "flow_free_expansion.prj", package = "r2ogs6"))

    ogs6_obj <- OGS6$new(sim_name = "sim",
                         sim_id = 1,
                         sim_path = "sim_path",
                         ogs_bin_path = "ogs_bin_path",
                         test_mode = TRUE)

    read_in(ogs6_obj, prj_path, "/OpenGeoSysProject/parameters/parameter")

    expect_equal(length(ogs6_obj$parameters), 7)
    expect_equal(ogs6_obj$parameters[[1]]$name, "E")
    expect_equal(ogs6_obj$parameters[[1]]$type, "Constant")
    expect_equal(ogs6_obj$parameters[[1]]$value, 10e9)
})


test_that("guess_structure works for simple lists", {

    my_xml <- xml2::read_xml("<a><b>1</b><b>2</b></a>")
    my_node <- xml2::xml_find_first(my_xml, "/a")

    my_list <- guess_structure(my_xml, "/a")

    expect_equal(my_list, list(b = "1", b = "2"))
})


test_that("order_parameters works for classes with Ellipsis argument", {

    ogs_parameter <- r2ogs6_parameter(name = "test",
                                      type = "test",
                                      index_values = list("1", "1 2"))

    parameters <- list(type = "test",
                       index_values = list("1", "1 2"),
                       name = "test")

    class_name <- "r2ogs6_parameter"

    ordered_parameters <- order_parameters(parameters,
                                           class_name)

    expect_equal(ordered_parameters, list(name = "test",
                                          type = "test",
                                          index_values = list("1", "1 2")))
})


