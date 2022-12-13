

test_that("node_to_object works for simple r2ogs6 classes", {

    prj_path <- (system.file("extdata/benchmarks/flow_free_expansion",
                             "flow_free_expansion.prj", package = "r2ogs6"))
    xml_doc <- validate_read_in_xml(prj_path)

    ogs6_obj <- OGS6$new(sim_name = "sim",
                         sim_path = "sim_path")

    read_in(ogs6_obj, xml_doc, "/OpenGeoSysProject/parameters/parameter")

    expect_equal(length(ogs6_obj$parameters), 7)
    expect_equal(ogs6_obj$parameters[[1]]$name, "E")
    expect_equal(ogs6_obj$parameters[[1]]$type, "Constant")
    expect_equal(ogs6_obj$parameters[[1]]$value, 10e9)
})


test_that("node_to_object works for nodes that have both attributes and text", {

    vtu_path <- system.file("extdata/benchmarks/flow_free_expansion",
                            "cube_1x1x1.vtu",
                            package = "r2ogs6")

    test_node <- xml2::read_xml("<test a = \"1\">some text</test>")

    test_obj <- node_to_object(test_node,
                               xpath = "/test")


    expect_equal(length(test_obj), 2)
    expect_equal(test_obj[["a"]], "1")
    expect_equal(test_obj[["xml_text"]], "some text")
})


test_that("node_to_object works for simple lists", {

    my_xml <- xml2::read_xml("<a><b>1</b><b>2</b></a>")
    my_node <- xml2::xml_find_first(my_xml, "/a")

    my_list <- node_to_object(my_xml, "/a")

    expect_equal(my_list, list(b = "1", b = "2"))
})

test_that("node_to_object prints warning for empty tags", {

    test_node <- xml2::read_xml("<test> </test>")

    expect_warning(node_to_object(test_node, xpath = "/test"))
})

test_that("order_parameters works for classes with Ellipsis argument", {

    ogs6_parameter <- prj_parameter(name = "test",
                                      type = "test",
                                      index_values = list("1", "1 2"))

    parameters <- list(type = "test",
                       index_values = list("1", "1 2"),
                       name = "test")

    class_name <- "prj_parameter"

    ordered_parameters <- order_parameters(parameters,
                                           class_name)

    expect_equal(ordered_parameters, list(name = "test",
                                          type = "test",
                                          index_values = list("1", "1 2")))
})

test_that("order_parameters throws a helpful error message for missing
          class arguments", {
    # e.g. a new process node in a *.prj file is found and the corresponding
    # class in prj_process is missing

    # use a class that does not have "..."
    ogs6_class <- prj_constitutive_relation(type = "test")
    # replace one parameter
    names(ogs6_class)[4] <- "Frodos_Ring"
    expect_error(
        order_parameters(parameters = ogs6_class,
                         class_name = "prj_constitutive_relation"),
        regexp = paste("Frodos_Ring not in class_args of class",
                        "prj_constitutive_relation"))
          }
)

