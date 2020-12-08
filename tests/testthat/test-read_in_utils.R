

# test_that("order_parameters works", {
#
#     #...
#
# })


test_that("guess_structure works", {

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


test_that("is_het_wrapper works", {

    ogs6_obj <- OGS6$new(sim_name = "sim",
                         sim_id = 1,
                         sim_path = "sim_path",
                         ogs_bin_path = "ogs_bin_path",
                         test_mode = TRUE)

    xml_doc <- xml2::read_xml(system.file("extdata/flow_free_expansion",
                                          "flow_free_expansion.prj", package = "r2ogs6"))

    time_stepping_node <- xml2::xml_find_first(xml_doc, "//time_stepping")

    expect_equal(is_het_wrapper(time_stepping_node), TRUE)

})


test_that("get_grandchild_length_vector works", {

    xml_doc <- xml2::read_xml("<a><b><c>1.1</c></b><b><c>2.1</c><c>2.2</c></b></a>")
    expect_equal(get_grandchild_length_vector(xml_doc), c(1, 2))

})


test_that("list_from_nodeset works", {

    xml_doc_1 <- xml2::read_xml("<a><b>1</b><b>2</b><b>3</b></a>")
    xml_nodeset_1 <- xml2::xml_children(xml_doc_1)

    xml_doc_2 <- xml2::read_xml("<a><b id=\"1\" name=\"first\"/><b id=\"2\" name=\"second\"/></a>")
    xml_nodeset_2 <- xml2::xml_children(xml_doc_2)

    expect_equal(list_from_nodeset(xml_nodeset_1), list(b = "1", b = "2", b = "3"))
    expect_equal(list_from_nodeset(xml_nodeset_2), list(b = c(id = "1", name = "first"),
                                                        b = c(id = "2", name = "second")))
})


test_that("find_read_in_func_call works", {

    node_name_1 <- "Mamma mia"
    node_name_2 <- "timesteps"

    expect_equal(find_read_in_func_call(node_name_1), "")
    expect_equal(find_read_in_func_call(node_name_2), "read_in_timesteps_node(xml_node)")

})