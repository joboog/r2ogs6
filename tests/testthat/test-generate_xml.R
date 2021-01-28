
test_that("build_redux_xml works", {

    prj_path <- (system.file("extdata/benchmarks/flow_free_expansion",
                             package = "r2ogs6"))

    redux_node <- build_redux_tree(path = prj_path,
                                   pattern = "\\.prj$",
                                   xpath = "/OpenGeoSysProject",
                                   required = TRUE)

    redux_doc <- xml2::as_xml_document(redux_node)
    mesh_node <- xml2::xml_find_first(redux_doc, "//mesh")
    expect_equal(xml2::xml_attrs(mesh_node), c(required = "TRUE",
                                               read_content_as = "string"))
})
