

test_that("generate_as_node_func correctly generates as_node function from XML element", {

    simple_xml_file <- system.file("extdata", "simple_case_1.xml", package = "r2ogs6")

    # xml_doc <- xml2::read_xml(simple_xml_file)
    #
    # children <- xml2::xml_find_first(xml_doc, paste("//", "a", sep = ""))
    #
    # children

    generate_as_node_func(simple_xml_file, "b", show_result = FALSE)

})
