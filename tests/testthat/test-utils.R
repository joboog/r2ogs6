
test_that("get_xml_encoding returns valid XML encoding", {
  expect_equal(get_xml_encoding(system.file("extdata", "flow_free_expansion.prj", package = "r2ogs6")), "ISO-8859-1")
})
#> Test passed

test_that("create_blank_prj writes .prj file with empty XML text nodes", {
  template_file <- system.file("extdata", "flow_free_expansion_template.prj", package = "r2ogs6")
  create_template_prj(system.file("extdata", "flow_free_expansion.prj", package = "r2ogs6"), template_file)

  xml_templ <- xml2::read_xml(template_file)

  #Get text nodes
  text_nodes<-xml2::xml_find_all(xml_templ, "//text()")

  #There should not be any text left in the text nodes
  expect_equal(xml2::xml_text(text_nodes), character(0))
})
#> Test passed
