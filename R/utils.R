
default_xml_version <- "1.0"
default_xml_encoding <- "ISO-8859-1"

#============================== GENERAL UTILITY (copy playground) ================================

###Stub
# Stub <- R6::R6Class("Stub",
#   public = list(
#
#     initialize = function(argument) {
#       #stopifnot(is.character(name), length(name) == 1)
#       #...
#     }
#   ),
#
#   active = list(
#
#   ),
#
#   private = list(
#
#   )
# )

#============================== XML UTILITY FUNCTIONS ================================

#' Reads the encoding of an XML file
#'
#' @param input_file_name An XML (.xml or, in the case of OpenGeoSys, .prj) file
#' @return The encoding of the input file
# @examples
# input_file_encoding <- get_xml_encoding("file.xml")
# input_file_encoding <- get_xml_encoding("file.prj")
get_xml_encoding <- function(input_file_name) {
  input_file <- file(input_file_name, "r")
  first_line <- readLines(input_file, n=1)
  close(input_file)
  decl <- stringr::str_split(first_line, '"', simplify = TRUE)

  xml_encoding <- default_xml_encoding

  if(length(decl) == 5 && decl[1] == "<?xml version=" && decl[3] == " encoding=" && decl[5]=="?>"){
    xml_encoding <- decl[4]
    cat(paste("Valid XML declaration in first line of input file found.\nDetected encoding ", xml_encoding))

  }else{
    cat("XML declaration not as expected. Example for valid declaration:\n")
    cat('<?xml version="1.0" encoding="UTF-8"?>\n')
    cat(paste("I will use my default value of XML encoding ",  default_xml_encoding, ".\n"))

  }
  return(xml_encoding)
}


#' Creates a template project file based on the XML structure and attributes of an existing project file.
#' @param existing_prj An existing .prj file to use as a template for the structure and attributes
#' @param template_prj The path where the template should be saved
#' @return The newly created template project file
# @examples
# blank_prj_file <- create_template_prj("file.prj")
create_template_prj <- function(existing_prj, template_prj) {
  xml_input <- xml2::read_xml(existing_prj)

  #Empty text nodes
  text_nodes<-xml2::xml_find_all(xml_input, "//text()")
  xml2::xml_set_text(text_nodes, "")

  xml2::write_xml(xml_input, template_prj, options = "format", encoding=get_xml_encoding(existing_prj))
}


