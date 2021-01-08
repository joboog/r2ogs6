
#===== read_in_vtu =====


#'read_in_vtu
#'@description Wrapper function to read in a whole .vtu file
#'@param vtu_path string: Path to .vtu file that should be read in
#'@return OGS6_vtu: Mesh object
#'@export
read_in_vtu <- function(vtu_path) {

    xml_doc <- validate_read_in_xml(vtu_path)
    xpath_expr <- "/VTKFile"

    root_node <- xml2::xml_find_first(xml_doc, xpath_expr)

    vtu_obj <-
        node_to_r2ogs6_class_object(xml_node = root_node,
                                    xpath_expr = xpath_expr,
                                    subclasses_names =
                                        get_subclass_names("OGS6_vtu"))
    return(invisible(vtu_obj))
}