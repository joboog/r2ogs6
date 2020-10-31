#This script contains various functions to turn data for a .vtu file into the correct XML format


#' Wrapper function to create a .vtu XML document based on the user input data
#' @param vtu_obj A vtu class object
#' @return A XML document ready for export to a file
# @examples (WIP)
vtu_data_to_xml <- function(vtu_obj) {

    validate_vtu_data(vtu_obj)

    data_node <- list(VTKFile = structure(list(),
                                          type = vtu_obj$type,
                                          version = vtu_obj$version,
                                          byte_order = vtu_obj$byte_order,
                                          header_type = vtu_obj$header_type))

    data_node <- add_opt_attr(data_node, vtu_obj$compressor, "compressor")

    #Add all the data here... (WIP)

}


vtu_unstructured_grid_to_xml <- function() {

    unstructured_grid_node <- list(UnstructuredGrid = list())

    field_data_xml <- adopt_nodes("FieldData", field_data_data_arrays)

}

#'(WIP, change input parameter to piece!!)
#'@param piece A list consisting of lists of data_array class objects
vtu_piece_to_xml <- function(number_of_points, number_of_cells, point_data, cell_data, points, cells) {

    piece_node <- list(piece = structure(list(adopt_nodes("PointData", point_data),
                                              adopt_nodes("CellData", cell_data),
                                              adopt_nodes("Points", points),
                                              adopt_nodes("Cells", cells)),
                                         NumberOfPoints = number_of_points,
                                         NumberOfCells = number_of_cells))

    return(xml2::as_xml_document(piece_node))
}


#(WIP)
vtu_data_array_to_xml <- function(data_array) {
    data_array_node <- list(DataArray = structure(list(),
                                                  type,
                                                  Name,
                                                  format,
                                                  rangeMin,
                                                  rangeMax))


    if(1){

    }

    return(xml2::as_xml_document(data_array_node))
}

#'vtu_appended_data_to_xml
#'@name vtu_appended_data_to_xml
#'(WIP)
#'@param appended_data A list object consisting of 2 elements named 'data' and 'encoding'
#'
vtu_appended_data_to_xml <- function(appended_data) {

    return(list(AppendedData = structure(list(appended_data[[data]]),
                                         encoding = appended_data[[encoding]])))
}

