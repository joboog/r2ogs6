#This script contains various functions to turn data for a .vtu file into the correct XML format

vtu_data_to_xml <- function() {


}



vtu_piece_to_xml <- function(n_points, n_cells, point_data, cell_data, points, cells) {
    piece_node <- list(piece = structure(list(),
                                         NumberOfPoints = 0,
                                         NumberOfCells = 0))
    return(xml2::as_xml_document(piece_node))
}

