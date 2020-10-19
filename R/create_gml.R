#This script contains various functions to turn data for a .gml file into the correct XML format

#' Export function
#' @param gml_data The .gml data (already in XML friendly format)
#' @param file_name The name of the .gml file to be written
# @examples
# export_gml_to_file(my_gml)
#' @export
export_gml_to_file <- function(gml_data, file_name) {
  doc <- xml2::as_xml_document(gml_data)
  xml2::write_xml(doc, file_name, options = "format", encoding="ISO-8859-1")
  invisible()
}


#' Wrapper function to create a XML document based on the user input data
#' @param geo_name The name of the geometry specified by the user
#' @param points_tibble A tibble containing points
#' @param polylines_list A list containing polylines
#' @param surfaces_list A list containing surfaces
#' @return A XML document ready for export to a file
# @examples (WIP)
#' @export
data_to_xml <- function(geo_name, points_tibble, polylines_list, surfaces_list) {

  #data_node <- xml2::read_xml('<OpenGeoSysGLI xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:ogs="http://www.opengeosys.org"/>')

  data_node <- xml2::xml_new_root(.value = "OpenGeoSysGLI",
                    "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance",
                    "xmlns:ogs" = "http://www.opengeosys.org")

  xml2::xml_add_child(data_node, xml2::as_xml_document(list(name = list(geo_name))))
  xml2::xml_add_child(data_node, points_to_xml(points_tibble))
  xml2::xml_add_child(data_node, polylines_to_xml(polylines_list))
  xml2::xml_add_child(data_node, surfaces_to_xml(surfaces_list))

  return(data_node)
}


#' Turns a tibble of points into an XML node
#' @param point_tibble The specified tibble
#' @return An XML node containing the points
#' @examples
#' my_tibble <- tibble::tibble(x = c(0, 0), y = c(0, 0), z = c(0, 1),is_origin = c(TRUE, FALSE))
#' point_node <- points_to_xml(my_tibble)
#' @export
points_to_xml <- function(point_tibble) {
  points_node <- list(points = list())

  for(i in 1:length(point_tibble[[1]])){
    if(!point_tibble[[4]][[i]]) {
      points_node[[1]] <- c(points_node[[1]], list(point = structure(list(),
                                                                     id = (i-1),
                                                                     x = point_tibble[[1]][[i]],
                                                                     y = point_tibble[[2]][[i]],
                                                                     z = point_tibble[[3]][[i]])))
    }else{
      points_node[[1]] <- c(points_node[[1]], list(point = structure(list(),
                                                                     id = (i-1),
                                                                     x = point_tibble[[1]][[i]],
                                                                     y = point_tibble[[2]][[i]],
                                                                     z = point_tibble[[3]][[i]],
                                                                     name = "origin")))
    }
  }

  return(xml2::as_xml_document(points_node))
}

#' Turns a list of polylines into an XML node
#' @param polyline_list The specified list
#' @return An XML node containing the polylines
#' @examples
#' my_list <- list(list(name = "front_left", c(0, 1)), list(name = "front_right", c(4, 5)))
#' polylines_node <- polylines_to_xml(my_list)
#' @export
polylines_to_xml <- function(polyline_list) {
  polylines_node <- list(polylines = list())

  for(i in 1:length(polyline_list)){
    polylines_node[[1]] <- c(polylines_node[[1]], list(polyline = structure(list(pnt = list(polyline_list[[i]][[2]][[1]]),
                                                                                 pnt = list(polyline_list[[i]][[2]][[2]])),
                                                                            id = (i-1),
                                                                            name = polyline_list[[i]][[1]])))
  }

  return(xml2::as_xml_document(polylines_node))
}

#' Turns a list of surfaces into an XML node
#' @param surfaces_list The specified list
#' @return An XML node containing the surfaces
#' @examples
#' my_list <- list(list(name = "left", c(0, 1, 2), c(0, 3, 2)),
#' list(name = "right", c(4, 6, 5), c(4, 6, 7)))
#' surfaces_node <- surfaces_to_xml(my_list)
#' @export
surfaces_to_xml <- function(surfaces_list) {
  surfaces_node <- list(surfaces = list())

  for(i in 1:length(surfaces_list)){
    surfaces_node[[1]] <- c(surfaces_node[[1]], list(surface = structure(c(list(element = structure(list(),
                                                                                                    p1 = surfaces_list[[i]][[2]][[1]],
                                                                                                    p2 = surfaces_list[[i]][[2]][[2]],
                                                                                                    p3 = surfaces_list[[i]][[2]][[3]])),
                                                                           list(element = structure(list(),
                                                                                                    p1 = surfaces_list[[i]][[3]][[1]],
                                                                                                    p2 = surfaces_list[[i]][[3]][[2]],
                                                                                                    p3 = surfaces_list[[i]][[3]][[3]]))),
                                                                         id = (i-1),
                                                                         name = surfaces_list[[i]][[1]])))
  }

  return(xml2::as_xml_document(surfaces_node))
}

