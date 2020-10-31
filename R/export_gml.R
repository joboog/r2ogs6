#This script contains various functions to turn data for a .gml file into the correct XML format


#' Wrapper function to create a .gml XML document based on the user input data
#' @param gml_obj A gml class object
#' @return A XML document ready for export to a file
# @examples (WIP)
gml_data_to_xml <- function(gml_obj) {

  validate_gml_data(gml_obj)

  data_node <- xml2::xml_new_root(.value = "OpenGeoSysGLI",
                    "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance",
                    "xmlns:ogs" = "http://www.opengeosys.org")

  xml2::xml_add_child(data_node, xml2::as_xml_document(list(name = list(gml_obj$geometry_name))))

  if(!is.null(gml_obj$points)){
    xml2::xml_add_child(data_node, points_to_xml(gml_obj$points))
  }

  if(!is.null(gml_obj$polylines)){
    xml2::xml_add_child(data_node, polylines_to_xml(gml_obj$polylines))
  }

  if(!is.null(gml_obj$surfaces)){
    xml2::xml_add_child(data_node, surfaces_to_xml(gml_obj$surfaces))
  }

  return(data_node)
}


#' Turns a tibble of points into an XML node
#' @param point_tibble The specified tibble
#' @return An XML node containing the points
#' @examples
#' my_tibble <- tibble::tibble(x = c(0, 0), y = c(0, 0), z = c(0, 1), name = c("origin", ""))
#' point_node <- points_to_xml(my_tibble)
#' @export
points_to_xml <- function(point_tibble) {
  points_node <- list(points = list())
  has_names <- (length(point_tibble) == 4)

  for(i in 1:length(point_tibble[[1]])){
    if(!has_names || point_tibble[[4]][[i]] == "") {
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
                                                                     name = point_tibble[[4]][[i]])))
    }
  }

  return(xml2::as_xml_document(points_node))
}

#' Turns a list of polylines into an XML node
#' @param polylines A list of polylines
#' @return An XML node containing the polylines
#' @examples
#' my_list <- list(list(name = "front_left", c(0, 1)), list(name = "front_right", c(4, 5)))
#' polylines_node <- polylines_to_xml(my_list)
#' @export
polylines_to_xml <- function(polylines) {
  polylines_node <- list(polylines = list())

  for(i in 1:length(polylines)){

    pnt_list <- list()

    for(j in 1:length(polylines[[i]][[2]])) {
      pnt_list <- c(pnt_list, list(pnt = list(polylines[[i]][[2]][[j]])))
    }

    polylines_node[[1]] <- c(polylines_node[[1]], list(polyline = structure(pnt_list,
                                                                            id = (i-1),
                                                                            name = polyline_list[[i]][[1]])))
  }

  return(xml2::as_xml_document(polylines_node))
}

#' Turns a list of surfaces into an XML node
#' @param surfaces A list of surfaces
#' @return An XML node containing the surfaces
#' @examples
#' my_list <- list(list(name = "left", c(0, 1, 2), c(0, 3, 2)),
#' list(name = "right", c(4, 6, 5), c(4, 6, 7)))
#' surfaces_node <- surfaces_to_xml(my_list)
#' @export
surfaces_to_xml <- function(surfaces) {
  surfaces_node <- list(surfaces = list())

  for(i in 1:length(surfaces)){
    surfaces_node[[1]] <- c(surfaces_node[[1]], list(surface = structure(c(list(element = structure(list(),
                                                                                                    p1 = surfaces[[i]][[2]][[1]],
                                                                                                    p2 = surfaces[[i]][[2]][[2]],
                                                                                                    p3 = surfaces[[i]][[2]][[3]])),
                                                                           list(element = structure(list(),
                                                                                                    p1 = surfaces[[i]][[3]][[1]],
                                                                                                    p2 = surfaces[[i]][[3]][[2]],
                                                                                                    p3 = surfaces[[i]][[3]][[3]]))),
                                                                         id = (i-1),
                                                                         name = surfaces[[i]][[1]])))
  }

  return(xml2::as_xml_document(surfaces_node))
}

