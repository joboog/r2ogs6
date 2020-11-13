#This script contains various functions to turn data for a .gml file into the correct XML format


#'export_gml
#'@description Wrapper function to create a .gml XML document based on the user input data
#'@param ogs6_obj A OGS6 class object
export_gml <- function(ogs6_obj) {

  gml_xml <- xml2::xml_new_root(.value = "OpenGeoSysGLI",
                                "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance",
                                "xmlns:ogs" = "http://www.opengeosys.org")

  xml2::xml_add_child(gml_xml, xml2::as_xml_document(list(name = list(ogs6_obj$gml$name))), .copy = FALSE)

  xml2::xml_add_child(gml_xml, points_to_xml(ogs6_obj$gml$points), .copy = FALSE)

  if(!is.null(ogs6_obj$gml$polylines)){
    xml2::xml_add_child(gml_xml, polylines_to_xml(ogs6_obj$gml$polylines), .copy = FALSE)
  }

  if(!is.null(ogs6_obj$gml$surfaces)){
    xml2::xml_add_child(gml_xml, surfaces_to_xml(ogs6_obj$gml$surfaces), .copy = FALSE)
  }

  file <- paste0(ogs6_obj$sim_path, ogs6_obj$gml$name, ".gml")

  xml2::write_xml(gml_xml, file, options = "format", encoding="ISO-8859-1")

  return(invisible())
}


#'points_to_xml
#'@description Turns a tibble of points into an XML node
#'@param points The specified tibble
#'@return An XML node containing the points
points_to_xml <- function(points) {
  points_node <- list(points = list())
  has_names <- (length(points) == 4)

  for(i in 1:length(points[[1]])){
    point_node <- list(point = structure(list(),
                                        id = (i-1),
                                        x = points[[1]][[i]],
                                        y = points[[2]][[i]],
                                        z = points[[3]][[i]]))

    if(has_names && points[[4]][[i]] != ""){
      attributes(point_node[[1]]) <- c(attributes(point_node[[1]]), name = points[[4]][[i]])
    }

    points_node[[1]] <- c(points_node[[1]], point_node)
  }

  return(xml2::as_xml_document(points_node))
}


#'polylines_to_xml
#'@description Turns a list of polylines into an XML node
#'@param polylines A list of polylines
#'@return An XML node containing the polylines
polylines_to_xml <- function(polylines) {
  polylines_node <- list(polylines = list())

  for(i in 1:length(polylines)){

    pnt_list <- list()

    for(j in 1:length(polylines[[i]][[2]])) {
      pnt_list <- c(pnt_list, list(pnt = list(polylines[[i]][[2]][[j]])))
    }

    polylines_node[[1]] <- c(polylines_node[[1]], list(polyline = structure(pnt_list,
                                                                            id = (i-1),
                                                                            name = polylines[[i]][[1]])))
  }

  return(xml2::as_xml_document(polylines_node))
}

#'surfaces_to_xml
#'@description Turns a list of surfaces into an XML node
#'@param surfaces A list of surfaces
#'@return An XML node containing the surfaces
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

