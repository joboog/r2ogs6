
#===== export_gml =====


#'export_gml
#'@description Creates a .gml XML document based on  user input data
#'@param gml OGS6_gml:
#'@param path string:
export_gml <- function(gml, path) {

  assertthat::assert_that(assertthat::is.string(path))

  gml_xml <- xml2::xml_new_root(
    .value = "OpenGeoSysGLI",
    "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance",
    "xmlns:ogs" = "http://www.opengeosys.org"
    )

  xml2::xml_add_child(gml_xml,
                      xml2::as_xml_document(
                        to_node(gml$name)))

  # If the points are in a tibble, coerce it to list for exporting
  points_list <- gml$points

  if(tibble::is_tibble(gml$points)){
    points_list <- split(gml$points,
                         seq(nrow(gml$points)))

    points_list <- lapply(points_list, function(x){
      as.list(x)
    })

    names(points_list) <- rep("point", length(points_list))
  }

  # The points need an extra 'id' attribute
  for(i in seq_len(length(points_list))){
    points_list[[i]] <- c(points_list[[i]], id = (i - 1))

    # Remove empty names
    if(points_list[[i]][["name"]] == ""){
      points_list[[i]][["name"]] <- NULL
    }
  }

  xml2::xml_add_child(gml_xml,
                      xml2::as_xml_document(
                        to_node(points_list,
                                "points", c("point"))))

  if(!is.null(gml$polylines)){

    # The polylines need an extra 'id' attribute
    polylines_with_ids <- list()
    for(i in seq_len(length(gml$polylines))){
      polyline <- list(name = gml$polylines[[i]][[1]], id = (i - 1))
      for(j in seq_len(length(gml$polylines[[i]][[2]]))){
        polyline <- c(polyline, list(pnt = gml$polylines[[i]][[2]][[j]]))
      }
      polylines_with_ids <- c(polylines_with_ids, list(polyline = polyline))
    }

    xml2::xml_add_child(gml_xml,
                        xml2::as_xml_document(
                          to_node(polylines_with_ids,
                                  "polylines", c("name",
                                                 "id"))))
  }

  if(!is.null(gml$surfaces)){

    # The surfaces need an extra 'id' attribute
    surfaces_with_ids <- list()
    for(i in seq_len(length(gml$surfaces))){
      surface <- c(gml$surfaces[[i]], list(id = (i - 1)))
      surfaces_with_ids <- c(surfaces_with_ids, list(surface = surface))
    }

    xml2::xml_add_child(gml_xml,
                        xml2::as_xml_document(
                          to_node(surfaces_with_ids,
                                  "surfaces", c("name",
                                                "id",
                                                "element"))))
  }

  xml2::write_xml(gml_xml, path, options = "format", encoding="ISO-8859-1")

  return(invisible())
}

