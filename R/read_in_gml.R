#Functions to read in data from a .gml file to an OGS6 object


#'read_in_gml
#'@description Wrapper function to read in a whole .gml file
#'@param ogs6_obj A OGS6 class object
#'@param gml_path The path to the geometry file that should be read in
#'@export
read_in_gml <- function(ogs6_obj, gml_path) {

    assertthat::assert_that("OGS6" %in% class(ogs6_obj))
    xml_doc <- validate_read_in_xml(gml_path)

    name <- xml2::xml_text(xml2::xml_find_first(xml_doc, "//name"))
    points <- read_in_points(xml_doc)
    polylines <- read_in_polylines(xml_doc)
    surfaces <- read_in_surfaces(xml_doc)

    ogs6_obj$add_gml(r2ogs6_gml(name, points, polylines, surfaces))
}


#'read_in_points
#'@description Reads points from a .gml file
#'@param xml_doc A parsed XML document (of class 'xml2::xml_document')
read_in_points <- function(xml_doc) {

    points_tibble <- tibble::tibble(x = c(),
                                    y = c(),
                                    z = c(),
                                    name = c())

    points_nodeset <- xml2::xml_find_all(xml_doc, "//points/*")

    for(i in seq_len(length(points_nodeset))){
        attrs <- xml2::xml_attrs(points_nodeset[[i]])

        point_name <- ""

        if(length(attrs) == 5){
            point_name <- attrs[[5]]
        }

        points_tibble <- tibble::add_row(points_tibble,
                                        x = as.double(attrs[[2]]),
                                        y = as.double(attrs[[3]]),
                                        z = as.double(attrs[[4]]),
                                        name = point_name,
                                        )
    }

    return(invisible(points_tibble))
}


#'read_in_polylines
#'@description Reads polylines from a .gml file
#'@param xml_doc A parsed XML document (of class 'xml2::xml_document')
read_in_polylines <- function(xml_doc) {

    polylines_list <- list()

    polylines_nodeset <- xml2::xml_find_all(xml_doc, "//polylines/*")

    if(length(polylines_nodeset) == 0){
        return(invisible(NULL))
    }

    for(i in seq_along(polylines_nodeset)){
        attrs <- xml2::xml_attrs(polylines_nodeset[[i]])
        pnt_nodeset <- xml2::xml_children(polylines_nodeset[[i]])
        pnt_vector <- c()

        for(j in seq_along(pnt_nodeset)){
            pnt_vector <- c(pnt_vector, xml2::xml_double(pnt_nodeset[[j]]))
        }

        polyline <- list(name = attrs[[2]], pnt_vector)
        polylines_list <- c(polylines_list, list(polyline))
    }

    return(invisible(polylines_list))
}


#'read_in_surfaces
#'@description Reads surfaces from a .gml file
#'@param xml_doc A parsed XML document (of class 'xml2::xml_document')
read_in_surfaces <- function(xml_doc) {

    surfaces_list <- list()

    surfaces_nodeset <- xml2::xml_find_all(xml_doc, "//surfaces/*")

    if(length(surfaces_nodeset) == 0){
        return(invisible(NULL))
    }

    for(i in seq_len(length(surfaces_nodeset))){
        attrs <- xml2::xml_attrs(surfaces_nodeset[[i]])
        element_nodeset <- xml2::xml_children(surfaces_nodeset[[i]])

        element_1 <- as.double(xml2::xml_attrs(element_nodeset[[1]]))

        surface <- list(name = attrs[[2]],
                        element = element_1)

        if(length(element_nodeset) == 2){
            element_2 <- as.double(xml2::xml_attrs(element_nodeset[[2]]))
            surface <- c(surface, list(element = element_2))
        }

        surfaces_list <- c(surfaces_list, list(surface))
    }

    return(invisible(surfaces_list))
}