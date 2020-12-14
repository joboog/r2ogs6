#This script contains various functions to verify data for a .gml file (WIP)

#'validate_r2ogs6_gml
#'@description Validator for class r2ogs6_gml. Checks if the defined polylines
#' and surfaces reference existing points.
#'@param r2ogs6_gml A r2ogs6_gml class object
validate_r2ogs6_gml <- function(r2ogs6_gml) {

    maximal_point_id <- length(r2ogs6_gml$points[[1]]) - 1

    #Check if polylines reference existing points
    for(i in seq_len(length(r2ogs6_gml$polylines))){
        for(j in seq_len(length(r2ogs6_gml$polylines[[i]][[2]]))){
            if(r2ogs6_gml$polylines[[i]][[2]][[j]] > maximal_point_id ||
               r2ogs6_gml$polylines[[i]][[2]][[j]] < 0){
                stop("Polyline references point ID which does not exist", call. = FALSE)
            }
        }
    }

    #Check if surfaces reference existing points
    for(i in seq_len(length(r2ogs6_gml$surfaces))){
        for(j in seq_len(length(r2ogs6_gml$surfaces[[i]][[2]]))){
            if(r2ogs6_gml$surfaces[[i]][[2]][[j]] > maximal_point_id ||
               r2ogs6_gml$surfaces[[i]][[2]][[j]] < 0 ||
               r2ogs6_gml$surfaces[[i]][[3]][[j]] > maximal_point_id ||
               r2ogs6_gml$surfaces[[i]][[3]][[j]] < 0){
                stop("Surface references point ID which does not exist", call. = FALSE)
            }
        }
    }

    return(invisible(r2ogs6_gml))
}


#'validate_points
#'@description Checks if the input is a tibble, if this tibble has the right number of elements,
#' if those elements are named correctly and if there are any overlapping points or duplicate point names
#'@param points A tibble with 3 vectors named 'x', 'y' and 'z' (and an optional 'name' vector)
validate_points <- function(points) {

    assertthat::assert_that(inherits(points, "tbl_df"))

    names <- names(points)

    if (!((length(points) == 4 && names[[1]] == "x" && names[[2]] == "y" &&
           names[[3]] == "z" && names[[4]] == "name") ||
          (length(points) == 3 && names[[1]] == "x" && names[[2]] == "y" &&
           names[[3]] == "z"))){
        stop(paste(points, " column names do not fit to 'x, y, z, (name)' "), call. = FALSE)
    }

    assertthat::assert_that(is.numeric(points$x))
    assertthat::assert_that(is.numeric(points$y))
    assertthat::assert_that(is.numeric(points$z))

    has_names <- (length(points) == 4)

    #Find overlapping points and duplicate names
    for(i in 1:(length(points[[1]])-1)){
        for(j in (i+1):length(points[[1]])){
            if(points[[1]][[i]] == points[[1]][[j]] &&
               points[[2]][[i]] == points[[2]][[j]] &&
               points[[3]][[i]] == points[[3]][[j]]){
                stop("Overlapping .gml points (with the same coordinates) detected", call. = FALSE)
            }

            if(has_names){
                if(points[[4]][[i]] == points[[4]][[j]] &&
                   points[[4]][[i]] != ""){
                    warning("Duplicate .gml point names detected", call. = FALSE)
                }
            }
        }
    }

    return(invisible(points))
}


#'validate_polylines
#'@description Checks if the input is a list, if this list consists of other lists and
#' if those child lists have the correct structure (length of 2, first element is a string named
#' 'name', second element is a numeric vector)
#'@param polylines A list consisting of other lists
validate_polylines <- function(polylines) {

    assertthat::assert_that(is.list(polylines))

    for(i in 1:length(polylines)){

        assertthat::assert_that(is.list(polylines[[i]]))
        assertthat::assert_that(length(polylines[[i]]) == 2)
        assertthat::assert_that(names(polylines[[i]])[[1]] == "name")
        assertthat::assert_that(assertthat::is.string(polylines[[i]][[1]]))
        assertthat::assert_that(is.numeric(polylines[[i]][[2]]))

        #Check for duplicate points / polylines?
    }

    return(invisible(polylines))
}


#'validate_surfaces
#'@description Checks if the input is a list, if this list consists of other lists and
#' if those child lists have the correct structure (length of 3, first element is a string named
#' 'name', second and third element are numeric vectors)
#'@param surfaces A list consisting of other lists
validate_surfaces <- function(surfaces) {

    assertthat::assert_that(is.list(surfaces))

    for(i in 1:length(surfaces)){

        assertthat::assert_that(is.list(surfaces[[i]]))
        assertthat::assert_that(length(surfaces[[i]]) == 3)
        assertthat::assert_that(names(surfaces[[i]])[[1]] == "name")

        assertthat::assert_that(is.numeric(surfaces[[i]][[2]]))
        assertthat::assert_that(length(surfaces[[i]][[2]]) == 3)
        assertthat::assert_that(is.numeric(surfaces[[i]][[3]]))
        assertthat::assert_that(length(surfaces[[i]][[3]]) == 3)

        validate_surface_elements(surfaces[[i]][[2]], surfaces[[i]][[3]])

        #Check for duplicate points / surfaces?
    }

    return(invisible(surfaces))
}


#'validate_surface_elements
#'@description Helper function, checks if two numerical vectors of length 3 (two surface elements)
#' each consist of 3 different elements and also have exactly 2 matching elements between them
#' which means they describe a valid surface. You can think of the two vectors as two triangles,
#' and the two triangles together form a square which is our surface.
#'@param surface_element_1 A numerical vector of length 3
#'@param surface_element_2 A numerical vector of length 3
validate_surface_elements = function (surface_element_1, surface_element_2) {

    if(surface_element_1[[1]] == surface_element_1[[2]] ||
       surface_element_1[[1]] == surface_element_1[[3]] ||
       surface_element_1[[2]] == surface_element_1[[3]] ||
       surface_element_2[[1]] == surface_element_2[[2]] ||
       surface_element_2[[1]] == surface_element_2[[3]] ||
       surface_element_2[[2]] == surface_element_2[[3]]) {
        stop("A surface element must consist of 3 different points", call. = FALSE)
    }

    equal_count <- 0

    for(i in 1:length(surface_element_1)) {
        for(j in 1:length(surface_element_2)) {
            if(surface_element_1[[i]] == surface_element_2[[j]]) {
                equal_count <- equal_count + 1
                break
            }
        }
    }

    if(equal_count != 2) {
        stop("Invalid surface detected", call. = FALSE)
    }
}