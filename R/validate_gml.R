#This script contains various functions to verify data for a .gml file (WIP)

#' validate_gml_data
#'
#' @name validate_gml_data
#' @description Checks if the defined gml class object isn't empty
#' @param gml_obj A gml class object
validate_gml_data <- function(gml_obj) {

    null_count <- 0

    if(is.null(gml_obj$points)){
        null_count <- null_count + 1
    }

    if(is.null(gml_obj$polylines)){
        null_count <- null_count + 1
    }

    if(is.null(gml_obj$surfaces)){
        null_count <- null_count + 1
    }

    if(null_count == 3){
        stop(paste("Defined .gml object only has a name. Consider adding some points,
                    polylines and surfaces."), call. = FALSE)
    }

    #More extensive checks... (WIP)
}


#'validate_points
#'
#'@name validate_points
#'@description Checks if the input is a tibble, if this tibble has the right number of elements,
#' if those elements are named correctly and if there are any overlapping points or duplicate point names
#'@param gml_points A tibble with 3 vectors named 'x', 'y' and 'z' (and an optional 'name' vector)
validate_points <- function(gml_points) {

    if(!(inherits(gml_points, "tbl_df") && inherits(gml_points, "tbl") &&
         inherits(gml_points, "data.frame"))){
        stop(paste(gml_points, " is not of class 'tbl_df' "), call. = FALSE)
    }

    names <- names(gml_points)

    if (!((length(gml_points) == 4 && names[[1]] == "x" && names[[2]] == "y" &&
           names[[3]] == "z" && names[[4]] == "name") ||
          (length(gml_points) == 3 && names[[1]] == "x" && names[[2]] == "y" &&
           names[[3]] == "z"))){
        stop(paste(gml_points, " column names do not fit to 'x, y, z, (name)' "), call. = FALSE)
    }

    if(!is.numeric(gml_points$x) || !is.numeric(gml_points$y) || !is.numeric(gml_points$z)){
        stop(paste(gml_points, "'x, y, z' columns must be numeric vectors"), call. = FALSE)
    }

    has_names <- (length(gml_points) == 4)

    #Find overlapping points and duplicate names
    for(i in 1:(length(gml_points[[1]])-1)){
        for(j in (i+1):length(gml_points[[1]])){
            if(gml_points[[1]][[i]] == gml_points[[1]][[j]] &&
               gml_points[[2]][[i]] == gml_points[[2]][[j]] &&
               gml_points[[3]][[i]] == gml_points[[3]][[j]]){
                stop("Overlapping .gml points (with the same coordinates) detected", call. = FALSE)
            }

            if(has_names){
                if(gml_points[[4]][[i]] == gml_points[[4]][[j]] &&
                   gml_points[[4]][[i]] != ""){
                    warning("Duplicate .gml point names detected", call. = FALSE)
                }
            }
        }
    }

    return(invisible(gml_points))
}

#'validate_polylines
#'
#'@name validate_polylines
#'@description Checks if the input is a list, if this list consists of other lists and
#' if those child lists have the correct structure (length of 2, first element is a string named
#' 'name', second element is a numeric vector)
#'@param gml_polylines A list consisting of other lists
validate_polylines <- function(gml_polylines) {

    if(!(inherits(gml_polylines, "list"))) {
        stop(paste("gml_polylines object is not of class 'list' "), call. = FALSE)
    }

    for(i in 1:length(gml_polylines)){
        if(!(inherits(gml_polylines[[i]], "list"))) {
            stop(paste("At least one element of gml_polylines is not of class 'list' "), call. = FALSE)
        }

        if(length(gml_polylines[[i]]) != 2) {
            stop(paste("At least one element of gml_polylines has incorrect length
                       (expected list of length 2)"), call. = FALSE)
        }

        if(names(gml_polylines[[i]])[[1]] != "name" || !is.character(gml_polylines[[i]][[1]])) {
            stop(paste("First element of gml_polylines (list) element must be
                       a character string named 'name' "), call. = FALSE)
        }

        if(!is.numeric(gml_polylines[[i]][[2]])) {
            stop(paste("Second element of gml_polylines (list) element must be
                       a numeric vector"), call. = FALSE)
        }

        #Check for duplicate points / polylines?
    }

    return(invisible(gml_polylines))
}

#'validate_surfaces
#'
#'@name validate_surfaces
#'@description Checks if the input is a list, if this list consists of other lists and
#' if those child lists have the correct structure (length of 3, first element is a string named
#' 'name', second and third element are numeric vectors)
#'@param gml_surfaces A list consisting of other lists
validate_surfaces <- function(gml_surfaces) {

    if(!(inherits(gml_surfaces, "list"))) {
        stop(paste("gml_surfaces object is not of class 'list'"), call. = FALSE)
    }

    for(i in 1:length(gml_surfaces)){
        if(!(inherits(gml_surfaces[[i]], "list"))) {
            stop(paste("At least one element of gml_surfaces is not of class 'list'"), call. = FALSE)
        }

        if(length(gml_surfaces[[i]]) != 3) {
            stop(paste("At least one element of gml_surfaces has incorrect length
                       (expected list of length 3)"), call. = FALSE)
        }

        if(names(gml_surfaces[[i]])[[1]] != "name" || !is.character(gml_surfaces[[i]][[1]])) {
            stop(paste("First element of gml_surfaces (list) element must be
                       a character string named 'name'"), call. = FALSE)
        }

        if(!is.numeric(gml_surfaces[[i]][[2]]) || !is.numeric(gml_surfaces[[i]][[3]]) ||
           length(gml_surfaces[[i]][[2]]) != 3 || length(gml_surfaces[[i]][[3]]) != 3) {
            stop(paste("Second and third element of gml_surfaces (list) element must be
                       a numeric vector of length 3"), call. = FALSE)
        }


        validate_surface_elements(gml_surfaces[[i]][[2]], gml_surfaces[[i]][[3]])


        #Check for duplicate points / surfaces?
    }

    return(invisible(gml_surfaces))

}


#'validate_surface_elements
#'
#'@name validate_surface_elements
#'@description Helper function, checks if two numerical vectors of length 3 (two surface elements)
#' each consist of 3 different elements and also have exactly 2 matching elements between them
#' which means they describe a valid surface. You can think of the two vectors as two triangles,
#' and the two triangles together form a square which is our surface.
#'@param gml_surface_element_1 A numerical vector of length 3
#'@param gml_surface_element_2 A numerical vector of length 3
validate_surface_elements = function (gml_surface_element_1, gml_surface_element_2) {

    if(gml_surface_element_1[[1]] == gml_surface_element_1[[2]] ||
       gml_surface_element_1[[1]] == gml_surface_element_1[[3]] ||
       gml_surface_element_1[[2]] == gml_surface_element_1[[3]] ||
       gml_surface_element_2[[1]] == gml_surface_element_2[[2]] ||
       gml_surface_element_2[[1]] == gml_surface_element_2[[3]] ||
       gml_surface_element_2[[2]] == gml_surface_element_2[[3]]) {
        stop("A surface element must consist of 3 different points", call. = FALSE)
    }

    equal_count <- 0

    for(i in 1:length(gml_surface_element_1)) {
        for(j in 1:length(gml_surface_element_2)) {
            if(gml_surface_element_1[[i]] == gml_surface_element_2[[j]]) {
                equal_count <- equal_count + 1
                break
            }
        }
    }

    if(equal_count != 2) {
        stop("Invalid surface detected", call. = FALSE)
    }
}