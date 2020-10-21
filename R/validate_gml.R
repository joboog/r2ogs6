#This script contains various functions to verify data for a .gml file (WIP)

# @export
validate_gml_data <- function(gml_geometry_name, gml_points = NA, gml_polylines = NA, gml_surfaces = NA) {

    validate_points(gml_points)
    validate_polylines(gml_polylines)
    validate_surfaces(gml_surfaces)
}

#'Checks if the input is a tibble, if this tibble has the right number of elements,
#' if those elements are named correctly, if the lists in the tibble are of the same length and
#' if there is any overlapping points or duplicate point names
#' @param gml_points A tibble with 3 vectors named 'x', 'y' and 'z' (and an optional 'name' vector)
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


validate_polylines <- function(gml_polylines) {

    if(!(inherits(gml_polylines, "list"))){
        stop(paste(gml_polylines, " is not of class 'list' "), call. = FALSE)
    }

}


validate_surfaces <- function(gml_surfaces) {

}