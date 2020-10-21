#This script contains various functions to read user input from a file (WIP)


read_gml_input_from_file <- function(point_file_name, polyline_file_name, surface_file_name) {
    read_gml_points(point_file_name)
}

#'Reads in a .tsv file and parses it as a tibble.
#'A column specifying the name of the points is optional, but the number of columns must be consistent.
#'If one row has an entry for the point name, all rows need an entry for the point name.
#'If only some points need to be named, put "" (double quotes) for the points that don't have a name.
#'@param file_name The name of the .tsv file containing the point tibble
# @example
#
#
read_gml_points <- function(file_name) {
    point_tibble <- readr::read_tsv(file_name, col_names = FALSE, quoted_na = FALSE, comment = "#")
    if(length(point_tibble) == 4) {
        names(point_tibble) <- c("x", "y", "z", "name")
    }else if(length(point_tibble) == 3) {
        names(point_tibble) <- c("x", "y", "z")
    }else{
        stop(paste("Invalid number of columns detected while reading file containing .gml points"), call. = FALSE)
    }
    return(point_tibble)
}
