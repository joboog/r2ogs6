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



#'pick_file
#'@description Lets the user pick a file and adds it to the specified OGS6 class object
#'@param ogs6_obj A OGS6 class object
#'@export
pick_file <- function(ogs6_obj) {

    file <- file.choose()

    file_name <- basename(file)

    file_extension <- tools::file_ext(file)

    if(file_extension == "vtu"){
        ogs6_obj$add_mesh(file_name)
    # }else if(file_extension == "gml"){
    #     #...
    # }else if(file_extension == "prj"){
    #     #...
    }else{
        stop("File must have extension .vtu", call. = FALSE)
    }

    file.copy(file, ogs6_obj$sim_path)
}


#Source: https://stackoverflow.com/questions/48218491/os-independent-way-to-select-directory-interactively-in-r/48296736
#Helper function for choosing a directory (platform independent!)
choose_directory = function(ini_dir = getwd(), caption = 'Select data directory') {
    if (exists('utils::choose.dir')) {
        utils::choose.dir(default = ini_dir, caption = caption)
    } else {
        tcltk::tk_choose.dir(default = ini_dir, caption = caption)
    }
}