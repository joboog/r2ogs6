# Functions to read in data from a .vtu file to an OGS6 object
# WIP, so far only creates the reference for the project file, might add
# analysis functions later!


#'pick_vtu_file
#'@description Lets the user pick a .vtu file and adds it to the specified OGS6
#' class object
#'@param ogs6_obj OGS6: Simulation object
#'@export
pick_vtu_file <- function(ogs6_obj) {

    assertthat::assert_that("OGS6" %in% class(ogs6_obj))

    file <- file.choose()
    check_file_extension(file, "vtu")

    ogs6_obj$add_mesh(OGS6_mesh$new(file))
    file.copy(file, ogs6_obj$sim_path)
}

#'read_in_vtu
#'@description Wrapper function to read in a whole .vtu file
#'@param ogs6_obj OGS6: Simulation object
#'@param vtu_path The path to the mesh file that should be read in
#'@export
read_in_vtu <- function(ogs6_obj, vtu_path) {

    assertthat::assert_that(class(ogs6_obj) == "OGS6")
    xml_doc <- validate_read_in_xml(vtu_path)

    #...
}