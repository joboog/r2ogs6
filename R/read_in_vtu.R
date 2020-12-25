
#===== pick_vtu_file =====


#'pick_vtu_file
#'@description Lets the user pick a .vtu file and adds it to the specified OGS6
#' class object
#'@param ogs6_obj OGS6: Simulation object
#'@param read_in_vtu flag: Should the file content be read in?
#'@export
pick_vtu_file <- function(ogs6_obj,
                          read_in_vtu = FALSE) {

    assertthat::assert_that("OGS6" %in% class(ogs6_obj))

    file <- file.choose()
    check_file_extension(file, "vtu")

    if(read_in_vtu){
        read_in_vtu(ogs6_obj, file)
    }else{
        ogs6_obj$add_mesh(file)
    }
}


#'read_in_vtu
#'@description Wrapper function to read in a whole .vtu file
#'@param ogs6_obj OGS6: Simulation object
#'@param vtu_path string: Path to the mesh file that should be read in
#'@export
read_in_vtu <- function(ogs6_obj, vtu_path) {

    assertthat::assert_that("OGS6" %in% class(ogs6_obj))
    xml_doc <- validate_read_in_xml(vtu_path)

    #...
}