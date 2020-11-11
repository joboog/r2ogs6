#This script contains functions to start the simulation


#'run_simulation
#'@description Calls OGS6 object validator functions, exports all necessary files and starts OpenGeoSys6
#'@param ogs6_obj A OGS6 class object
#'@param iter_n The number of iterations (for simulation chains)
#'@export
run_simulation <- function(ogs6_obj, iter_n = 1) {

    assertthat::assert_that(is.numeric(iter_n), iter_n > 0, iter_n < 500)

    #Call all validators
    validate_all(ogs6_obj)

    #Export the simulation files
    export_gml(ogs6_obj)
    export_prj(ogs6_obj)

    #Run simulations (and read in output as input)
    for(i in seq_len(iter_n)){

        #Call OGS6
        system(command = paste0(ogs6_obj$ogs_bin_path, "ogs.exe ",
                                ogs6_obj$sim_path, ogs6_obj$sim_name, ".prj "))

        # read_in_output(ogs6_obj)
    }
}


#'validate_all
#'@description Validates all necessary parameters
#'@param ogs6_obj A OGS6 class object
validate_all <- function(ogs6_obj) {

    if(!ogs6_obj$get_status()){
        stop("There are some components missing from your OGS6 object.", call. = FALSE)
    }

    if(is.null(ogs6_obj$gml)){
        if(length(ogs6_obj$meshes) < 2){
            stop("If you don't want to specify a gml object, you must have multiple
                      meshes. You can define more by calling the function generate_structured_mesh.", call. = FALSE)
        }
    }else if(length(ogs6_obj$meshes) != 1){
        stop("If you want to specify a gml object, there must be only one mesh (one vtk file).", call. = FALSE)
    }

    #...
}


#'read_in_output
#'@description After a OGS6 simulation was run, reads in the generated .vtu files as new input for
#' the .prj file
#'@param ogs6_obj A OGS6 class object
read_in_output <- function(ogs6_obj) {

    #....WIP
}



