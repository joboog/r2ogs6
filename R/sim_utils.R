#This script contains functions to start the simulation


#'run_simulation
#'@description Calls OGS6 object validator functions, exports all necessary files and starts OpenGeoSys6
#'@param ogs6_obj
#'@param iter_n The number of iterations (for simulation chains)
#'@export
run_simulation <- function(ogs6_obj, iter_n = 1) {

    assertthat::assert_that(is.numeric(iter_n), iter_n > 0, iter_n < 500)

    #Call all validators
    validate_all(ogs6_obj)

    #Export all necessary files
    export_all(ogs6_obj)

    #Run simulations (and read in output as input)
    for(i in seq_len(iter_n)){

        #Call OGS6
        #...

        read_in_output(ogs6_obj)
    }
}


#'export_all
#'@description Exports all necessary files
#'@param ogs6_obj
export_all <- function(ogs6_obj) {

    #
    gml_file_name <- "bla"

    if(length(ogs6_obj$sim_input[["vtu_meshes"]]) == 1){
        export_xml_to_file(gml_data_to_xml(ogs6_obj$sim_input[["gml_obj"]]), gml_file_name)
    }

    #
    prj_file_name <- "ha"

    export_xml_to_file(prj_data_to_xml(ogs6_obj$sim_input[["prj_obj"]]), prj_file_name)
}


#'validate_all
#'@description Validates all necessary parameters
#'@param ogs6_obj
validate_all <- function(ogs6_obj) {

}


#'read_in_output
#'@description After a OGS6 simulation was run, reads in the generated .vtu files as new input for
#' the .prj file
#'@param ogs6_obj
read_in_output <- function(ogs6_obj) {


    output_file_name <- "ha"

    ogs6_obj$set_sim_input_obj_param("prj_obj", "meshes", output_file_name)
}



