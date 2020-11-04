# Functions for adding .vtu data to a OGS6 simulation object

#'generate_structured_mesh
#'@description Wrapper function to call generateStructuredMesh.exe (VTK mesh generator).
#'For full documentation see https://www.opengeosys.org/docs/tools/meshing/structured-mesh-generation/
#'@param ogs6_obj The OGS6 class object (containing the path to the ogs6 bin/ folder)
#'@param element_type Element type to be created: line | tri | quad | hex |
#' prism | tet | pyramid
#' @param ...
#' @return The newly generated .vtu file path
generate_structured_mesh = function(ogs6_obj, element_type, ...) {

    assertthat::assert_that(inherits(ogs6_obj, "OGS6"))
    assertthat::assert_that(assertthat::is.string(element_type))

    valid_element_types <- list("line", "tri", "quad", "hex", "prism", "tet", "pyramid")

    mesh_number <- 1

    is_first <- check_for_input_of_name(ogs6_obj, "vtu_meshes", FALSE, FALSE)

    if(!is_first){
        mesh_number <- length(ogs6_obj$sim_input[["vtu_meshes"]]) + 1
    }

    mesh_output_file <- paste0(ogs6_obj$sim_path, ogs6_obj$sim_name, "_", mesh_number, ".vtu")

    if(!element_type %in% valid_element_types){
        stop("Specified element type invalid.", call. = FALSE)
    }

    #sysname <- Sys.info()[['sysname']]

    if(file.exists(paste0(ogs6_obj$ogs_bin_path, "/generateStructuredMesh.exe"))) {
        system(command = paste0(ogs6_obj$ogs_bin_path, "/generateStructuredMesh.exe",
                                " -e ", element_type, " -o ", mesh_output_file, " ", ...))
    }else{
        stop(paste("Could not find executable file generateStructuredMesh.exe at location",
                   ogs6_obj$ogs_bin_path), call. = FALSE)
    }

    ogs6_obj$add_sim_input("vtu_meshes", mesh_output_file)

    return(invisible())
}