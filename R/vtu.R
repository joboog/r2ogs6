# Functions for adding .vtu data to a OGS6 simulation object

#'generate_structured_mesh
#'@description Wrapper function to call generateStructuredMesh.exe (VTK mesh generator).
#'For full documentation see https://www.opengeosys.org/docs/tools/meshing/structured-mesh-generation/
#'@param ogs6_obj A OGS6 class object (containing the path to the ogs6 bin/ folder)
#'@param call_str The arguments the script will be called with (EXCEPT -o output_file_name, this will
#' be generated automatically!)
#'@return The newly generated .vtu file path
generate_structured_mesh = function(ogs6_obj, call_str) {

    assertthat::assert_that(inherits(ogs6_obj, "OGS6"))
    assertthat::assert_that(assertthat::is.string(call_str))

    mesh_number <- 1

    is_first <- (length(ogs6_obj$meshes) == 0)

    if(!is_first){
        mesh_number <- length(ogs6_obj$meshes) + 1
    }

    mesh_output_file <- paste0(ogs6_obj$sim_path, ogs6_obj$sim_name, "_", mesh_number, ".vtu")

    #sysname <- Sys.info()[['sysname']]

    system(command = paste0(ogs6_obj$ogs_bin_path, "generateStructuredMesh.exe",
                            " -o ", mesh_output_file, " ", call_str))

    ogs6_obj$add_mesh(mesh_output_file)

    return(invisible(mesh_output_file))
}