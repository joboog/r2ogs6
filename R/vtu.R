# Functions for adding .vtu data to a OGS6 simulation object


#'r2ogs6_mesh
#'@description Wrapper class, so far only has a reference to a .vtu file
#'@param mesh_ref A reference to a .vtu file
#'@export
r2ogs6_mesh <- function(mesh_ref){
    new_r2ogs6_mesh(mesh_ref)
}

new_r2ogs6_mesh <- function(mesh_ref){

    assertthat::assert_that(assertthat::is.string(mesh_ref))

    structure(
        list(
            mesh_ref = mesh_ref
            ),
        class = "r2ogs6_mesh"
    )
}

#'as_node.r2ogs6_curve
#'@description Implementation of generic function as_node for S3 class r2ogs6_mesh
#'@param x A r2ogs6_mesh class object
as_node.r2ogs6_mesh <- function(x){
    node <- list(mesh = structure(list(x$mesh_ref)))
    return(node)
}


#'generate_structured_mesh
#'@description Wrapper function to call generateStructuredMesh.exe (VTK mesh generator).
#'For full documentation see https://www.opengeosys.org/docs/tools/meshing/structured-mesh-generation/
#'@param ogs6_obj A OGS6 class object (containing the path to the ogs6 bin/ folder)
#'@param call_str The arguments the script will be called with (EXCEPT -o output_file_name, this will
#' be generated automatically!)
#'@return The newly generated .vtu file path
#'@export
generate_structured_mesh = function(ogs6_obj, call_str) {

    assertthat::assert_that(inherits(ogs6_obj, "OGS6"))
    assertthat::assert_that(assertthat::is.string(call_str))

    mesh_number <- 1

    is_first <- (length(ogs6_obj$meshes) == 0)

    if(!is_first){
        mesh_number <- length(ogs6_obj$meshes) + 1
    }

    mesh_output_file_name <- paste0(ogs6_obj$sim_name, "_", mesh_number, ".vtu")
    mesh_output_file <- paste0(ogs6_obj$sim_path, mesh_output_file_name)

    #sysname <- Sys.info()[['sysname']]

    system(command = paste0(ogs6_obj$ogs_bin_path, "generateStructuredMesh.exe",
                            " -o ", mesh_output_file, " ", call_str))

    ogs6_obj$add_mesh(r2ogs6_mesh(mesh_output_file_name))

    return(invisible(mesh_output_file_name))
}