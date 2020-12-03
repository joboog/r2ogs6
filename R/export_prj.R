#This script contains functions to export the .prj data

#'export_prj
#'@description Wrapper function to create a .prj XML document based on the user input data
#'@param ogs6_obj ...
export_prj <- function(ogs6_obj) {

    meshes_node <- NULL

    #If there is a .gml defined, add "mesh" node, else add "meshes" node
    if(is.null(ogs6_obj$geometry)) {
        meshes_node <- to_node(ogs6_obj$meshes, "meshes")
    }else{
        meshes_node <- to_node(ogs6_obj$meshes[[1]]$mesh_ref, "mesh")
    }

    #Add all of the required children
    prj_node <- list(
        OpenGeoSysProject = list(
            meshes_node,
            to_node(ogs6_obj$geometry),
            to_node(ogs6_obj$processes),
            to_node(ogs6_obj$media),
            to_node(ogs6_obj$time_loop),
            to_node(ogs6_obj$parameters),
            to_node(ogs6_obj$curves),
            to_node(ogs6_obj$process_variables),
            to_node(ogs6_obj$nonlinear_solvers),
            to_node(ogs6_obj$linear_solvers),
            to_node(ogs6_obj$test_definition),
            to_node(ogs6_obj$insitu),
        )
    )

    file <- paste0(ogs6_obj$sim_path, ogs6_obj$sim_name, ".prj")

    prj_xml <- xml2::as_xml_document(prj_node)

    xml2::write_xml(prj_xml, file, options = "format", encoding="ISO-8859-1")

    return(invisible())
}