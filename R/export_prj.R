#This script contains functions to export the .prj data

#'export_prj
#'@description Wrapper function to create a .prj XML document based on the user input data
#'@param ogs6_obj ...
export_prj <- function(ogs6_obj) {

    prj_node <- list(OpenGeoSysProject = structure(list()))

    meshes_node <- NULL

    #If there is a .gml defined, add "mesh" node, else add "meshes" node
    if(is.null(ogs6_obj$geometry)) {
        meshes_node <- adopt_nodes("meshes", ogs6_obj$meshes)
    }else{
        meshes_node <- list(mesh = list(ogs6_obj$meshes[[1]]))
    }

    #Create wrapper nodes where necessary
    processes_node <- adopt_nodes("processes", ogs6_obj$processes)
    media_node <- adopt_nodes("media", ogs6_obj$media)
    parameters_node <- adopt_nodes("parameters", ogs6_obj$parameters)
    curves_node <- adopt_nodes("curves", ogs6_obj$curves)
    process_variables_node <- adopt_nodes("process_variables", ogs6_obj$process_variables)
    nonlinear_solvers_node <- adopt_nodes("nonlinear_solvers", ogs6_obj$nonlinear_solvers)
    linear_solvers_node <- adopt_nodes("linear_solvers", ogs6_obj$linear_solvers)
    test_definition_node <- adopt_nodes("test_definition", ogs6_obj$test_definition)

    #Add all of the required children
    prj_node <- add_children(prj_node, list(meshes_node,
                                            geometry = ogs6_obj$geometry,
                                            processes_node,
                                            media_node,
                                            time_loop = ogs6_obj$time_loop,
                                            parameters_node,
                                            curves_node,
                                            process_variables_node,
                                            nonlinear_solvers_node,
                                            linear_solvers_node,
                                            test_definition_node
                                            ))

    file <- paste0(ogs6_obj$sim_path, ogs6_obj$sim_name, ".prj")

    prj_xml <- xml2::as_xml_document(prj_node)

    xml2::write_xml(prj_xml, file, options = "format", encoding="ISO-8859-1")

    return(invisible())
}