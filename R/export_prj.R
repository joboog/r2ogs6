#This script contains functions to export the .prj data

#'export_prj
#'@description Wrapper function to create a .prj XML document based on the user input data
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
export_prj <- function(ogs6_obj) {

    node <- list(OpenGeoSysProject = structure(list()))

    meshes_node <- NULL

    #If there is a .gml defined, add "mesh" node, else add "meshes" node
    if(is.null(geometry)) {
        meshes_node <- adopt_nodes("meshes", meshes)
    }else{
        meshes_node <- list(mesh = meshes[[1]])
    }

    #Create wrapper nodes where necessary
    processes_node <- adopt_nodes("processes", processes)
    media_node <- adopt_nodes("media", media)
    parameters_node <- adopt_nodes("parameters", parameters)
    curves_node <- adopt_nodes("curves", curves)
    process_variables_node <- adopt_nodes("process_variables", process_variables)
    nonlinear_solvers_node <- adopt_nodes("nonlinear_solvers", nonlinear_solvers)
    linear_solvers_node <- adopt_nodes("linear_solvers", linear_solvers)
    test_definition_node <- adopt_nodes("test_definition", test_definition)

    #Add all of the required children
    prj_node <- add_children(prj_node, list(meshes_node,
                                            geometry = geometry,
                                            processes_node,
                                            media_node,
                                            time_loop = time_loop,
                                            parameters_node,
                                            curves_node,
                                            process_variables_node,
                                            nonlinear_solvers_node,
                                            linear_solvers_node,
                                            test_definition_node
                                            ))

    #...
    return(invisible())
}

#VALIDATION:
# if(!has_gml){
#     if(length(ogs6_obj$sim_input[["vtk_meshes"]]) < 2){
#         stop("If you don't want to specify a gml object, you must have multiple
#                   meshes. You can define more by calling the function generate_structured_mesh.", call. = FALSE)
#     }
# }else if(length(ogs6_obj$sim_input[["vtk_meshes"]]) != 1){
#     stop("If you want to specify a gml object, there must be (only) one mesh (one vtk file).", call. = FALSE)
# }