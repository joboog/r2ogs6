#This script contains various S3 classes, methods and functions to turn data for a .prj file into
#the correct XML format. (WIP!!!)


#============================== Functions to be exported ================================


#'new_prj
#'@description Class describing the .prj file and any parameters defined in one
#'@param processes
#'@param time_loop
#'@param media
#'@param parameters
#'@param process_variables
#'@param nonlinear_solvers
#'@param linear_solvers
#'@param mesh
#'@param geometry
#'@param meshes
#'@param curves
#'@param test_definition
new_prj <- function() {

    #validation here...

    structure(
        list(meshes,
             geometry,
             processes,
             time_loop,
             media,
             parameters,
             curves,
             process_variables,
             nonlinear_solvers,
             linear_solvers,
             test_definition
             ),

        class = "r2ogs6_prj")
}


#'as_node.r2ogs6_prj
#'@description
as_node.r2ogs6_prj <- function(obj) {

    #Validate the r2ogs6_prj object
    #...

    prj_node <- list(OpenGeoSysProject = structure(list()))

    #If there is a .gml defined, add "geometry" and "mesh" node
    if(1) {

    }

    #Create wrapper nodes where necessary
    processes_node <- adopt_nodes("processes", obj$processes)
    media_node <- adopt_nodes("media", obj$media)
    parameters_node <- adopt_nodes("parameters", obj$parameters)
    #...

    process_variables_node <- adopt_nodes("process_variables", obj$process_variables)
    nonlinear_solvers_node <- adopt_nodes("nonlinear_solvers", obj$nonlinear_solvers)
    linear_solvers_node <- adopt_nodes("linear_solvers", obj$linear_solvers)

    #Add all of the required children
    prj_node <- add_children(prj_node, list(processes_node,
                                            obj$time_loop,
                                            media_node,
                                            parameters_node,

                                            nonlinear_solvers_node,
                                            linear_solvers_node
                                            )
                             )

    #Optional: Add all defined curves to a parent node named "curves"

    #Add all defined process variables to a parent node named "process_variables"

    #Optional: Add all defined test definition to a parent node named "test_definition"

    return(prj_node)
}

#'input_add_prj_obj
#'@description Adds an empty prj class object to a ogs6 class object input list
#'@param ogs6_obj The ogs6 object the prj class object should be added to
#'
input_add_prj_obj <- function(ogs6_obj) {

    assertthat::assert_that(inherits(ogs6_obj, "OGS6"))

    check_for_input_of_name(ogs6_obj, "prj_obj", TRUE, TRUE, "input_add_prj_obj")

    has_gml <- check_for_input_of_name(ogs6_obj, "gml_obj", TRUE, FALSE)

    check_for_input_of_name(ogs6_obj, "vtu_meshes", TRUE, TRUE, "generate_structured_mesh")

    if(!has_gml){
        if(length(ogs6_obj$sim_input[["vtk_meshes"]]) < 2){
            stop("If you don't want to specify a gml object, you must have multiple
                  meshes. You can define more by calling the function generate_structured_mesh.", call. = FALSE)
        }
    }else if(length(ogs6_obj$sim_input[["vtk_meshes"]]) != 1){
        stop("If you want to specify a gml object, there must be (only) one mesh (one vtk file).", call. = FALSE)
    }

    ogs6_obj$set_sim_input("prj_obj", new_prj())
}


#============================== curves classes and methods ================================

#============================== process_variables classes and methods ================================

#============================== test_definition classes and methods ================================






