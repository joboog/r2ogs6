#============================== PROCESS_VARIABLES CLASSES AND METHODS ================================

#============================== PROCESS_VARIABLE ================================


#'r2ogs6_process_variable
#'@description S3 class describing a .prj process variable
#'@param name The name of the process variable
#'@param components The components of the process variable
#'@param order The order of the process variable
#'@param initial_condition The initial condition of the process variable
#'@param boundary_conditions The boundary conditions of the process variable
#'@export
r2ogs6_process_variable <- function(name, components, order, initial_condition, boundary_conditions){

    #Coerce input
    if(assertthat::is.string(components)){
        components <- as.double(components)
    }

    if(assertthat::is.string(order)){
        order <- as.double(order)
    }

    new_r2ogs6_process_variable(name, components, order, initial_condition, boundary_conditions)
}


new_r2ogs6_process_variable <- function(name, components, order, initial_condition, boundary_conditions){

    assertthat::assert_that(assertthat::is.string(name))
    assertthat::assert_that(assertthat::is.number(components))
    assertthat::assert_that(assertthat::is.number(order))
    assertthat::assert_that(assertthat::is.string(initial_condition))

    validate_wrapper_list(boundary_conditions, "r2ogs6_boundary_condition")

    structure(list(name = name,
                   components = components,
                   order = order,
                   initial_condition = initial_condition,
                   boundary_conditions = boundary_conditions),
              class = "r2ogs6_process_variable"
    )
}


#'as_node.r2ogs6_process_variable
#'@description Implementation of generic function as_node for S3 class r2ogs6_process_variable
#'@param x A r2ogs6_process_variable class object
as_node.r2ogs6_process_variable <- function(x) {

    node <- list(process_variable = structure(list()))

    boundary_conditions_node <- adopt_nodes("boundary_conditions", x$boundary_conditions)

    node <- add_children(node, list(name = x$name,
                                    components = x$components,
                                    order = x$order,
                                    initial_condition = x$initial_condition,
                                    boundary_conditions_node))

    return(invisible(node))
}


#'input_add.r2ogs6_process_variable
#'@description Implementation of generic function input_add for S3 class r2ogs6_process_variable
#'@param x A r2ogs6_process_variable class object
#'@param ogs6_obj A OGS6 class object
#'@export
input_add.r2ogs6_process_variable <- function(x, ogs6_obj) {
    ogs6_obj$add_process_variable(x)
}


#============================== BOUNDARY_CONDITION ================================


#'r2ogs6_boundary_condition
#'@description S3 class describing a .prj boundary condition
#'@param type ...
#'@param parameter ...
#'@param component ...
#'@param mesh ...
#'@param geometrical_set ...
#'@param geometry ...
#'@export
r2ogs6_boundary_condition <- function(type, parameter, component = NULL, mesh = NULL, geometrical_set = NULL,
                                      geometry = NULL){

    #Coerce input
    if(!is.null(component)){
        if(assertthat::is.string(component)){
            component <- as.double(component)
        }
    }

    new_r2ogs6_boundary_condition(type, parameter, component, mesh, geometrical_set, geometry)
}


new_r2ogs6_boundary_condition <- function(type, parameter, component = NULL, mesh = NULL, geometrical_set = NULL,
                                          geometry = NULL){


    assertthat::assert_that(assertthat::is.string(type))
    assertthat::assert_that(assertthat::is.string(parameter))

    if(!is.null(component)){
        assertthat::assert_that(assertthat::is.number(component))
    }

    if(!is.null(mesh)){
        assertthat::assert_that(assertthat::is.string(mesh))
    }

    if(!is.null(geometrical_set)){
        assertthat::assert_that(assertthat::is.string(geometrical_set))
    }

    if(!is.null(geometry)){
        assertthat::assert_that(assertthat::is.string(geometry))
    }

    structure(list(type = type,
                   parameter = parameter,
                   component = component,
                   mesh = mesh,
                   geometrical_set = geometrical_set,
                   geometry = geometry),
              class = "r2ogs6_boundary_condition"
    )
}


#'as_node.r2ogs6_boundary_condition
#'@description Implementation of generic function as_node for S3 class r2ogs6_boundary_condition
#'@param x A r2ogs6_boundary_condition class object
as_node.r2ogs6_boundary_condition <- function(x) {

    node <- list(boundary_condition = structure(list()))

    node <- add_children(node, list(mesh = x$mesh,
                                    geometrical_set = x$geometrical_set,
                                    geometry = x$geometry,
                                    type = x$type,
                                    component = x$component,
                                    parameter = x$parameter))

    return(invisible(node))
}
