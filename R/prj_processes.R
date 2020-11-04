
#============================== PROCESSES CLASSES AND METHODS ================================

#============================== PROCESS ================================
#NOT TO BE CONFUSED WITH TIME LOOP PROCESSES!

#'r2ogs6_process
#'@description S3 class describing a .prj (processes) process
#'@param name The process name
#'@param type The process type
#'@param integration_order ...
#'@param dimension ...
#'@param constitutive_relation ...
#'@param process_variables ...
#'@param secondary_variables A list of secondary variables
#'@param specific_body_force The specific body force
#'@param coupling_scheme Optional:
#'@export
r2ogs6_process <- function(name, type, integration_order, dimension, constitutive_relation,
                           process_variables, secondary_variables, specific_body_force, coupling_scheme = NULL){

    #Make this more user friendly
    #...


    #Validate and call the constructor in the end
    validate_r2ogs6_process(new_r2ogs6_process(name, type, integration_order, dimension, constitutive_relation,
                                               process_variables, secondary_variables, specific_body_force,
                                               coupling_scheme))

}


#'validate_r2ogs6_process
#'@description Validator for r2ogs6_process class objects, contains more extensive checks than the constructor
validate_r2ogs6_process <- function(r2ogs6_process){

    validate_param_list(r2ogs6_process$constitutive_relation, 3, c("type", "youngs_modulus", "poissons_ratio"))
    validate_param_list(r2ogs6_process$process_variables, 2, c("displacement", "pressure"))

    #Add more validation functionality...

    return(r2ogs6_process)
}


#new_r2ogs6_process
#'@description Constructor for S3 class r2ogs6_processes
new_r2ogs6_process <- function(name, type, integration_order, dimension, constitutive_relation,
                               process_variables, secondary_variables, specific_body_force, coupling_scheme = NULL) {

    #Basic validation
    assertthat::assert_that(assertthat::is.string(name))
    assertthat::assert_that(assertthat::is.string(type))

    assertthat::assert_that(assertthat::is.number(integration_order))
    assertthat::assert_that(assertthat::is.number(dimension))

    assertthat::assert_that(is.list(constitutive_relation))
    assertthat::assert_that(is.list(process_variables))
    assertthat::assert_that(is.list(secondary_variables))
    assertthat::assert_that(is.vector(specific_body_force))

    if(!is.null(coupling_scheme)){
        assertthat::assert_that(assertthat::is.string(coupling_scheme))
    }

    structure(
        list(
            name = name,
            type = type,
            integration_order = integration_order,
            dimension = dimension,
            constitutive_relation = constitutive_relation,
            process_variables = process_variables,
            secondary_variables = secondary_variables,
            specific_body_force = specific_body_force,
        ),
        class = "r2ogs6_process"
    )
}


#'as_node.r2ogs6_process
#'@description Implementation of generic function as_node for S3 class r2ogs6_process
#'@param obj A r2ogs6_process class object
as_node.r2ogs6_process <- function(obj) {

    process_node <- list(process = structure(list()))

    process_node <- add_children(process_node, list(name = obj$name,
                                                    type = obj$type,
                                                    coupling_scheme = obj$coupling_scheme,
                                                    integration_order = obj$integration_order,
                                                    dimension = obj$dimension,
                                                    constitutive_relation = obj$constitutive_relation,
                                                    process_variables = obj$process_variables,
                                                    specific_body_force = obj$specific_body_force))

    return(process_node)
}


#'input_add.r2ogs6_process
#'@description Implementation of generic function input_add for S3 class r2ogs6_process
#'@param obj A r2ogs6_process class object
#'@param ogs6_obj A OGS6 class object
input_add.r2ogs6_process <- function(obj, ogs6_obj) {

    check_for_input_of_name(ogs6_obj, "prj_obj", TRUE, TRUE, "input_add.prj_obj")

    ogs6_obj$add_to_sim_input_obj_param("prj_obj", "processes", process)
}