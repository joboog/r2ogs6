
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

    #Make more user friendly...

    #Validate and call the constructor in the end
    validate_r2ogs6_process(new_r2ogs6_process(name, type, integration_order, dimension, constitutive_relation,
                                               process_variables, secondary_variables, specific_body_force,
                                               coupling_scheme))

}


new_r2ogs6_process <- function(name, type, integration_order, dimension, constitutive_relation,
                               process_variables, secondary_variables, specific_body_force, coupling_scheme = NULL) {

    #Basic validation
    assertthat::assert_that(assertthat::is.string(name))
    assertthat::assert_that(assertthat::is.string(type))

    assertthat::assert_that(assertthat::is.number(integration_order))
    assertthat::assert_that(assertthat::is.number(dimension))

    assertthat::assert_that(is.vector(constitutive_relation))
    assertthat::assert_that(is.vector(process_variables))
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
            coupling_scheme = coupling_scheme
        ),

        class = "r2ogs6_process"
    )
}


validate_r2ogs6_process <- function(r2ogs6_process){

    #Check lists for correct length, coerce names
    assertthat::assert_that(length(r2ogs6_process$constitutive_relation) == 3)
    names(r2ogs6_process$constitutive_relation) <- c("type", "youngs_modulus", "poissons_ratio")

    assertthat::assert_that(length(r2ogs6_process$process_variables) == 2)
    names(r2ogs6_process$process_variables) <- c("displacement", "pressure")

    for(i in seq_len(length(r2ogs6_process$secondary_variables))){
        if(length(r2ogs6_process$secondary_variables[[i]]) == 1){
            r2ogs6_process$secondary_variables[[i]] <- c(r2ogs6_process$secondary_variables[[i]][[1]],
                                                         r2ogs6_process$secondary_variables[[i]][[1]])
        }else if(length(r2ogs6_process$secondary_variables[[i]]) != 2){
            stop(paste("Parameters passed to secondary_variables must be string vectors of",
                       "length 1 or 2"), call. = FALSE)
        }
    }

    #Add more validation functionality...

    return(invisible(r2ogs6_process))
}


#'as_node.r2ogs6_process
#'@description Implementation of generic function as_node for S3 class r2ogs6_process
#'@param x A r2ogs6_process class object
as_node.r2ogs6_process <- function(x) {

    node <- list(process = structure(list()))

    sbf_str <- paste(x$specific_body_force, collapse = " ")

    const_rel_node <- simple_vector_to_node("constitutive_relation", x$constitutive_relation)
    proc_var_node <- simple_vector_to_node("process_variables", x$process_variables)

    sec_var_node <- list(secondary_variables = structure(list()))

    for(i in seq_len(length(x$secondary_variables))){

        sec_var_node[[1]] <- c(sec_var_node[[1]],
                               list(secondary_variable = structure(list())))
        attributes(sec_var_node[[1]][[i]]) <- list(internal_name = x$secondary_variables[[i]][[1]],
                                                output_name = x$secondary_variables[[i]][[2]])
    }

    node <- add_children(node, list(name = x$name,
                                    type = x$type,
                                    coupling_scheme = x$coupling_scheme,
                                    integration_order = x$integration_order,
                                    dimension = x$dimension,
                                    const_rel_node,
                                    proc_var_node,
                                    sec_var_node,
                                    specific_body_force = sbf_str))

    return(invisible(node))
}


#'input_add.r2ogs6_process
#'@description Implementation of generic function input_add for S3 class r2ogs6_process
#'@param x A r2ogs6_process class object
#'@param ogs6_obj A OGS6 class object
#'@export
input_add.r2ogs6_process <- function(x, ogs6_obj) {
    ogs6_obj$add_process(x)
}