

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

    #Coerce input
    if(assertthat::is.string(integration_order)){
        integration_order <- as.double(integration_order)
    }

    if(assertthat::is.string(dimension)){
        dimension <- as.double(dimension)
    }

    if(assertthat::is.string(specific_body_force)){
        specific_body_force <- as.double(unlist(strsplit(specific_body_force, " ")))
    }

    #Validate and call the constructor
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
    assertthat::assert_that(length(constitutive_relation) == 3)
    names(constitutive_relation) <- c("type", "youngs_modulus", "poissons_ratio")

    assertthat::assert_that(is.vector(process_variables))
    assertthat::assert_that(length(process_variables) == 2)
    names(process_variables) <- c("displacement", "pressure")

    assertthat::assert_that(is.list(secondary_variables))
    names(secondary_variables) <- rep("secondary_variable", length(secondary_variables))
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
            coupling_scheme = coupling_scheme,
            tag_name = "process",
            is_subclass = FALSE,
            attr_names = c("secondary_variable"),
            flatten_on_exp = c("specific_body_force")
        ),

        class = "r2ogs6_process"
    )
}


validate_r2ogs6_process <- function(r2ogs6_process){

    for(i in seq_len(length(r2ogs6_process$secondary_variables))){
        if(length(r2ogs6_process$secondary_variables[[i]]) == 1){
            r2ogs6_process$secondary_variables[[i]] <- c(r2ogs6_process$secondary_variables[[i]][[1]],
                                                         r2ogs6_process$secondary_variables[[i]][[1]])
        }else if(length(r2ogs6_process$secondary_variables[[i]]) != 2){
            stop(paste("Parameters passed to secondary_variables must be string vectors of",
                       "length 1 or 2"), call. = FALSE)
        }
        names(r2ogs6_process$secondary_variables[[i]]) <- c("internal_name", "output_name")
    }

    #Add more validation functionality...

    return(invisible(r2ogs6_process))
}

