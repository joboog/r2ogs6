#This script contains various S3 classes, methods and functions to turn data for a .prj file into
#the correct XML format. (WIP!!!)


#============================== Functions to be exported ================================




#============================== processes classes and methods ================================


#S3 class representing a .prj process element
#'@param name The process name
#'@param type The process type
#'@param integration_order ...
#'@param dimension ...
#'@param constitutive_relation ...
#'@param process_variables ...
#'@param secondary_variables A list of secondary variables
#'@param specific_body_force The specific body force
new_r2ogs_prj_process <- function(name, type, integration_order, dimension, constitutive_relation,
                                   process_variables, secondary_variables, specific_body_force) {
    #validating functions here

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
        class = "r2ogs_prj_process"
    )
}





#============================== media classes and methods ================================

new_r2ogs_prj_property <- function(name = character(), type = character(), value = double()){
    #Validate...


    structure(
        list(
            name = name,
            type = type,
            value = value
        ),
        class = "r2ogs6_prj_property"
    )
}




#'Defines a .prj medium phase element
#'@param type A string specifying the medium type (valid types: ...)
#'@param properties A list of properties (see ?r2ogs6_property for more info)
new_r2ogs6_prj_medium_phase <- function(type = character(), properties = list()) {

    stopifnot(is.character(type), length(type) == 1)

    structure(
        list(
            type = type,
            properties = properties
        ),
        class = "r2ogs6_prj_medium_phase"
    )
}




#'
#'@param phases A list of medium phases
#'@param properties A list of medium properties
new_r2ogs6_prj_medium <- function(phases = list(), properties = list()) {
    #Validate

    stopifnot(is.list(phases))
    stopifnot(is.list(properties))

    structure(
        list(
            phases = phases,
            properties = properties
        ),
        class = "r2ogs6_prj_medium"
    )
}




#============================== time_loop classes and methods ================================

#============================== parameters classes and methods ================================


new_r2ogs6_prj_parameter <- function(name = character(), type = character(), value = double()) {
    #Validate

    structure(
        list(
            name = name,
            type = type,
            value = value
        ),
        class = "r2ogs6_prj_parameter"
    )
}



#============================== process_variables classes and methods ================================


#============================== nonlinear_solvers classes and methods ================================

#============================== linear_solvers classes and methods ================================






