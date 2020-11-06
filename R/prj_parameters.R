#============================== PARAMETERS CLASSES AND METHODS ================================

#============================== PARAMETER ================================

#'r2ogs6_parameter
#'@description S3 class describing a .prj parameter
#'@param name The parameter name
#'@param type The parameter type
#'@param values The parameter values
#'@export
r2ogs6_parameter <- function(name, type, values) {

    #Make this more user friendly
    #...

    new_r2ogs6_parameter(name, type, values)
}


#'new_r2ogs6_parameter
#'@description Constructor for S3 class r2ogs6_parameter
new_r2ogs6_parameter <- function(name, type, values) {

    assertthat::assert_that(assertthat::is.string(name))
    assertthat::assert_that(assertthat::is.string(type))
    assertthat::assert_that(is.numeric(values))

    structure(
        list(
            name = name,
            type = type,
            values = values
        ),
        class = "r2ogs6_parameter"
    )
}


#'as_node.r2ogs6_parameter
#'@description
as_node.r2ogs6_parameter <- function(obj) {
    parameter_node <- list(parameter = structure(list()))

    parameter_node <- add_children(parameter_node, list(name = obj$name,
                                                        type = obj$type))

    if(length(obj$values) == 1){
        parameter_node <- add_children(parameter_node, list(value = obj$values[[1]]))
    }else{

        val_string <- paste(obj$values, collapse = " ")
        parameter_node <- add_children(parameter_node, list(values = val_string))
    }

    return(parameter_node)
}


#'input_add.r2ogs6_parameter
#'@description Implementation of generic function input_add for S3 class r2ogs6_parameter
#'@param obj A r2ogs6_parameter class object
#'@param ogs6_obj A OGS6 class object
#'@export
input_add.r2ogs6_parameter <- function(obj, ogs6_obj) {
    ogs6_obj$add_parameter(obj)
}