#============================== PARAMETERS CLASSES AND METHODS ================================

#============================== PARAMETER ================================

#'r2ogs6_parameter
#'@description S3 class describing a .prj parameter
#'@param name The parameter name
#'@param type The parameter type
#'@param values Optional: The parameter values
#'@param value Optional: The parameter value
#'@export
r2ogs6_parameter <- function(name, type, values = NULL, value = NULL) {

    if(!is.null(values) && !is.null(value)){
        stop(paste("r2ogs6_parameter: Use either 'values' or 'value' parameter (XOR)"), call. = FALSE)
    }

    #Coerce input
    if(!is.null(value)){
        if(assertthat::is.string(value)){
            value <- as.double(value)
        }
        values <- value
    }else{
        if(assertthat::is.string(values)){
            values <- as.double(unlist(strsplit(values, " ")))
        }
    }

    new_r2ogs6_parameter(name, type, values)
}


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
#'@description Implementation of generic function as_node for S3 class r2ogs6_parameter
#'@param x A r2ogs6_parameter class object
as_node.r2ogs6_parameter <- function(x) {
    node <- list(parameter = structure(list()))

    node <- add_children(node, list(name = x$name,
                                    type = x$type))

    if(length(x$values) == 1){
        node <- add_children(node, list(value = x$values[[1]]))
    }else{

        val_string <- paste(x$values, collapse = " ")
        node <- add_children(node, list(values = val_string))
    }

    return(node)
}


#'input_add.r2ogs6_parameter
#'@description Implementation of generic function input_add for S3 class r2ogs6_parameter
#'@param x A r2ogs6_parameter class object
#'@param ogs6_obj A OGS6 class object
#'@export
input_add.r2ogs6_parameter <- function(x, ogs6_obj) {
    ogs6_obj$add_parameter(x)
}