

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
            values = values,
            tag_name = "parameter",
            is_subclass = FALSE,
            attr_names = character(),
            flatten_on_exp = c("values")
        ),
        class = "r2ogs6_parameter"
    )
}

