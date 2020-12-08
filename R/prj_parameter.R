
#===== r2ogs6_parameter =====


#'r2ogs6_parameter
#'@description S3 class describing a .prj parameter
#'@param name The parameter name
#'@param type The parameter type
#'@param values Optional: string | numeric: Parameter values
#'@param value Optional: string | double: Parameter value
#'@export
r2ogs6_parameter <- function(name,
                             type,
                             values = NULL,
                             value = NULL) {

    if(!is.null(values) && !is.null(value)){
        stop(paste("r2ogs6_parameter: Use either 'values' or 'value'",
                   "parameter (XOR)"), call. = FALSE)
    }

    #Coerce input
    value <- coerce_string_to_numeric(value)
    values <- coerce_string_to_numeric(values, TRUE)

    new_r2ogs6_parameter(name,
                         type,
                         values,
                         value)
}


new_r2ogs6_parameter <- function(name,
                                 type,
                                 values,
                                 value) {

    assertthat::assert_that(assertthat::is.string(name))
    assertthat::assert_that(assertthat::is.string(type))

    validate_is_null_or_numeric(values)
    validate_is_null_or_number(value)

    structure(
        list(
            name = name,
            type = type,
            values = values,
            value = value,
            is_subclass = FALSE,
            attr_names = character(),
            flatten_on_exp = c("values")
        ),
        class = "r2ogs6_parameter"
    )
}
