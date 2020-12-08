
#===== r2ogs6_search_length_algorithm =====


#'r2ogs6_search_length_algorithm
#'@description tag: search_length_algorithm
#'@param type string: The type
#'@param value string | double: The value
#'@export
r2ogs6_search_length_algorithm <- function(type,
                                           value = NULL) {

    #Coerce input
    value <- coerce_string_to_numeric(value)

    new_r2ogs6_search_length_algorithm(type,
                                       value)
}


new_r2ogs6_search_length_algorithm <- function(type,
                                               value = NULL) {

    assertthat::assert_that(assertthat::is.string(type))

    validate_is_null_or_number(value)

    structure(
        list(
            type = type,
            value = value,
            is_subclass = FALSE,
            attr_names = character(),
            flatten_on_exp = character()
        ),
        class = "r2ogs6_search_length_algorithm"
    )
}
