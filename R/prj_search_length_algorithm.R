
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

    are_null_or_numbers(value)

    structure(
        list(
            type = type,
            value = value,
            xpath = "search_length_algorithm",
            attr_names = character(),
            flatten_on_exp = character()
        ),
        class = "r2ogs6_search_length_algorithm"
    )
}
