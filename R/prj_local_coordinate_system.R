
#'r2ogs6_local_coordinate_system
#'@description tag: local_coordinate_system
#'@param basis_vector_0 string | double: A basis vector
#'@param basis_vector_1 string | double: A basis vector
#'@param basis_vector_2 Optional: string | double: A basis vector
#'@export
r2ogs6_local_coordinate_system <- function(basis_vector_0,
                                           basis_vector_1,
                                           basis_vector_2 = NULL) {

    #Coerce input
    basis_vector_0 <- coerce_string_to_numeric(basis_vector_0)
    basis_vector_1 <- coerce_string_to_numeric(basis_vector_1)
    basis_vector_2 <- coerce_string_to_numeric(basis_vector_2)

    new_r2ogs6_local_coordinate_system(basis_vector_0,
                                       basis_vector_1,
                                       basis_vector_2)
}


new_r2ogs6_local_coordinate_system <- function(basis_vector_0,
                                               basis_vector_1,
                                               basis_vector_2 = NULL) {

    assertthat::assert_that(is.double(basis_vector_0))
    assertthat::assert_that(is.double(basis_vector_1))

    validate_is_null_or_number(basis_vector_1)

    structure(
        list(
            basis_vector_0 = basis_vector_0,
            basis_vector_1 = basis_vector_1,
            basis_vector_2 = basis_vector_2,
            tag_name = "local_coordinate_system",
            is_subclass = FALSE,
            attr_names = character(),
            flatten_on_exp = character()
        ),
        class = "r2ogs6_local_coordinate_system"
    )
}