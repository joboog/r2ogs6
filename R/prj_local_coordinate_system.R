
#' r2ogs6_local_coordinate_system
#' @description tag: local_coordinate_system
#' @param basis_vector_0 string | double: A basis vector
#' @param basis_vector_1 string | double: A basis vector
#' @param basis_vector_2 Optional: string | double: A basis vector
#' @example man/examples/ex_prj_local_coordinate_system.R
#' @export
r2ogs6_local_coordinate_system <- function(basis_vector_0,
                                           basis_vector_1,
                                           basis_vector_2 = NULL) {

    new_r2ogs6_local_coordinate_system(basis_vector_0,
                                       basis_vector_1,
                                       basis_vector_2)
}


new_r2ogs6_local_coordinate_system <- function(basis_vector_0,
                                               basis_vector_1,
                                               basis_vector_2 = NULL) {

    assertthat::assert_that(assertthat::is.string(basis_vector_0))
    assertthat::assert_that(assertthat::is.string(basis_vector_1))

    are_null_or_strings(basis_vector_2)

    structure(
        list(
            basis_vector_0 = basis_vector_0,
            basis_vector_1 = basis_vector_1,
            basis_vector_2 = basis_vector_2,
            xpath = "local_coordinate_system",
            attr_names = character(),
            flatten_on_exp = character()
        ),
        class = "r2ogs6_local_coordinate_system"
    )
}
