
#' prj_local_coordinate_system
#' @description tag: local_coordinate_system
#' @param basis_vector_0 prj_basis_vector_0: A basis vector
#' @param basis_vector_1 prj_basis_vector_1: A basis vector
#' @param basis_vector_2 Optional prj_basis_vector_2: A basis vector
#' @example man/examples/ex_prj_local_coordinate_system.R
#' @export
prj_local_coordinate_system <- function(basis_vector_0,
                                           basis_vector_1,
                                           basis_vector_2 = NULL) {

    new_prj_local_coordinate_system(basis_vector_0,
                                       basis_vector_1,
                                       basis_vector_2)
}


new_prj_local_coordinate_system <- function(basis_vector_0,
                                               basis_vector_1,
                                               basis_vector_2 = NULL) {

    assertthat::assert_that(class(basis_vector_0) == "prj_basis_vector_0")
    assertthat::assert_that(class(basis_vector_1) == "prj_basis_vector_1")
    is_null_or_has_class(basis_vector_2, "prj_basis_vector_2")

    structure(
        list(
            basis_vector_0 = basis_vector_0,
            basis_vector_1 = basis_vector_1,
            basis_vector_2 = basis_vector_2,
            xpath = "local_coordinate_system",
            attr_names = character(),
            flatten_on_exp = character()
        ),
        class = "prj_local_coordinate_system"
    )
}


#'prj_basis_vector_0
#'@description tag: basis_vector_0
#'@param name Optional:
#'@param implicit Optional: 
#'@export
prj_basis_vector_0 <- function(
                        name = NULL,
                        implicit = NULL) {

    new_prj_basis_vector_0(name, implicit)
}

new_prj_basis_vector_0 <- function(
                            name = NULL,
                            implicit = NULL) {

    are_null_or_strings(name)
    are_null_or_string_flags(implicit)
    
    structure(list(
        name = name,
        implicit = implicit,
        xpath = "local_coordinate_system/basis_vector_0",
        attr_names = c("implicit"),
        flatten_on_exp = character(),
        remove_name_on_exp = TRUE
        ),
        class = "prj_basis_vector_0"
    )
}

#'prj_basis_vector_1
#'@description tag: basis_vector_1
#'@param name Optional:
#'@param implicit Optional: 
#'@export
prj_basis_vector_1 <- function(
                        name = NULL,
                        implicit = NULL) {

    new_prj_basis_vector_1(name, implicit)
}

new_prj_basis_vector_1 <- function(
                            name = NULL,
                            implicit = NULL) {

    are_null_or_strings(name)
    are_null_or_string_flags(implicit)
    
    structure(list(
        name = name,
        implicit = implicit,
        xpath = "local_coordinate_system/basis_vector_1",
        attr_names = c("implicit"),
        flatten_on_exp = character(),
        remove_name_on_exp = TRUE
        ),
        class = "prj_basis_vector_1"
    )
}

#'prj_basis_vector_2
#'@description tag: basis_vector_2
#'@param name Optional:
#'@param implicit Optional: 
#'@export
prj_basis_vector_2 <- function(
                        name = NULL,
                        implicit = NULL) {

    new_prj_basis_vector_2(name, implicit)
}

new_prj_basis_vector_2 <- function(
                            name = NULL,
                            implicit = NULL) {

    are_null_or_strings(name)
    are_null_or_string_flags(implicit)
    
    structure(list(
        name = name,
        implicit = implicit,
        xpath = "local_coordinate_system/basis_vector_2",
        attr_names = c("implicit"),
        flatten_on_exp = character(),
        remove_name_on_exp = TRUE
        ),
        class = "prj_basis_vector_2"
    )
}