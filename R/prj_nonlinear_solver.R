
#===== r2ogs6_nonlinear_solver =====


#'r2ogs6_nonlinear_solver
#'@description tag: nonlinear_solver
#'@param name string: Name
#'@param type string: Type
#'@param max_iter string | double: Maximal number of iterations
#'@param linear_solver string: Name of corresponding linear_solver
#'@param damping Optional: string | double: Damping
#'@export
r2ogs6_nonlinear_solver <- function(name,
                                    type,
                                    max_iter,
                                    linear_solver,
                                    damping = NULL){

    #Coerce input
    max_iter <- coerce_string_to_numeric(max_iter)
    damping <- coerce_string_to_numeric(damping)

    new_r2ogs6_nonlinear_solver(name,
                                type,
                                max_iter,
                                linear_solver,
                                damping)
}


new_r2ogs6_nonlinear_solver <- function(name,
                                        type,
                                        max_iter,
                                        linear_solver,
                                        damping = NULL){

    assertthat::assert_that(assertthat::is.string(name))
    assertthat::assert_that(assertthat::is.string(type))
    assertthat::assert_that(assertthat::is.number(max_iter))
    assertthat::assert_that(assertthat::is.string(linear_solver))

    validate_is_null_or_number(damping)

    structure(list(name = name,
                   type = type,
                   max_iter = max_iter,
                   linear_solver = linear_solver,
                   damping = damping,
                   is_subclass = FALSE,
                   attr_names = character(),
                   flatten_on_exp = character()
                   ),
              class = "r2ogs6_nonlinear_solver"
    )
}
