
#===== prj_nonlinear_solver =====


#' prj_nonlinear_solver
#' @description tag: nonlinear_solver
#' @param name string: Name
#' @param type string: Type
#' @param max_iter string | double: Maximal number of iterations
#' @param linear_solver string: Name of corresponding linear_solver
#' @param damping Optional: string | double: Damping
#' @example man/examples/ex_prj_nonlinear_solver.R
#' @export
prj_nonlinear_solver <- function(name,
                                    type,
                                    max_iter,
                                    linear_solver,
                                    damping = NULL){

    #Coerce input
    max_iter <- coerce_string_to_numeric(max_iter)
    damping <- coerce_string_to_numeric(damping)

    new_prj_nonlinear_solver(name,
                                type,
                                max_iter,
                                linear_solver,
                                damping)
}


new_prj_nonlinear_solver <- function(name,
                                        type,
                                        max_iter,
                                        linear_solver,
                                        damping = NULL){

    assertthat::assert_that(assertthat::is.string(name))
    assertthat::assert_that(assertthat::is.string(type))
    assertthat::assert_that(assertthat::is.number(max_iter))
    assertthat::assert_that(assertthat::is.string(linear_solver))

    are_null_or_numbers(damping)

    structure(list(name = name,
                   type = type,
                   max_iter = max_iter,
                   linear_solver = linear_solver,
                   damping = damping,
                   xpath = "nonlinear_solvers/nonlinear_solver",
                   attr_names = character(),
                   flatten_on_exp = character()
                   ),
              class = "prj_nonlinear_solver"
    )
}
