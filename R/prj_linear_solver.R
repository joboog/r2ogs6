
#===== prj_linear_solver =====


#' prj_linear_solver
#' @description tag: linear_solver
#' @param name string: The name of the linear solver
#' @param eigen Optional: list: ...
#' @param lis Optional: string: ...
#' @param petsc Optional: character: ...
#' @example man/examples/ex_prj_linear_solver.R
#' @export
prj_linear_solver <- function(name,
                                 eigen = NULL,
                                 lis = NULL,
                                 petsc = NULL){

    #Coerce input

    new_prj_linear_solver(name,
                             eigen,
                             lis,
                             petsc)
}


new_prj_linear_solver <- function(name,
                                     eigen = NULL,
                                     lis = NULL,
                                     petsc = NULL){

    assertthat::assert_that(assertthat::is.string(name))

    is_null_or_has_class(eigen, "prj_eigen")
    are_null_or_strings(lis)

    if(!is.null(petsc)){
        assertthat::assert_that(is.list(petsc))
        assertthat::assert_that(length(petsc) %in% c(1, 2))

        if(length(petsc) == 1){
            names(petsc) <- c("parameters")
        }else{
            names(petsc) <- c("prefix", "parameters")
        }
    }

    structure(list(name = name,
                   eigen = eigen,
                   lis = lis,
                   petsc = petsc,
                   xpath = "linear_solvers/linear_solver",
                   attr_names = character(),
                   flatten_on_exp = character()
                   ),
              class = "prj_linear_solver"
    )
}


#===== prj_eigen =====


#' prj_eigen
#' @description tag: eigen
#' @param solver_type string:
#' @param precon_type Optional: string:
#' @param max_iteration_step Optional: string | double: ...
#' @param error_tolerance Optional: string | double: ...
#' @param scaling Optional: string | double: ...
#' @param restart Optional: string | double: ...
#' @example man/examples/ex_prj_eigen.R
#' @export
prj_eigen <- function(solver_type,
                         precon_type = NULL,
                         max_iteration_step = NULL,
                         error_tolerance = NULL,
                         scaling = NULL,
                         restart = NULL){

    #Coerce input
    max_iteration_step <- coerce_string_to_numeric(max_iteration_step)
    error_tolerance <- coerce_string_to_numeric(error_tolerance)
    restart <- coerce_string_to_numeric(restart)

    new_prj_eigen(solver_type,
                     precon_type,
                     max_iteration_step,
                     error_tolerance,
                     scaling,
                     restart)
}


new_prj_eigen <- function(solver_type,
                             precon_type = NULL,
                             max_iteration_step = NULL,
                             error_tolerance = NULL,
                             scaling = NULL,
                             restart = NULL){

    assertthat::assert_that(assertthat::is.string(solver_type))

    are_null_or_strings(precon_type,
                               scaling)
    are_null_or_numbers(max_iteration_step,
                               error_tolerance,
                               restart)

    structure(list(solver_type = solver_type,
                   precon_type = precon_type,
                   max_iteration_step = max_iteration_step,
                   error_tolerance = error_tolerance,
                   scaling = scaling,
                   restart = restart,
                   xpath = "linear_solvers/linear_solver/eigen",
                   attr_names = character(),
                   flatten_on_exp = character()
    ),
    class = "prj_eigen"
    )
}
