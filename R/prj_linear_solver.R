
#===== r2ogs6_linear_solver =====


#'r2ogs6_linear_solver
#'@description tag: linear_solver
#'@param name string: The name of the linear solver
#'@param eigen Optional: list: ...
#'@param lis Optional: string: ...
#'@param petsc Optional: character: ...
#'@export
r2ogs6_linear_solver <- function(name,
                                 eigen = NULL,
                                 lis = NULL,
                                 petsc = NULL){

    #Coerce input

    new_r2ogs6_linear_solver(name,
                             eigen,
                             lis,
                             petsc)
}


new_r2ogs6_linear_solver <- function(name,
                                     eigen = NULL,
                                     lis = NULL,
                                     petsc = NULL){

    assertthat::assert_that(assertthat::is.string(name))

    validate_is_null_or_class_obj(eigen, "r2ogs6_eigen")
    validate_is_null_or_string(lis)

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
                   is_subclass = FALSE,
                   attr_names = character(),
                   flatten_on_exp = character()
                   ),
              class = "r2ogs6_linear_solver"
    )
}


#===== r2ogs6_eigen =====


#'r2ogs6_eigen
#'@description tag: eigen
#'@param solver_type string:
#'@param precon_type Optional: string:
#'@param max_iteration_step Optional: string | double: ...
#'@param error_tolerance Optional: string | double: ...
#'@param scaling Optional: string | double: ...
#'@param restart Optional: string | double: ...
#'@export
r2ogs6_eigen <- function(solver_type,
                         precon_type = NULL,
                         max_iteration_step = NULL,
                         error_tolerance = NULL,
                         scaling = NULL,
                         restart = NULL){

    #Coerce input
    max_iteration_step <- coerce_string_to_numeric(max_iteration_step)
    error_tolerance <- coerce_string_to_numeric(error_tolerance)
    restart <- coerce_string_to_numeric(restart)

    new_r2ogs6_eigen(solver_type,
                     precon_type,
                     max_iteration_step,
                     error_tolerance,
                     scaling,
                     restart)
}


new_r2ogs6_eigen <- function(solver_type,
                             precon_type = NULL,
                             max_iteration_step = NULL,
                             error_tolerance = NULL,
                             scaling = NULL,
                             restart = NULL){

    assertthat::assert_that(assertthat::is.string(solver_type))

    validate_is_null_or_string(precon_type,
                               scaling)
    validate_is_null_or_number(max_iteration_step,
                               error_tolerance,
                               restart)

    structure(list(solver_type = solver_type,
                   precon_type = precon_type,
                   max_iteration_step = max_iteration_step,
                   error_tolerance = error_tolerance,
                   scaling = scaling,
                   restart = restart,
                   is_subclass = TRUE,
                   attr_names = character(),
                   flatten_on_exp = character()
    ),
    class = "r2ogs6_eigen"
    )
}
