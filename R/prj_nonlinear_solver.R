

#'r2ogs6_nonlinear_solver
#'@description S3 class describing a .prj nonlinear solver
#'@param name The name of the nonlinear solver
#'@param type The type of the nonlinear solver
#'@param max_iter The maximal number of iterations
#'@param linear_solver The name of the linear solver
#'@export
r2ogs6_nonlinear_solver <- function(name, type, max_iter, linear_solver){

    #Coerce input
    if(assertthat::is.string(max_iter)){
        max_iter <- as.double(max_iter)
    }

    new_r2ogs6_nonlinear_solver(name, type, max_iter, linear_solver)
}


new_r2ogs6_nonlinear_solver <- function(name, type, max_iter, linear_solver){

    assertthat::assert_that(assertthat::is.string(name))
    assertthat::assert_that(assertthat::is.string(type))
    assertthat::assert_that(assertthat::is.number(max_iter))
    assertthat::assert_that(assertthat::is.string(linear_solver))

    structure(list(name = name,
                   type = type,
                   max_iter = max_iter,
                   linear_solver = linear_solver,
                   tag_name = "nonlinear_solver",
                   is_subclass = FALSE,
                   attr_names = character(),
                   flatten_on_exp = character()
                   ),
              class = "r2ogs6_nonlinear_solver"
    )
}

