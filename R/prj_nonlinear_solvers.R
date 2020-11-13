#============================== NONLINEAR_SOLVERS CLASSES AND METHODS ================================

#============================== NONLINEAR_SOLVER================================


#'r2ogs6_nonlinear_solver
#'@description S3 class describing a .prj nonlinear solver
#'@param name The name of the nonlinear solver
#'@param type The type of the nonlinear solver
#'@param max_iter The maximal number of iterations
#'@param linear_solver The name of the linear solver
#'@export
r2ogs6_nonlinear_solver <- function(name, type, max_iter, linear_solver){

    #Make this more user friendly
    #...

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
                   linear_solver = linear_solver),
              class = "r2ogs6_nonlinear_solver"
    )
}


#'as_node.r2ogs6_nonlinear_solver
#'@description Implementation of generic function as_node for S3 class r2ogs6_nonlinear_solver
#'@param x A r2ogs6_nonlinear_solver class object
as_node.r2ogs6_nonlinear_solver <- function(x) {

    node <- list(nonlinear_solver = structure(list()))

    node <- add_children(node, list(name = x$name,
                                    type = x$type,
                                    max_iter = x$max_iter,
                                    linear_solver = x$linear_solver))

    return(node)
}


#'input_add.r2ogs6_nonlinear_solver
#'@description Implementation of generic function input_add for S3 class r2ogs6_nonlinear_solver
#'@param x A r2ogs6_nonlinear_solver class object
#'@param ogs6_obj A OGS6 class object
#'@export
input_add.r2ogs6_nonlinear_solver <- function(x, ogs6_obj) {
    ogs6_obj$add_nonlinear_solver(x)
}
