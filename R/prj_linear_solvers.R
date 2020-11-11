#============================== LINEAR_SOLVERS CLASSES AND METHODS ================================

#============================== LINEAR_SOLVER================================


#'r2ogs6_linear_solver
#'@description S3 class describing a .prj linear solver
#'@param name The name of the linear solver
#'@param eigen ...
#'@param lis ...
#'@param petsc ...
#'@export
r2ogs6_linear_solver <- function(name, eigen, lis = NULL, petsc = NULL){

    #Make this more user friendly
    #...

    new_r2ogs6_linear_solver(name, eigen, lis, petsc)
}


new_r2ogs6_linear_solver <- function(name, eigen, lis = NULL, petsc = NULL){

    assertthat::assert_that(assertthat::is.string(name))

    validate_param_list(eigen, 4, c("solver_type", "precon_type", "max_iteration_step", "error_tolerance"))

    if(!is.null(lis)){
        assertthat::assert_that(assertthat::is.string(lis))
    }

    if(!is.null(petsc)){
        validate_param_list(petsc, 2, c("prefix", "parameters"))
    }

    structure(list(name = name,
                   eigen = eigen,
                   lis = lis,
                   petsc = petsc),
              class = "r2ogs6_linear_solver"
    )
}


#'as_node.r2ogs6_linear_solver
#'@description Implementation of generic function as_node for S3 class r2ogs6_linear_solver
#'@param obj A r2ogs6_linear_solver class object
as_node.r2ogs6_linear_solver <- function(obj) {

    node <- list(linear_solver = structure(list()))

    node <- add_children(node, list(name = obj$name,
                                    eigen = obj$eigen,
                                    lis = obj$lis,
                                    petsc = obj$petsc))

    return(node)
}


#'input_add.r2ogs6_linear_solver
#'@description Implementation of generic function input_add for S3 class r2ogs6_linear_solver
#'@param obj A r2ogs6_linear_solver class object
#'@param ogs6_obj A OGS6 class object
#'@export
input_add.r2ogs6_linear_solver <- function(obj, ogs6_obj) {
    ogs6_obj$add_linear_solver(obj)
}