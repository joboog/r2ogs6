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

    validate_r2ogs6_linear_solver(new_r2ogs6_linear_solver(name, eigen, lis, petsc))
}


new_r2ogs6_linear_solver <- function(name, eigen, lis = NULL, petsc = NULL){

    assertthat::assert_that(assertthat::is.string(name))

    assertthat::assert_that(is.list(eigen))
    assertthat::assert_that(length(eigen) == 4)
    names(eigen) <- c("solver_type", "precon_type", "max_iteration_step", "error_tolerance")

    if(!is.null(lis)){
        assertthat::assert_that(assertthat::is.string(lis))
    }

    if(!is.null(petsc)){
        assertthat::assert_that(is.character(petsc))
        assertthat::assert_that(length(petsc) == 2)
        names(petsc) <- c("prefix", "parameters")
    }

    structure(list(name = name,
                   eigen = eigen,
                   lis = lis,
                   petsc = petsc),
              class = "r2ogs6_linear_solver"
    )
}


validate_r2ogs6_linear_solver <- function(r2ogs6_linear_solver){

    #Coerce input
    if(assertthat::is.string(r2ogs6_linear_solver$eigen[[3]])){
        r2ogs6_linear_solver$eigen[[3]] <- as.double(r2ogs6_linear_solver$eigen[[3]])
    }

    if(assertthat::is.string(r2ogs6_linear_solver$eigen[[4]])){
        r2ogs6_linear_solver$eigen[[4]] <- as.double(r2ogs6_linear_solver$eigen[[4]])
    }

    return(invisible(r2ogs6_linear_solver))
}


#'as_node.r2ogs6_linear_solver
#'@description Implementation of generic function as_node for S3 class r2ogs6_linear_solver
#'@param x A r2ogs6_linear_solver class object
as_node.r2ogs6_linear_solver <- function(x) {

    node <- list(linear_solver = structure(list()))

    eigen_node <- simple_vector_to_node("eigen", x$eigen)

    node <- add_children(node, list(name = x$name,
                                    eigen_node,
                                    lis = x$lis,
                                    petsc = x$petsc))

    return(node)
}


#'input_add.r2ogs6_linear_solver
#'@description Implementation of generic function input_add for S3 class r2ogs6_linear_solver
#'@param x A r2ogs6_linear_solver class object
#'@param ogs6_obj A OGS6 class object
#'@export
input_add.r2ogs6_linear_solver <- function(x, ogs6_obj) {
    ogs6_obj$add_linear_solver(x)
}