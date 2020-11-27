

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
                   petsc = petsc,
                   tag_name = "linear_solver",
                   is_subclass = FALSE,
                   attr_names = character(),
                   flatten_on_exp = character()
                   ),
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