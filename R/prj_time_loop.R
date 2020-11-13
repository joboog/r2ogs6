#============================== TIME_LOOP CLASSES AND METHODS ================================


#'r2ogs6_time_loop
#'@description S3 class describing a .prj time_loop
#'@param processes A list of r2ogs6_tl_process class objects
#'@param output A r2ogs6_tl_output class object
#'@param global_processes_coupling ...
#'@export
r2ogs6_time_loop <- function(processes, output, global_processes_coupling = NULL) {

    #Make this more user friendly
    #...

    new_r2ogs6_time_loop(processes, output, global_processes_coupling)
}


new_r2ogs6_time_loop <- function(processes, output, global_processes_coupling = NULL) {

    validate_wrapper_list(processes, "r2ogs6_tl_process")
    assertthat::assert_that(class(output) == "r2ogs6_tl_output")

    if(!is.null(global_processes_coupling)){
        assertthat::assert_that(class(global_processes_coupling) == "r2ogs6_tl_gpc")
    }

    structure(
        list(
            processes = processes,
            output = output,
            global_processes_coupling = global_processes_coupling
        ),
        class = "r2ogs6_time_loop"
    )
}


#'as_node.r2ogs6_time_loop
#'@description Implementation of generic function as_node for S3 class r2ogs6_time_loop
#'@param x A r2ogs6_time_loop class object
as_node.r2ogs6_time_loop <- function(x) {
    node <- list(time_loop = structure(list()))

    processes_node <- adopt_nodes("processes", x$processes)

    node <- add_children(node, list(as_node(x$global_processes_coupling),
                                    processes_node,
                                    as_node(x$output)))

    return(invisible(node))
}


#'input_add.r2ogs6_time_loop
#'@description Implementation of generic function input_add for S3 class r2ogs6_time_loop
#'@param x A r2ogs6_time_loop class object
#'@param ogs6_obj A OGS6 class object
#'@export
input_add.r2ogs6_time_loop <- function(x, ogs6_obj){
    ogs6_obj$add_time_loop(x)
}


#============================== TIME_LOOP GLOBAL PROCESSES COUPLING ================================

#WIP!!!!!!!!!!!!!!

new_r2ogs6_global_processes_coupling <- function() {

    structure(
        list(
        ),
        class = "r2ogs6_global_processes_coupling"
    )
}

#'as_node.r2ogs6_global_processes_coupling
#'@description Implementation of generic function as_node for S3 class r2ogs6_global_processes_coupling
#'@param x A r2ogs6_global_processes_coupling class object
as_node.r2ogs6_global_processes_coupling <- function(x) {
    node <- list(global_processes_coupling = structure(list()))

    node <- add_children(node, list())

    return(invisible(node))
}


#============================== TIME_LOOP PROCESS ================================


#'r2ogs6_tl_process
#'@description S3 class describing a .prj time_loop process
#'@param ref References a r2ogs6_process object by name
#'@param nonlinear_solver ...
#'@param convergence_criterion ...
#'@param time_discretization ...
#'@param time_stepping ...
#'@export
r2ogs6_tl_process <- function(ref, nonlinear_solver, convergence_criterion,
                              time_discretization, time_stepping) {

    #Make this more user friendly
    #...

    new_r2ogs6_tl_process(ref, nonlinear_solver, convergence_criterion,
                          time_discretization, time_stepping)
}


new_r2ogs6_tl_process <- function(ref, nonlinear_solver, convergence_criterion,
                                  time_discretization, time_stepping) {

    assertthat::assert_that(assertthat::is.string(ref))
    assertthat::assert_that(assertthat::is.string(nonlinear_solver))
    assertthat::assert_that(is.vector(convergence_criterion))
    assertthat::assert_that(is.vector(time_discretization))
    assertthat::assert_that(is.list(time_stepping))

    structure(
        list(ref = ref,
             nonlinear_solver = nonlinear_solver,
             convergence_criterion = convergence_criterion,
             time_discretization = time_discretization,
             time_stepping = time_stepping
        ),
        class = "r2ogs6_tl_process"
    )
}


#'as_node.r2ogs6_tl_process
#'@description Implementation of generic function as_node for S3 class r2ogs6_tl_process
#'@param x A r2ogs6_tl_process class object
as_node.r2ogs6_tl_process <- function(x) {
    node <- list(process = structure(list(), ref = x$ref))

    convergence_criterion_node <- simple_vector_to_node("convergence_criterion",
                                                        x$convergence_criterion)
    time_discretization_node <- simple_vector_to_node("time_discretization",
                                                      x$time_discretization)

    time_stepping_node <- list(time_stepping = structure(list()))
    timesteps_node <- timesteps_as_node(x$time_stepping[[4]])
    time_stepping_node <- add_children(time_stepping_node, list(type = x$time_stepping[[1]],
                                                                t_initial = x$time_stepping[[2]],
                                                                t_end = x$time_stepping[[3]],
                                                                timesteps_node))


    node <- add_children(node, list(nonlinear_solver = x$nonlinear_solver,
                                    convergence_criterion_node,
                                    time_discretization_node,
                                    time_stepping_node))

    return(invisible(node))
}


#============================== TIME_LOOP OUTPUT ================================


#'r2ogs6_tl_output
#'@description S3 class describing a .prj time_loop output
#'@param type ...
#'@param prefix ...
#'@param suffix ...
#'@param timesteps ...
#'@param variables ...
#'@param compress_output Optional: Should the output be compressed?
#'@export
r2ogs6_tl_output <- function(type, prefix, suffix, timesteps, variables, compress_output = NULL) {

    #Make this more user friendly
    #...

    new_r2ogs6_tl_output(type, prefix, suffix, timesteps, variables, compress_output)
}


new_r2ogs6_tl_output <- function(type, prefix, suffix, timesteps, variables, compress_output = NULL) {

    assertthat::assert_that(assertthat::is.string(type))
    assertthat::assert_that(assertthat::is.string(prefix))
    assertthat::assert_that(assertthat::is.string(suffix))

    assertthat::assert_that(is.list(timesteps))
    assertthat::assert_that(is.list(variables))

    names(variables) <- rep("variable", length(variables))

    if(!is.null(compress_output)){
        valid_vals <- c("false", "true")
        assertthat::assert_that(compress_output %in% valid_vals)
    }

    structure(
        list(type = type,
             prefix = prefix,
             suffix = suffix,
             timesteps = timesteps,
             variables = variables,
             compress_output = compress_output
        ),
        class = "r2ogs6_tl_output"
    )
}


#'as_node.r2ogs6_tl_output
#'@description Implementation of generic function as_node for S3 class r2ogs6_tl_output
#'@param x A r2ogs6_tl_output class object
as_node.r2ogs6_tl_output <- function(x) {
    node <- list(output = structure(list()))

    timesteps_node <- timesteps_as_node(x$timesteps, TRUE)
    variables_node <- simple_vector_to_node("variables", x$variables)

    node <- add_children(node, list(type = x$type,
                                    prefix = x$prefix,
                                    suffix = x$suffix,
                                    compress_output = x$compress_output,
                                    timesteps_node,
                                    variables_node
                                    ))

    return(invisible(node))
}

#Helper
timesteps_as_node <- function(timesteps, in_output = FALSE){

    node <- list(timesteps = structure(list()))

    for(i in seq_len(length(timesteps))){
        names(timesteps[[i]])[[1]] <- "repeat"

        if(!in_output){
            names(timesteps[[i]])[[2]] <- "delta_t"
        }else{
            names(timesteps[[i]])[[2]] <- "each_steps"
        }

        node[[1]] <- c(node[[1]], simple_vector_to_node("pair", timesteps[[i]]))
    }

    return(invisible(node))
}
