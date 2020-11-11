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
#'@param obj A r2ogs6_time_loop class object
as_node.r2ogs6_time_loop <- function(obj) {
    node <- list(time_loop = structure(list()))

    node <- add_children(node, list(as_node(obj$global_processes_coupling),
                                    as_node(obj$processes),
                                    as_node(obj$output)))

    return(node)
}


#'input_add.r2ogs6_time_loop
#'@description Implementation of generic function input_add for S3 class r2ogs6_time_loop
#'@param obj A r2ogs6_time_loop class object
#'@param ogs6_obj A OGS6 class object
#'@export
input_add.r2ogs6_time_loop <- function(obj, ogs6_obj){
    ogs6_obj$add_time_loop(obj)
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

    #Val...

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
#'@param obj A r2ogs6_tl_process class object
as_node.r2ogs6_tl_process <- function(obj) {
    node <- list(process = structure(list(), ref = obj$ref))

    node <- add_children(node, list(nonlinear_solver = obj$nonlinear_solver,
                                    convergence_criterion = obj$convergence_criterion,
                                    time_discretization = obj$time_discretization,
                                    time_stepping = obj$time_stepping))

    return(node)
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
#'@param obj A r2ogs6_tl_output class object
as_node.r2ogs6_tl_output <- function(obj) {
    node <- list(output = structure(list()))

    node <- add_children(node, list(type = obj$type,
                                    prefix = obj$prefix,
                                    suffix = obj$suffix,
                                    compress_output = obj$compress_output,
                                    timesteps = obj$timesteps,
                                    variables = obj$variables
                                    ))

    return(node)
}
