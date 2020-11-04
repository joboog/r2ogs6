#============================== TIME_LOOP CLASSES AND METHODS ================================


#'r2ogs6_time_loop
#'@description S3 class describing a .prj time_loop
#'@param processes ...
#'@param output ...
#'@param global_processes_coupling ...
#'@export
r2ogs6_time_loop <- function(processes, output, global_processes_coupling = NULL) {

    #Make this more user friendly
    #...

    new_r2ogs6_time_loop(processes, output, global_processes_coupling)
}


#'new_r2ogs6_time_loop
#'@description Constructor for S3 class r2ogs6_time_loop
new_r2ogs6_time_loop <- function(processes, output, global_processes_coupling = NULL) {

    #Val...

    structure(
        list(
            processes = processes,
            output = output,
            global_processes_coupling = global_processes_coupling
        ),
        class = "r2ogs6_time_loop"
    )
}


#'WIP
#'@description
as_node.r2ogs6_time_loop <- function(obj) {
    time_loop_node <- list(time_loop = structure(list()))

    time_loop_node <- add_children(time_loop_node, list(global_processes_coupling = obj$global_processes_coupling,
                                                        processes = obj$processes,
                                                        output = obj$output))

    return(time_loop_node)
}


#'WIP
#'@description Adds a time loop
input_add.r2ogs6_time_loop <- function(time_loop, ogs6_obj){

    check_for_input_of_name(ogs6_obj, "prj_obj", TRUE, TRUE, "input_add_prj_obj")

    ogs6_obj$set_sim_input_obj_param("prj_obj", "time_loop", time_loop)
}


#'r2ogs6_tl_process
#'@description S3 class describing a .prj time_loop process
#'@param nonlinear_solver ...
#'@param convergence_criterion ...
#'@param time_discretization ...
#'@param time_stepping ...
#'@export
r2ogs6_tl_process <- function(nonlinear_solver, convergence_criterion,
                              time_discretization, time_stepping) {

    #Make this more user friendly
    #...

    new_r2ogs6_tl_process(nonlinear_solver, convergence_criterion,
                          time_discretization, time_stepping)
}


