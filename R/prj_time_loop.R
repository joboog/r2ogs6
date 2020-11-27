

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
        assertthat::assert_that(class(global_processes_coupling) == "r2ogs6_global_processes_coupling")
    }

    structure(
        list(
            processes = processes,
            output = output,
            global_processes_coupling = global_processes_coupling,
            tag_name = "time_loop",
            is_subclass = FALSE,
            attr_names = character(),
            flatten_on_exp = character()
        ),
        class = "r2ogs6_time_loop"
    )
}


#WIP!!!!!!!!!!!!!!
r2ogs6_global_processes_coupling <- function() {

    new_r2ogs6_global_processes_coupling()
}


new_r2ogs6_global_processes_coupling <- function() {

    structure(
        list(
        ),
        class = "r2ogs6_global_processes_coupling"
    )
}


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

    validate_r2ogs6_tl_process(new_r2ogs6_tl_process(ref, nonlinear_solver, convergence_criterion,
                          time_discretization, time_stepping))
}


new_r2ogs6_tl_process <- function(ref, nonlinear_solver, convergence_criterion,
                                  time_discretization, time_stepping) {

    assertthat::assert_that(assertthat::is.string(ref))
    assertthat::assert_that(assertthat::is.string(nonlinear_solver))

    assertthat::assert_that(is.list(convergence_criterion))
    assertthat::assert_that(is.vector(time_discretization))

    assertthat::assert_that(is.list(time_stepping))
    assertthat::assert_that(length(time_stepping) == 4)
    names(time_stepping) <- c("type", "t_initial", "t_end", "timesteps")


    structure(
        list(ref = ref,
             nonlinear_solver = nonlinear_solver,
             convergence_criterion = convergence_criterion,
             time_discretization = time_discretization,
             time_stepping = time_stepping,
             tag_name = "process",
             is_subclass = TRUE,
             attr_names = c("ref"),
             flatten_on_exp = character()
        ),
        class = "r2ogs6_tl_process"
    )
}


validate_r2ogs6_tl_process <- function(r2ogs6_tl_process) {

    #Coerce input
    if(assertthat::is.string(r2ogs6_tl_process$time_stepping[[2]])){
        r2ogs6_tl_process$time_stepping[[2]] <- as.double(r2ogs6_tl_process$time_stepping[[2]])
    }

    if(assertthat::is.string(r2ogs6_tl_process$time_stepping[[3]])){
        r2ogs6_tl_process$time_stepping[[3]] <- as.double(r2ogs6_tl_process$time_stepping[[3]])
    }

    r2ogs6_tl_process$time_stepping[[4]] <- validate_timesteps(r2ogs6_tl_process$time_stepping[[4]])

    return(invisible(r2ogs6_tl_process))
}


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

    timesteps <- validate_timesteps(timesteps, TRUE)

    assertthat::assert_that(is.vector(variables))
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
             compress_output = compress_output,
             tag_name = "output",
             is_subclass = TRUE,
             attr_names = character(),
             flatten_on_exp = character()
        ),
        class = "r2ogs6_tl_output"
    )
}


#Validation helper function
validate_timesteps <- function(timesteps, in_output = FALSE){

    assertthat::assert_that(is.list(timesteps))

    for(i in seq_len(length(timesteps))){

        assertthat::assert_that(is.vector(timesteps[[i]]))
        assertthat::assert_that(length(timesteps[[i]]) == 2)

        names(timesteps[[i]])[[1]] <- "repeat"

        #Coerce input
        if(assertthat::is.string(timesteps[[i]][[1]])){
            timesteps[[i]][[1]] <- as.double(timesteps[[i]][[1]])
        }

        if(assertthat::is.string(timesteps[[i]][[2]])){
            timesteps[[i]][[2]] <- as.double(timesteps[[i]][[2]])
        }

        if(!in_output){
            names(timesteps[[i]])[[2]] <- "delta_t"
        }else{
            names(timesteps[[i]])[[2]] <- "each_steps"
        }
    }

    names(timesteps) <- rep("pair", length(timesteps))

    return(invisible(timesteps))
}
