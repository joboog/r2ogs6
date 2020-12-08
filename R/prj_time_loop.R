
#===== r2ogs6_time_loop =====


#'r2ogs6_time_loop
#'@description tag: time_loop
#'@param processes list, r2ogs6_tl_process:
#'@param output r2ogs6_output:
#'@param global_process_coupling Optional: r2ogs6_global_process_coupling:
#'@export
r2ogs6_time_loop <- function(processes,
                             output,
                             global_process_coupling = NULL) {

    #Make this more user friendly
    #...

    new_r2ogs6_time_loop(processes,
                         output,
                         global_process_coupling)
}


new_r2ogs6_time_loop <- function(processes,
                                 output,
                                 global_process_coupling = NULL) {

    validate_wrapper_list(processes, "r2ogs6_tl_process")
    assertthat::assert_that(class(output) == "r2ogs6_output")

    validate_is_null_or_class_obj(global_process_coupling,
                                  "r2ogs6_global_process_coupling")

    structure(
        list(
            processes = processes,
            output = output,
            global_process_coupling = global_process_coupling,
            is_subclass = FALSE,
            attr_names = character(),
            flatten_on_exp = character()
        ),
        class = "r2ogs6_time_loop"
    )
}


#===== r2ogs6_tl_process =====


#'r2ogs6_tl_process
#'@description tag: process (parent: time_loop, NOT processes!)
#'@param ref string: References a r2ogs6_process object by name
#'@param nonlinear_solver string:
#'@param convergence_criterion r2ogs6_convergence_criterion:
#'@param time_discretization vector:
#'@param time_stepping list:
#'@param compensate_non_equilibrium_initial_residuum string: Optional: Either
#'"true" or "false"
#'@export
r2ogs6_tl_process <- function(ref,
                              nonlinear_solver,
                              convergence_criterion,
                              time_discretization,
                              time_stepping,
                              compensate_non_equilibrium_initial_residuum =
                                  NULL) {

    #Make this more user friendly
    #...

    validate_r2ogs6_tl_process(
        new_r2ogs6_tl_process(ref,
                              nonlinear_solver,
                              convergence_criterion,
                              time_discretization,
                              time_stepping,
                              compensate_non_equilibrium_initial_residuum))
}


new_r2ogs6_tl_process <- function(ref,
                                  nonlinear_solver,
                                  convergence_criterion,
                                  time_discretization,
                                  time_stepping,
                                  compensate_non_equilibrium_initial_residuum =
                                      NULL) {

    assertthat::assert_that(assertthat::is.string(ref))
    assertthat::assert_that(assertthat::is.string(nonlinear_solver))

    assertthat::assert_that(class(convergence_criterion) ==
                                "r2ogs6_convergence_criterion")

    assertthat::assert_that(is.vector(time_discretization))

    time_stepping <- validate_param_list(time_stepping, c("type",
                                                          "t_initial",
                                                          "t_end",
                                                          "timesteps"))

    validate_is_null_or_str_flag(compensate_non_equilibrium_initial_residuum)

    structure(
        list(ref = ref,
             nonlinear_solver = nonlinear_solver,
             convergence_criterion = convergence_criterion,
             time_discretization = time_discretization,
             time_stepping = time_stepping,
             compensate_non_equilibrium_initial_residuum =
                 compensate_non_equilibrium_initial_residuum,
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
        r2ogs6_tl_process$time_stepping[[2]] <-
            as.double(r2ogs6_tl_process$time_stepping[[2]])
    }

    if(assertthat::is.string(r2ogs6_tl_process$time_stepping[[3]])){
        r2ogs6_tl_process$time_stepping[[3]] <-
            as.double(r2ogs6_tl_process$time_stepping[[3]])
    }

    r2ogs6_tl_process$time_stepping[[4]] <-
        validate_timesteps(r2ogs6_tl_process$time_stepping[[4]])

    return(invisible(r2ogs6_tl_process))
}


#===== r2ogs6_output =====


#'r2ogs6_output
#'@description tag: output
#'@param type string:
#'@param prefix string:
#'@param variables vector:
#'@param suffix Optional: string:
#'@param timesteps Optional:
#'@param compress_output Optional: string: Should the output be compressed?
#' Either "true" or "false"
#'@param data_mode Optional: string:
#'@param output_iteration_results Optional: string: Either "true" or "false"
#'@param meshes Optional: character: A vector of mesh names
#'@param fixed_output_times Optional: string | numeric:
#'@export
r2ogs6_output <- function(type,
                             prefix,
                             variables,
                             suffix = NULL,
                             timesteps = NULL,
                             compress_output = NULL,
                             data_mode = NULL,
                             output_iteration_results = NULL,
                             meshes = NULL,
                             fixed_output_times = NULL) {

    #Coerce input
    fixed_output_times <- coerce_string_to_numeric(fixed_output_times, TRUE)

    new_r2ogs6_output(type,
                         prefix,
                         variables,
                         suffix,
                         timesteps,
                         compress_output,
                         data_mode,
                         output_iteration_results,
                         meshes,
                         fixed_output_times)
}


new_r2ogs6_output <- function(type,
                                 prefix,
                                 variables,
                                 suffix = NULL,
                                 timesteps = NULL,
                                 compress_output = NULL,
                                 data_mode = NULL,
                                 output_iteration_results = NULL,
                                 meshes = NULL,
                                 fixed_output_times = NULL) {

    assertthat::assert_that(assertthat::is.string(type))
    assertthat::assert_that(assertthat::is.string(prefix))
    assertthat::assert_that(is.vector(variables))
    names(variables) <- rep("variable", length(variables))

    validate_is_null_or_string(suffix,
                               data_mode)


    if(!is.null(timesteps)){
        timesteps <- validate_timesteps(timesteps, TRUE)
    }

    validate_is_null_or_str_flag(compress_output,
                                 output_iteration_results)

    if(!is.null(meshes)){
        assertthat::assert_that(is.character(meshes))
        names(meshes) <- rep("mesh", length(meshes))
    }

    validate_is_null_or_numeric(fixed_output_times)

    structure(
        list(type = type,
             prefix = prefix,
             variables = variables,
             suffix = suffix,
             timesteps = timesteps,
             compress_output = compress_output,
             data_mode = data_mode,
             output_iteration_results = output_iteration_results,
             meshes = meshes,
             fixed_output_times = fixed_output_times,
             is_subclass = TRUE,
             attr_names = character(),
             flatten_on_exp = c("fixed_output_times")
        ),
        class = "r2ogs6_output"
    )
}


#===== r2ogs6_global_process_coupling =====


#'r2ogs6_global_process_coupling
#'@description tag: global_process_coupling
#'@param max_iter string | double: Maximal number of iterations
#'@param convergence_criteria list, r2ogs6_convergence_criterion:
#' Convergence criteria
#'@export
r2ogs6_global_process_coupling <- function(max_iter,
                                           convergence_criteria) {

    #Coerce input
    max_iter <- coerce_string_to_numeric(max_iter)

    new_r2ogs6_global_process_coupling(max_iter,
                                       convergence_criteria)
}


new_r2ogs6_global_process_coupling <- function(max_iter,
                                               convergence_criteria) {

    assertthat::assert_that(is.double(max_iter))

    validate_wrapper_list(convergence_criteria,
                          "r2ogs6_convergence_criterion")

    structure(
        list(
            max_iter = max_iter,
            convergence_criteria = convergence_criteria,
            is_subclass = TRUE,
            attr_names = character(),
            flatten_on_exp = character()
        ),
        class = "r2ogs6_global_process_coupling"
    )
}


#===== r2ogs6_convergence_criterion =====


#'r2ogs6_convergence_criterion
#'@description tag: convergence_criterion
#'@param type string: Type
#'@param norm_type string: ...
#'@param abstol string | double: Absolute tolerance
#'@param reltol string | double: Relative tolerance
#'@param abstols string | numeric: Absolute tolerances
#'@param reltols string | numeric: Relative tolerances
#'@export
r2ogs6_convergence_criterion <- function(type,
                                         norm_type,
                                         abstol = NULL,
                                         reltol = NULL,
                                         abstols = NULL,
                                         reltols = NULL) {

    #Coerce input
    abstols <- coerce_string_to_numeric(abstols, TRUE)
    reltols <- coerce_string_to_numeric(reltols, TRUE)

    new_r2ogs6_convergence_criterion(type,
                                     norm_type,
                                     abstol,
                                     reltol,
                                     abstols,
                                     reltols)
}


new_r2ogs6_convergence_criterion <- function(type,
                                             norm_type,
                                             abstol = NULL,
                                             reltol = NULL,
                                             abstols = NULL,
                                             reltols = NULL) {

    assertthat::assert_that(assertthat::is.string(type))
    assertthat::assert_that(assertthat::is.string(norm_type))

    validate_is_null_or_number(abstol,
                               reltol)
    validate_is_null_or_numeric(abstols,
                                reltols)

    structure(
        list(
            type = type,
            norm_type = norm_type,
            abstol = abstol,
            reltol = reltol,
            abstols = abstols,
            reltols = reltols,
            is_subclass = TRUE,
            attr_names = character(),
            flatten_on_exp = c("abstols", "reltols")
        ),
        class = "r2ogs6_convergence_criterion"
    )
}


#===== timesteps validation =====


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
