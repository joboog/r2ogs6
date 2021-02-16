
#===== prj_time_loop =====


#' prj_time_loop
#' @description tag: time_loop
#' @param processes list, prj_tl_process:
#' @param output prj_output:
#' @param global_process_coupling Optional: prj_global_process_coupling:
#' @example man/examples/ex_prj_time_loop.R
#' @export
prj_time_loop <- function(processes,
                             output,
                             global_process_coupling = NULL) {

    #Make this more user friendly
    #...

    new_prj_time_loop(processes,
                         output,
                         global_process_coupling)
}


new_prj_time_loop <- function(processes,
                                 output,
                                 global_process_coupling = NULL) {

    is_wrapper_list(processes, "prj_tl_process")
    assertthat::assert_that(class(output) == "prj_output")

    is_null_or_has_class(global_process_coupling,
                                  "prj_global_process_coupling")

    structure(
        list(
            processes = processes,
            output = output,
            global_process_coupling = global_process_coupling,
            xpath = "time_loop",
            attr_names = character(),
            flatten_on_exp = character()
        ),
        class = "prj_time_loop"
    )
}


#===== prj_tl_process =====


#' prj_tl_process
#' @description tag: process (parent: time_loop, NOT processes!)
#' @param ref string: References a prj_process object by name
#' @param nonlinear_solver string:
#' @param convergence_criterion prj_convergence_criterion:
#' @param time_discretization vector:
#' @param time_stepping prj_time_stepping:
#' @param compensate_non_equilibrium_initial_residuum string: Optional: Either
#'   "true" or "false"
#' @example man/examples/ex_prj_tl_process.R
#' @export
prj_tl_process <- function(ref,
                              nonlinear_solver,
                              convergence_criterion,
                              time_discretization,
                              time_stepping,
                              compensate_non_equilibrium_initial_residuum =
                                  NULL) {

    #Make this more user friendly
    #...

    new_prj_tl_process(
        ref,
        nonlinear_solver,
        convergence_criterion,
        time_discretization,
        time_stepping,
        compensate_non_equilibrium_initial_residuum
    )
}


new_prj_tl_process <- function(ref,
                                  nonlinear_solver,
                                  convergence_criterion,
                                  time_discretization,
                                  time_stepping,
                                  compensate_non_equilibrium_initial_residuum =
                                      NULL) {

    assertthat::assert_that(assertthat::is.string(ref))
    assertthat::assert_that(assertthat::is.string(nonlinear_solver))

    assertthat::assert_that(class(convergence_criterion) ==
                                "prj_convergence_criterion")

    assertthat::assert_that(is.vector(time_discretization))

    assertthat::assert_that(class(time_stepping) == "prj_time_stepping")

    if(!is.null(compensate_non_equilibrium_initial_residuum)){
        compensate_non_equilibrium_initial_residuum <-
            stringr::str_remove_all(compensate_non_equilibrium_initial_residuum,
                                    "[:space:]*")
    }

    are_null_or_string_flags(compensate_non_equilibrium_initial_residuum)

    structure(
        list(ref = ref,
             nonlinear_solver = nonlinear_solver,
             convergence_criterion = convergence_criterion,
             time_discretization = time_discretization,
             time_stepping = time_stepping,
             compensate_non_equilibrium_initial_residuum =
                 compensate_non_equilibrium_initial_residuum,
             xpath = "time_loop/processes/process",
             attr_names = c("ref"),
             flatten_on_exp = character()
        ),
        class = "prj_tl_process"
    )
}


#===== prj_output =====


#' prj_output
#' @description tag: output
#' @param type string:
#' @param prefix string:
#' @param variables vector:
#' @param suffix Optional: string:
#' @param timesteps Optional:
#' @param compress_output Optional: string: Should the output be compressed?
#'   Either "true" or "false"
#' @param data_mode Optional: string:
#' @param output_iteration_results Optional: string: Either "true" or "false"
#' @param meshes Optional: character: A vector of mesh names
#' @param fixed_output_times Optional: string | numeric:
#' @example man/examples/ex_prj_output.R
#' @export
prj_output <- function(type,
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
    fixed_output_times <- coerce_string_to_numeric(fixed_output_times)

    if(is.list(meshes)){
        meshes <- unlist(meshes)
    }

    new_prj_output(type,
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


new_prj_output <- function(type,
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

    are_null_or_strings(suffix,
                               data_mode)


    if(!is.null(timesteps)){
        timesteps <- validate_timesteps(timesteps, TRUE)
    }

    are_null_or_string_flags(compress_output)

    if(!is.null(meshes)){
        assertthat::assert_that(is.character(meshes))
        names(meshes) <- rep("mesh", length(meshes))
    }

    are_null_or_numeric(fixed_output_times)

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
             xpath = "time_loop/output",
             attr_names = character(),
             flatten_on_exp = c("fixed_output_times")
        ),
        class = "prj_output"
    )
}


#===== prj_global_process_coupling =====


#' prj_global_process_coupling
#' @description tag: global_process_coupling
#' @param max_iter string | double: Maximal number of iterations
#' @param convergence_criteria list, prj_convergence_criterion:
#'   Convergence criteria
#' @example man/examples/ex_prj_global_process_coupling.R
#' @export
prj_global_process_coupling <- function(max_iter,
                                           convergence_criteria) {

    #Coerce input
    max_iter <- coerce_string_to_numeric(max_iter)

    new_prj_global_process_coupling(max_iter,
                                       convergence_criteria)
}


new_prj_global_process_coupling <- function(max_iter,
                                               convergence_criteria) {

    assertthat::assert_that(is.double(max_iter))

    is_wrapper_list(convergence_criteria,
                          "prj_convergence_criterion")

    structure(
        list(
            max_iter = max_iter,
            convergence_criteria = convergence_criteria,
            xpath = "time_loop/global_process_coupling",
            attr_names = character(),
            flatten_on_exp = character()
        ),
        class = "prj_global_process_coupling"
    )
}


#===== prj_convergence_criterion =====


#' prj_convergence_criterion
#' @description tag: convergence_criterion
#' @param type string: Type
#' @param norm_type string: ...
#' @param abstol string | double: Absolute tolerance
#' @param reltol string | double: Relative tolerance
#' @param abstols string | numeric: Absolute tolerances
#' @param reltols string | numeric: Relative tolerances
#' @example man/examples/ex_prj_convergence_criterion.R
#' @export
prj_convergence_criterion <- function(type,
                                         norm_type,
                                         abstol = NULL,
                                         reltol = NULL,
                                         abstols = NULL,
                                         reltols = NULL) {

    #Coerce input
    abstol <- coerce_string_to_numeric(abstol)
    reltol <- coerce_string_to_numeric(reltol)
    abstols <- coerce_string_to_numeric(abstols)
    reltols <- coerce_string_to_numeric(reltols)

    new_prj_convergence_criterion(type,
                                     norm_type,
                                     abstol,
                                     reltol,
                                     abstols,
                                     reltols)
}


new_prj_convergence_criterion <- function(type,
                                             norm_type,
                                             abstol = NULL,
                                             reltol = NULL,
                                             abstols = NULL,
                                             reltols = NULL) {

    assertthat::assert_that(assertthat::is.string(type))
    assertthat::assert_that(assertthat::is.string(norm_type))

    are_null_or_numbers(abstol,
                               reltol)
    are_null_or_numeric(abstols,
                                reltols)

    structure(
        list(
            type = type,
            norm_type = norm_type,
            abstol = abstol,
            reltol = reltol,
            abstols = abstols,
            reltols = reltols,
            xpath = c("time_loop/processes/process/convergence_criterion",
                      paste0("time_loop/global_process_coupling/",
                             "convergence_criteria/convergence_criterion")),
            attr_names = character(),
            flatten_on_exp = c("abstols", "reltols")
        ),
        class = "prj_convergence_criterion"
    )
}


#===== prj_time_stepping =====


#' prj_time_stepping
#' @description tag: time_stepping
#' @param type string:
#' @param t_initial Optional: string | double:
#' @param t_end Optional: string | double:
#' @param timesteps Optional: list:
#' @param initial_dt Optional: string | double:
#' @param minimum_dt Optional: string | double:
#' @param maximum_dt Optional: string | double:
#' @param number_iterations Optional: string | numeric:
#' @param multiplier Optional: string | numeric:
#' @param dt_guess Optional: string | double:
#' @param dt_min Optional: string | double:
#' @param dt_max Optional: string | double:
#' @param rel_dt_min Optional: string | double:
#' @param rel_dt_max Optional: string | double:
#' @param tol Optional: string | double:
#' @example man/examples/ex_prj_time_stepping.R
#' @export
prj_time_stepping <- function(type,
                                 t_initial = NULL,
                                 t_end = NULL,
                                 timesteps = NULL,
                                 initial_dt = NULL,
                                 minimum_dt = NULL,
                                 maximum_dt = NULL,
                                 number_iterations = NULL,
                                 multiplier = NULL,
                                 dt_guess = NULL,
                                 dt_min = NULL,
                                 dt_max = NULL,
                                 rel_dt_min = NULL,
                                 rel_dt_max = NULL,
                                 tol = NULL) {

    # Coerce input
    t_initial <- coerce_string_to_numeric(t_initial)
    t_end <- coerce_string_to_numeric(t_end)
    initial_dt <- coerce_string_to_numeric(initial_dt)
    minimum_dt <- coerce_string_to_numeric(minimum_dt)
    maximum_dt <- coerce_string_to_numeric(maximum_dt)
    dt_guess <- coerce_string_to_numeric(dt_guess)
    dt_min <- coerce_string_to_numeric(dt_min)
    dt_max <- coerce_string_to_numeric(dt_max)
    rel_dt_min <- coerce_string_to_numeric(rel_dt_min)
    rel_dt_max <- coerce_string_to_numeric(rel_dt_max)
    tol <- coerce_string_to_numeric(tol)

    number_iterations <- coerce_string_to_numeric(number_iterations)
    multiplier <- coerce_string_to_numeric(multiplier)

    new_prj_time_stepping(type,
                             t_initial,
                             t_end,
                             timesteps,
                             initial_dt,
                             minimum_dt,
                             maximum_dt,
                             number_iterations,
                             multiplier,
                             dt_guess,
                             dt_min,
                             dt_max,
                             rel_dt_min,
                             rel_dt_max,
                             tol)
}


new_prj_time_stepping <- function(type,
                                     t_initial = NULL,
                                     t_end = NULL,
                                     timesteps = NULL,
                                     initial_dt = NULL,
                                     minimum_dt = NULL,
                                     maximum_dt = NULL,
                                     number_iterations = NULL,
                                     multiplier = NULL,
                                     dt_guess = NULL,
                                     dt_min = NULL,
                                     dt_max = NULL,
                                     rel_dt_min = NULL,
                                     rel_dt_max = NULL,
                                     tol = NULL) {

    are_strings(type)

    are_null_or_numbers(t_initial,
                        t_end,
                        initial_dt,
                        minimum_dt,
                        maximum_dt,
                        dt_guess,
                        dt_min,
                        dt_max,
                        rel_dt_min,
                        rel_dt_max,
                        tol)

    are_null_or_numeric(number_iterations,
                                multiplier)

    if(!is.null(timesteps)){
        timesteps <- validate_timesteps(timesteps)
    }


    structure(list(type = type,
                   t_initial = t_initial,
                   t_end = t_end,
                   timesteps = timesteps,
                   initial_dt = initial_dt,
                   minimum_dt = minimum_dt,
                   maximum_dt = maximum_dt,
                   number_iterations = number_iterations,
                   multiplier = multiplier,
                   dt_guess = dt_guess,
                   dt_min = dt_min,
                   dt_max = dt_max,
                   rel_dt_min = rel_dt_min,
                   rel_dt_max = rel_dt_max,
                   tol = tol,
                   xpath = "time_loop/processes/process/time_stepping",
                   attr_names = character(),
                   flatten_on_exp = c("number_iterations",
                                      "multiplier")
    ),
    class = "prj_time_stepping"
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
