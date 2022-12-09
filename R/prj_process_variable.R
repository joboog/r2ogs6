
#===== prj_process_variable =====


#' prj_process_variable
#' @description tag: process_variable
#' @param name string: The name of the process variable
#' @param components string | double:
#' @param order string | double:
#' @param initial_condition string:
#' @param boundary_conditions list, prj_boundary_condition:
#' @param source_terms Optional: list, prj_source_term:
#' @param mesh Optional: string: list:
#' @param deactivated_subdomains Optional: list, prj_deactivated_subdomain:
#' @param compensate_non_equilibrium_initial_residuum Optional: string
#' @example man/examples/ex_prj_process_variable.R
#' @export
prj_process_variable <- function(name,
                            components,
                            order,
                            initial_condition,
                            boundary_conditions = NULL,
                            source_terms = NULL,
                            mesh = NULL,
                            deactivated_subdomains = NULL,
                            compensate_non_equilibrium_initial_residuum = NULL){

    #Coerce input
    components <- coerce_string_to_numeric(components)
    order <- coerce_string_to_numeric(order)

    new_prj_process_variable(name,
                                components,
                                order,
                                initial_condition,
                                boundary_conditions,
                                source_terms,
                                mesh,
                                deactivated_subdomains,
                                compensate_non_equilibrium_initial_residuum)
}


new_prj_process_variable <- function(name,
                            components,
                            order,
                            initial_condition,
                            boundary_conditions = NULL,
                            source_terms = NULL,
                            mesh = NULL,
                            deactivated_subdomains = NULL,
                            compensate_non_equilibrium_initial_residuum = NULL){

    assertthat::assert_that(assertthat::is.string(name))
    assertthat::assert_that(assertthat::is.number(components))
    assertthat::assert_that(assertthat::is.number(order))
    assertthat::assert_that(assertthat::is.string(initial_condition))

    if(is.character(boundary_conditions)){
        boundary_conditions <- NULL
    }

    if(!is.null(boundary_conditions)){
        is_wrapper_list(boundary_conditions,
                              "prj_boundary_condition")
    }

    if(!is.null(source_terms)){
        is_wrapper_list(source_terms,
                              "prj_source_term")
    }

    are_null_or_strings(mesh, compensate_non_equilibrium_initial_residuum)

    if(!is.null(deactivated_subdomains)){
        is_wrapper_list(deactivated_subdomains,
                              "prj_deactivated_subdomain")
    }

    structure(list(name = name,
                   components = components,
                   order = order,
                   initial_condition = initial_condition,
                   boundary_conditions = boundary_conditions,
                   source_terms = source_terms,
                   mesh = mesh,
                   deactivated_subdomains = deactivated_subdomains,
                    compensate_non_equilibrium_initial_residuum =
                       compensate_non_equilibrium_initial_residuum,
                   xpath = "process_variables/process_variable",
                   attr_names = character(),
                   flatten_on_exp = character()
                   ),
              class = "prj_process_variable"
    )
}


#===== prj_boundary_condition =====


#' prj_boundary_condition
#' @description tag: boundary_condition
#' @param type string:
#' @param parameter string:
#' @param geometrical_set Optional: string:
#' @param geometry Optional: string:
#' @param component Optional: string | double:
#' @param mesh Optional: string:
#' @param alpha Optional: string:
#' @param u_0 Optional: string:
#' @param constraint_type Optional: string:
#' @param constraining_process_variable Optional: string:
#' @param constraint_threshold Optional: string | double:
#' @param constraint_direction Optional: string:
#' @param area_parameter Optional: string:
#' @param bc_object Optional: string:
#' @param flush_stdout Optional: string:
#' @param property_name Optional: string:
#' @param initial_value_parameter Optional: string:
#' @param constant_name Optional: string:
#' @param coefficient_current_variable_name Optional: string:
#' @param coefficient_other_variable_name Optional: string:
#' @param coefficient_mixed_variables_name Optional: string:
#' @param threshold_parameter Optional: string:
#' @param comparison_operator Optional: string:
#' @param time_interval Optional: list of 2, character:
#' @example man/examples/ex_prj_boundary_condition.R
#' @export
prj_boundary_condition <- function(type,
                                      parameter = NULL,
                                      geometrical_set = NULL,
                                      geometry = NULL,
                                      component = NULL,
                                      mesh = NULL,
                                      alpha = NULL,
                                      u_0 = NULL,
                                      constraint_type = NULL,
                                      constraining_process_variable = NULL,
                                      constraint_threshold = NULL,
                                      constraint_direction = NULL,
                                      area_parameter = NULL,
                                      bc_object = NULL,
                                      flush_stdout = NULL,
                                      property_name = NULL,
                                      initial_value_parameter = NULL,
                                      constant_name = NULL,
                                      coefficient_current_variable_name = NULL,
                                      coefficient_other_variable_name = NULL,
                                      coefficient_mixed_variables_name = NULL,
                                      threshold_parameter = NULL,
                                      comparison_operator = NULL,
                                      time_interval = NULL){

    #Coerce input
    component <- coerce_string_to_numeric(component)
    constraint_threshold <- coerce_string_to_numeric(constraint_threshold)

    new_prj_boundary_condition(type,
                                  parameter,
                                  geometrical_set,
                                  geometry,
                                  component,
                                  mesh,
                                  alpha,
                                  u_0,
                                  constraint_type,
                                  constraining_process_variable,
                                  constraint_threshold,
                                  constraint_direction,
                                  area_parameter,
                                  bc_object,
                                  flush_stdout,
                                  property_name,
                                  initial_value_parameter,
                                  constant_name,
                                  coefficient_current_variable_name,
                                  coefficient_other_variable_name,
                                  coefficient_mixed_variables_name,
                                  threshold_parameter,
                                  comparison_operator,
                                  time_interval)
}


new_prj_boundary_condition <- function(type,
                                          parameter = NULL,
                                          geometrical_set = NULL,
                                          geometry = NULL,
                                          component = NULL,
                                          mesh = NULL,
                                          alpha = NULL,
                                          u_0 = NULL,
                                          constraint_type = NULL,
                                          constraining_process_variable = NULL,
                                          constraint_threshold = NULL,
                                          constraint_direction = NULL,
                                          area_parameter = NULL,
                                          bc_object = NULL,
                                          flush_stdout = NULL,
                                          property_name = NULL,
                                          initial_value_parameter = NULL,
                                          constant_name = NULL,
                                          coefficient_current_variable_name =
                                              NULL,
                                          coefficient_other_variable_name =
                                              NULL,
                                          coefficient_mixed_variables_name =
                                              NULL,
                                          threshold_parameter = NULL,
                                          comparison_operator = NULL,
                                          time_interval = NULL){

    assertthat::assert_that(assertthat::is.string(type))

    are_null_or_numbers(component,
                               constraint_threshold)

    are_null_or_strings(parameter,
                               geometrical_set,
                               geometry,
                               mesh,
                               alpha,
                               u_0,
                               constraint_type,
                               constraining_process_variable,
                               constraint_direction,
                               area_parameter,
                               bc_object,
                               property_name,
                               initial_value_parameter,
                               constant_name,
                               coefficient_current_variable_name,
                               coefficient_other_variable_name,
                               coefficient_mixed_variables_name,
                               threshold_parameter,
                               comparison_operator)

    are_null_or_string_flags(flush_stdout)

    if(!is.null(time_interval)){
        time_interval <- coerce_names(time_interval, c("start", "end"))
    }

    structure(list(type = type,
                   parameter = parameter,
                   geometrical_set = geometrical_set,
                   geometry = geometry,
                   component = component,
                   mesh = mesh,
                   alpha = alpha,
                   u_0 = u_0,
                   constraint_type = constraint_type,
                   constraining_process_variable =
                       constraining_process_variable,
                   constraint_threshold = constraint_threshold,
                   constraint_direction = constraint_direction,
                   area_parameter = area_parameter,
                   bc_object = bc_object,
                   flush_stdout = flush_stdout,
                   property_name = property_name,
                   initial_value_parameter = initial_value_parameter,
                   constant_name = constant_name,
                   coefficient_current_variable_name =
                       coefficient_current_variable_name,
                   coefficient_other_variable_name =
                       coefficient_other_variable_name,
                   coefficient_mixed_variables_name =
                       coefficient_mixed_variables_name,
                   threshold_parameter = threshold_parameter,
                   comparison_operator = comparison_operator,
                   time_interval = time_interval,
                   xpath = paste0("process_variables/process_variable/",
                                  "boundary_conditions/boundary_condition"),
                   attr_names = character(),
                   flatten_on_exp = character()
                   ),
              class = "prj_boundary_condition"
    )
}


#===== prj_source_term =====


#' prj_source_term
#' @description tag: source_term
#' @param type string:
#' @param parameter Optional: string:
#' @param geometrical_set Optional: string:
#' @param geometry Optional: string:
#' @param mesh Optional: string:
#' @param source_term_object Optional: string:
#' @param flush_stdout Optional: string:
#' @example man/examples/ex_prj_source_term.R
#' @export
prj_source_term <- function(type,
                               parameter = NULL,
                               geometrical_set = NULL,
                               geometry = NULL,
                               mesh = NULL,
                               source_term_object = NULL,
                               flush_stdout = NULL){

    #Coerce input

    new_prj_source_term(type,
                           parameter,
                           geometrical_set,
                           geometry,
                           mesh,
                           source_term_object,
                           flush_stdout)
}


new_prj_source_term <- function(type,
                                   parameter = NULL,
                                   geometrical_set = NULL,
                                   geometry = NULL,
                                   mesh = NULL,
                                   source_term_object = NULL,
                                   flush_stdout = NULL){

    assertthat::assert_that(assertthat::is.string(type))

    are_null_or_strings(parameter,
                        geometrical_set,
                        geometry,
                        mesh,
                        source_term_object,
                        flush_stdout)

    structure(list(type = type,
                   parameter = parameter,
                   geometrical_set = geometrical_set,
                   geometry = geometry,
                   mesh = mesh,
                   source_term_object = source_term_object,
                   flush_stdout = flush_stdout,
                   xpath = paste0("process_variables/process_variable/",
                                  "source_terms/source_term"),
                   attr_names = character(),
                   flatten_on_exp = character()),
    class = "prj_source_term"
    )
}


#===== prj_deactivated_subdomain =====


#' prj_deactivated_subdomain
#' @description tag: deactivated_subdomain
#' @param material_ids string | double:
#' @param time_interval Optional: list, numeric:
#' @param time_curve Optional:
#' @param line_segment Optional:
#' @example man/examples/ex_prj_deactivated_subdomain.R
#' @export
prj_deactivated_subdomain <- function(material_ids,
                                      time_interval = NULL,
                                      time_curve = NULL,
                                      line_segment = NULL) {

    # Coerce input
    material_ids <- coerce_string_to_numeric(material_ids)

    new_prj_deactivated_subdomain(material_ids,
                                  time_interval,
                                  time_curve,
                                  line_segment)
}


new_prj_deactivated_subdomain <- function(material_ids,
                                          time_interval = NULL,
                                          time_curve = NULL,
                                          line_segment = NULL) {

    assertthat::assert_that(assertthat::is.number(material_ids))

    time_interval <- is_null_or_coerce_names(time_interval, c("start", "end"))

    are_null_or_strings(time_curve)

    line_segment <- is_null_or_coerce_names(line_segment, c("start", "end"))


    structure(list(material_ids = material_ids,
                   time_interval = time_interval,
                   time_curve = time_curve,
                   line_segment = line_segment,
                   xpath =
                       paste0("process_variables/process_variable/",
                              "deactivated_subdomains/deactivated_subdomain"),
                   attr_names = c(),
                   flatten_on_exp = character()
    ),
    class = "prj_deactivated_subdomain"
    )
}
