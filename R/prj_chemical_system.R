
#===== prj_chemical_system =====


#' prj_chemical_system
#' @description tag: chemical_system
#' @param chemical_solver string: Either "Phreeqc" or "SelfContained".
#' @param database string: Required if chemical_solver == "Phreeqc" .
#' @param solution prj_solution: Required if chemical_solver == "Phreeqc".
#' @param mesh string:
#' @param knobs Optional: list:
#' @param kinetic_reactants Optional: list, prj_kinetic_reactant:
#' @param rates Optional: list, prj_rate:
#' @param equilibrium_reactants Optional: list, prj_phase_component:
#' @param surface Optional:
#' @param user_punch Optional:
#' @param use_high_precision Optional:
#' @param linear_solver Optional: string:
#' @param exchangers Optional: prj_exchangers
#' @param chemical_reactions list: Required if
#' chemical_solver == "SelfContained".
#' @param number_of_components integer: Required if
#' chemical_solver == "SelfContained".
#' @example man/examples/ex_prj_chemical_system.R
#' @export
prj_chemical_system <- function(chemical_solver,
                               mesh,
                               linear_solver = NULL,
                               database = NULL,
                               solution = NULL,
                               knobs = NULL,
                               kinetic_reactants = NULL,
                               rates = NULL,
                               equilibrium_reactants = NULL,
                               surface = NULL,
                               user_punch = NULL,
                               use_high_precision = NULL,
                               exchangers = NULL,
                               chemical_reactions = NULL,
                               number_of_components = NULL) {

    # Add coercing utility here
    number_of_components <- coerce_string_to_numeric(number_of_components)

    new_prj_chemical_system(chemical_solver,
                            mesh,
                            linear_solver,
                            database,
                            solution,
                            knobs,
                            kinetic_reactants,
                            rates,
                            equilibrium_reactants,
                            surface,
                            user_punch,
                            use_high_precision,
                            exchangers,
                            chemical_reactions,
                            number_of_components)
}


new_prj_chemical_system <- function(chemical_solver,
                                    mesh,
                                    linear_solver = NULL,
                                    database = NULL,
                                    solution = NULL,
                                    knobs = NULL,
                                    kinetic_reactants = NULL,
                                    rates = NULL,
                                    equilibrium_reactants = NULL,
                                    surface = NULL,
                                    user_punch = NULL,
                                    use_high_precision = NULL,
                                    exchangers = NULL,
                                    chemical_reactions = NULL,
                                    number_of_components = NULL) {


    are_strings(chemical_solver, mesh)

    are_null_or_strings(linear_solver, database)

    assertthat::assert_that(is.null(solution) |
                                class(solution) == "prj_solution")

    knobs <- is_null_or_coerce_names(
        knobs,
        c("max_iter",
          "relative_convergence_tolerance",
          "tolerance",
          "step_size",
          "scaling")
    )

    if(!is.null(kinetic_reactants)){
        is_wrapper_list(kinetic_reactants,
                        "prj_kinetic_reactant")
    }

    if(!is.null(rates)){
        is_wrapper_list(rates,
                        "prj_rate")
    }

    if(!is.null(equilibrium_reactants)){
        is_wrapper_list(equilibrium_reactants,
                        "prj_phase_component")
    }

    assertthat::assert_that(is.null(surface) |
                                class(surface) == "prj_surface")

    assertthat::assert_that(is.null(user_punch) |
                                is.list(user_punch))

    are_null_or_string_flags(use_high_precision)

    assertthat::assert_that(is.null(exchangers) |
                                class(exchangers) == "prj_exchangers")

    if(!is.null(chemical_reactions)){
        is_wrapper_list(chemical_reactions,
                        "prj_chemical_reaction")
    }

    assertthat::assert_that(is.null(number_of_components) |
                                is.numeric(number_of_components))

    structure(list(chemical_solver = chemical_solver,
                   mesh = mesh,
                   linear_solver = linear_solver,
                   database = database,
                   solution = solution,
                   knobs = knobs,
                   kinetic_reactants = kinetic_reactants,
                   rates = rates,
                   equilibrium_reactants = equilibrium_reactants,
                   surface = surface,
                   user_punch = user_punch,
                   use_high_precision = use_high_precision,
                   exchangers = exchangers,
                   chemical_reactions = chemical_reactions,
                   number_of_components = number_of_components,
                   xpath = "chemical_system",
                   attr_names = c("chemical_solver", "site_unit"),
                   flatten_on_exp = character()
    ),
    class = "prj_chemical_system"
    )
}


#===== prj_solution =====


#' prj_solution
#' @description tag: solution
#' @param temperature string | double: Temperature
#' @param pressure string | double: Pressure
#' @param pe string | double: pe
#' @param components list: Components
#' @param charge_balance Optional: string: Charge balance
#' @param fixing_pe Optional: string
#' @example man/examples/ex_prj_solution.R
#' @export
prj_solution <- function(temperature,
                         pressure,
                         pe,
                         components,
                         charge_balance = NULL,
                         fixing_pe = NULL) {

    #Coerce input
    temperature <- coerce_string_to_numeric(temperature)
    pressure <- coerce_string_to_numeric(pressure)
    pe <- coerce_string_to_numeric(pe)


    new_prj_solution(temperature,
                    pressure,
                    pe,
                    components,
                    charge_balance,
                    fixing_pe)
}


new_prj_solution <- function(temperature,
                            pressure,
                            pe,
                            components,
                            charge_balance = NULL,
                            fixing_pe = NULL) {
    are_numbers(temperature,
                pressure,
                pe)

    assertthat::assert_that(is.list(components))
    names(components) <- rep("component", length(components))

    are_null_or_strings(charge_balance)
    are_null_or_string_flags(fixing_pe)

    structure(
        list(
            temperature = temperature,
            pressure = pressure,
            pe = pe,
            components = components,
            charge_balance = charge_balance,
            fixing_pe = fixing_pe,
            xpath = "chemical_system/solution",
            attr_names = c("fixing_pe"),
            flatten_on_exp = character()
        ),
        class = "prj_solution"
    )
}


#===== prj_phase_component =====


#' prj_phase_component
#' @description S3 class describing .prj phase_component
#' @param name The component name
#' @param saturation_index The saturation index of the component
#' @param initial_amount optional: The initial amount of the component
#' @example man/examples/ex_prj_phase_component.R
#' @export
prj_phase_component <- function(name,
                                   saturation_index,
                                   initial_amount = NULL) {

    #Coerce input
    saturation_index <- coerce_string_to_numeric(saturation_index)
    initial_amount <- coerce_string_to_numeric(initial_amount)

    new_prj_phase_component(name,
                               saturation_index,
                               initial_amount)
}


new_prj_phase_component <- function(name,
                                       saturation_index,
                                       initial_amount) {

    assertthat::assert_that(assertthat::is.string(name))

    are_numbers(saturation_index)
    are_null_or_numeric(initial_amount)

    structure(
        list(
            name = name,
            saturation_index = saturation_index,
            initial_amount = initial_amount,
            xpath = "chemical_system/equilibrium_reactants/phase_component",
            attr_names = character(),
            flatten_on_exp = character()
        ),
        class = "prj_phase_component"
    )
}


#===== prj_kinetic_reactant =====


#' prj_kinetic_reactant
#' @description S3 class describing .prj kinetic_reactant
#' @param name The component name
#' @param initial_amount The initial amount of the component
#' @param chemical_formula The chemical formula of the component
#' @param fix_amount Should the amount be fixed or not?
#' @example man/examples/ex_prj_kinetic_reactant.R
#' @export
prj_kinetic_reactant <- function(name,
                                    initial_amount = NULL,
                                    chemical_formula = NULL,
                                    fix_amount = NULL) {

    #Coerce input
    initial_amount <- coerce_string_to_numeric(initial_amount)

    new_prj_kinetic_reactant(name,
                                initial_amount,
                                chemical_formula,
                                fix_amount)
}


new_prj_kinetic_reactant <- function(name,
                                       initial_amount = NULL,
                                       chemical_formula = NULL,
                                       fix_amount = NULL) {

    assertthat::assert_that(assertthat::is.string(name))
    are_null_or_numbers(initial_amount)
    are_null_or_strings(chemical_formula,
                               fix_amount)

    structure(
        list(
            name = name,
            initial_amount = initial_amount,
            chemical_formula = chemical_formula,
            fix_amount = fix_amount,
            xpath = "chemical_system/kinetic_reactants/kinetic_reactant",
            attr_names = character(),
            flatten_on_exp = character()
        ),
        class = "prj_kinetic_reactant"
    )
}


#===== prj_rate =====

#' prj_rate
#' @description S3 class describing .prj rate
#' @param kinetic_reactant string: References a kinetic_reactant object
#' @param expression character: Statements
#' @example man/examples/ex_prj_rate.R
#' @export
prj_rate <- function(kinetic_reactant,
                        expression) {

    #Coerce input

    new_prj_rate(kinetic_reactant, expression)
}


new_prj_rate <- function(kinetic_reactant,
                            expression) {

    assertthat::assert_that(assertthat::is.string(kinetic_reactant))
    assertthat::assert_that(is.list(expression))
    names(expression) <- rep("statement", length(expression))

    structure(
        list(
            kinetic_reactant = kinetic_reactant,
            expression = expression,
            xpath = "chemical_system/rates/rate",
            attr_names = character(),
            flatten_on_exp = character()
        ),
        class = "prj_rate"
    )
}


#=== prj_exchangers ====

#'prj_exchangers
#'@description tag: exchangers
#'@param exchange_site list
#'@export
prj_exchangers <- function(exchange_site) {

    # Add coercing utility here

    new_prj_exchangers(exchange_site)
}

new_prj_exchangers <- function(exchange_site) {

    assertthat::assert_that(is.list(exchange_site))

    structure(list(exchange_site = exchange_site,
                   xpath = "chemical_system/exchangers",
                   attr_names = character(),
                   flatten_on_exp = character()
    ),
    class = "prj_exchangers"
    )
}


#=== prj_surface ====

#'prj_surface
#'@description tag: surface
#'@param site_unit Optional: string
#'@param ... site
#'@export
prj_surface <- function(site_unit = NULL,
                        ...) {

    ellipsis_list <- list(...)
    site <- ellipsis_list[names(ellipsis_list) == "site"]

    new_prj_surface(site_unit,
                    site)
}

new_prj_surface <- function(site_unit,
                            site) {

    are_null_or_strings(site_unit)
    assertthat::assert_that(is.list(site))
    for (each in site){
        assertthat::assert_that(is.list(each))
    }

    structure(list(site_unit = site_unit,
                   site = site,
                   xpath = "chemical_system/surface",
                   attr_names = c("site_unit"),
                   unwrap_on_exp = c("site"),
                   flatten_on_exp = character()
    ),
    class = "prj_surface"
    )
}


#=== prj_chemical_reaction =============================================

#'prj_chemical_reaction
#'@description tag: chemical_reaction
#'@param stoichiometric_coefficients numeric:
#'@param reaction_type string:
#'@param first_order_rate_constant double:
#'@export
prj_chemical_reaction <- function(stoichiometric_coefficients,
                                  reaction_type,
                                  first_order_rate_constant) {

    # Add coercing utility here
    stoichiometric_coefficients <-
        coerce_string_to_numeric(stoichiometric_coefficients)
    first_order_rate_constant <-
        coerce_string_to_numeric(first_order_rate_constant)

    new_prj_chemical_reaction(stoichiometric_coefficients,
                              reaction_type,
                              first_order_rate_constant)
}

new_prj_chemical_reaction <- function(stoichiometric_coefficients,
                                      reaction_type,
                                      first_order_rate_constant) {

    are_numeric(stoichiometric_coefficients)
    are_strings(reaction_type)
    assertthat::assert_that(is.double(first_order_rate_constant))

    structure(list(stoichiometric_coefficients = stoichiometric_coefficients,
                   reaction_type = reaction_type,
                   first_order_rate_constant = first_order_rate_constant,
                   xpath = paste0("chemical_system/chemical_reactions/",
                                  "chemical_reaction"),
                   attr_names = character(),
                   flatten_on_exp = c("stoichiometric_coefficients")
    ),
    class = "prj_chemical_reaction"
    )
}
