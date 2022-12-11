
#===== prj_chemical_system =====


#' prj_chemical_system
#' @description tag: chemical_system
#' @param chemical_solver string:
#' @param database string:
#' @param solution prj_solution:
#' @param mesh Optional: string:
#' @param knobs Optional: list:
#' @param kinetic_reactants Optional: list, prj_kinetic_reactant:
#' @param rates Optional: list, prj_rate:
#' @param equilibrium_reactants Optional: list, prj_phase_component:
#' @param surface Optional:
#' @param user_punch Optional:
#' @param linear_solver Optional:
#' @param exchangers Optional: prj_exchangers
#' @example man/examples/ex_prj_chemical_system.R
#' @export
prj_chemical_system <- function(chemical_solver,
                                   database,
                                   solution,
                                   mesh = NULL,
                                   knobs = NULL,
                                   kinetic_reactants = NULL,
                                   rates = NULL,
                                   equilibrium_reactants = NULL,
                                   surface = NULL,
                                   user_punch = NULL,
                                   linear_solver = NULL,
                                   exchangers = NULL) {

    # Add coercing utility here

    new_prj_chemical_system(chemical_solver,
                               database,
                               solution,
                               mesh,
                               knobs,
                               kinetic_reactants,
                               rates,
                               equilibrium_reactants,
                               surface,
                               user_punch,
                               linear_solver,
                               exchangers)
}


new_prj_chemical_system <- function(chemical_solver,
                                       database,
                                       solution,
                                       mesh = NULL,
                                       knobs = NULL,
                                       kinetic_reactants = NULL,
                                       rates = NULL,
                                       equilibrium_reactants = NULL,
                                       surface = NULL,
                                       user_punch = NULL,
                                       linear_solver = NULL,
                                       exchangers = NULL) {


    are_strings(chemical_solver,
                database)

    assertthat::assert_that(class(solution) == "prj_solution")
    assertthat::assert_that(is.null(exchangers) |
                                class(exchangers) == "prj_exchangers")

    are_null_or_strings(mesh)

    if(!is.null(equilibrium_reactants)){
        is_wrapper_list(equilibrium_reactants,
                        "prj_phase_component")
    }

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

    structure(list(chemical_solver = chemical_solver,
                   database = database,
                   solution = solution,
                   mesh = mesh,
                   knobs = knobs,
                   kinetic_reactants = kinetic_reactants,
                   rates = rates,
                   equilibrium_reactants = equilibrium_reactants,
                   surface = surface,
                   user_punch = user_punch,
                   linear_solver = linear_solver,
                   exchangers = exchangers,
                   xpath = "chemical_system",
                   attr_names = c("chemical_solver"),
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
