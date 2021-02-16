
#===== r2ogs6_chemical_system =====


#' r2ogs6_chemical_system
#' @description tag: chemical_system
#' @param chemical_solver string:
#' @param database string:
#' @param solution r2ogs6_solution:
#' @param mesh Optional: string:
#' @param knobs Optional: list:
#' @param kinetic_reactants Optional: list, r2ogs6_kinetic_reactant:
#' @param rates Optional: list, r2ogs6_rate:
#' @param equilibrium_reactants Optional: list, r2ogs6_phase_component:
#' @param surface Optional:
#' @param user_punch Optional:
#' @example man/examples/ex_prj_chemical_system.R
#' @export
r2ogs6_chemical_system <- function(chemical_solver,
                                   database,
                                   solution,
                                   mesh = NULL,
                                   knobs = NULL,
                                   kinetic_reactants = NULL,
                                   rates = NULL,
                                   equilibrium_reactants = NULL,
                                   surface = NULL,
                                   user_punch = NULL) {

    # Add coercing utility here

    new_r2ogs6_chemical_system(chemical_solver,
                               database,
                               solution,
                               mesh,
                               knobs,
                               kinetic_reactants,
                               rates,
                               equilibrium_reactants,
                               surface,
                               user_punch)
}


new_r2ogs6_chemical_system <- function(chemical_solver,
                                       database,
                                       solution,
                                       mesh = NULL,
                                       knobs = NULL,
                                       kinetic_reactants = NULL,
                                       rates = NULL,
                                       equilibrium_reactants = NULL,
                                       surface = NULL,
                                       user_punch = NULL) {


    are_strings(chemical_solver,
                database)

    assertthat::assert_that(class(solution) == "r2ogs6_solution")

    are_null_or_strings(mesh)

    if(!is.null(equilibrium_reactants)){
        is_wrapper_list(equilibrium_reactants,
                        "r2ogs6_phase_component")
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
                        "r2ogs6_kinetic_reactant")
    }

    if(!is.null(rates)){
        is_wrapper_list(rates,
                        "r2ogs6_rate")
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
                   xpath = "chemical_system",
                   attr_names = c("chemical_solver"),
                   flatten_on_exp = character()
    ),
    class = "r2ogs6_chemical_system"
    )
}


#===== r2ogs6_solution =====


#' r2ogs6_solution
#' @description tag: solution
#' @param temperature string | double: Temperature
#' @param pressure string | double: Pressure
#' @param pe string | double: pe
#' @param components list: Components
#' @param charge_balance Optional: string: Charge balance
#' @example man/examples/ex_prj_solution.R
#' @export
r2ogs6_solution <- function(temperature,
                            pressure,
                            pe,
                            components,
                            charge_balance = NULL) {

    #Coerce input
    temperature <- coerce_string_to_numeric(temperature)
    pressure <- coerce_string_to_numeric(pressure)
    pe <- coerce_string_to_numeric(pe)


    new_r2ogs6_solution(temperature,
                        pressure,
                        pe,
                        components,
                        charge_balance)
}


new_r2ogs6_solution <- function(temperature,
                                pressure,
                                pe,
                                components,
                                charge_balance = NULL) {

    are_numbers(temperature,
                pressure,
                pe)

    assertthat::assert_that(is.list(components))
    names(components) <- rep("component", length(components))

    are_null_or_strings(charge_balance)

    structure(
        list(
            temperature = temperature,
            pressure = pressure,
            pe = pe,
            components = components,
            charge_balance = charge_balance,
            xpath = "chemical_system/solution",
            attr_names = character(),
            flatten_on_exp = character()
        ),
        class = "r2ogs6_solution"
    )
}


#===== r2ogs6_phase_component =====


#' r2ogs6_phase_component
#' @description S3 class describing .prj phase_component
#' @param name The component name
#' @param saturation_index The saturation index of the component
#' @param initial_amount optional: The initial amount of the component
#' @example man/examples/ex_prj_phase_component.R
#' @export
r2ogs6_phase_component <- function(name,
                                   saturation_index,
                                   initial_amount = NULL) {

    #Coerce input
    saturation_index <- coerce_string_to_numeric(saturation_index)
    initial_amount <- coerce_string_to_numeric(initial_amount)

    new_r2ogs6_phase_component(name,
                               saturation_index,
                               initial_amount)
}


new_r2ogs6_phase_component <- function(name,
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
        class = "r2ogs6_phase_component"
    )
}


#===== r2ogs6_kinetic_reactant =====


#' r2ogs6_kinetic_reactant
#' @description S3 class describing .prj kinetic_reactant
#' @param name The component name
#' @param initial_amount The initial amount of the component
#' @param chemical_formula The chemical formula of the component
#' @param fix_amount Should the amount be fixed or not?
#' @example man/examples/ex_prj_kinetic_reactant.R
#' @export
r2ogs6_kinetic_reactant <- function(name,
                                    initial_amount = NULL,
                                    chemical_formula = NULL,
                                    fix_amount = NULL) {

    #Coerce input
    initial_amount <- coerce_string_to_numeric(initial_amount)

    new_r2ogs6_kinetic_reactant(name,
                                initial_amount,
                                chemical_formula,
                                fix_amount)
}


new_r2ogs6_kinetic_reactant <- function(name,
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
        class = "r2ogs6_kinetic_reactant"
    )
}


#===== r2ogs6_rate =====


#' r2ogs6_rate
#' @description S3 class describing .prj rate
#' @param kinetic_reactant string: References a kinetic_reactant object
#' @param expression character: Statements
#' @example man/examples/ex_prj_rate.R
#' @export
r2ogs6_rate <- function(kinetic_reactant,
                        expression) {

    #Coerce input

    new_r2ogs6_rate(kinetic_reactant, expression)
}


new_r2ogs6_rate <- function(kinetic_reactant,
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
        class = "r2ogs6_rate"
    )
}
