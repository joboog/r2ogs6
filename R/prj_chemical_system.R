
#===== r2ogs6_chemical_system =====


#'r2ogs6_chemical_system
#'@description tag: chemical_system
#'@param chemical_solver string:
#'@param database string:
#'@param solution r2ogs6_solution:
#'@param mesh Optional: string:
#'@param equilibrium_reactants Optional: list, r2ogs6_phase_component:
#'@param knobs Optional: list:
#'@param kinetic_reactants Optional: list, r2ogs6_kinetic_reactant:
#'@param rates Optional: list, r2ogs6_rate:
#'@export
r2ogs6_chemical_system <- function(chemical_solver,
                                   database,
                                   solution,
                                   mesh = NULL,
                                   equilibrium_reactants = NULL,
                                   knobs = NULL,
                                   kinetic_reactants = NULL,
                                   rates = NULL) {

    #Coerce input

    new_r2ogs6_chemical_system(
        chemical_solver,
        database,
        solution,
        mesh,
        equilibrium_reactants,
        knobs,
        kinetic_reactants,
        rates
    )
}


new_r2ogs6_chemical_system <- function(chemical_solver,
                                       database,
                                       solution,
                                       mesh = NULL,
                                       equilibrium_reactants = NULL,
                                       knobs = NULL,
                                       kinetic_reactants = NULL,
                                       rates = NULL) {

    assertthat::assert_that(assertthat::is.string(chemical_solver))
    assertthat::assert_that(assertthat::is.string(database))
    assertthat::assert_that(class(solution) == "r2ogs6_solution")

    validate_is_null_or_string(mesh)

    if(!is.null(equilibrium_reactants)){
        validate_wrapper_list(equilibrium_reactants,
                              "r2ogs6_phase_component")
    }

    if(!is.null(knobs)){
        knobs <- validate_param_list(knobs,
                                     c("max_iter",
                                       "relative_convergence_tolerance",
                                       "tolerance",
                                       "step_size",
                                       "scaling"))
    }

    if(!is.null(kinetic_reactants)){
        validate_wrapper_list(kinetic_reactants,
                              "r2ogs6_kinetic_reactant")
    }

    if(!is.null(rates)){
        validate_wrapper_list(rates,
                              "r2ogs6_rate")
    }

    structure(
        list(
            chemical_solver = chemical_solver,
            database = database,
            solution = solution,
            mesh = mesh,
            equilibrium_reactants = equilibrium_reactants,
            knobs = knobs,
            kinetic_reactants = kinetic_reactants,
            rates = rates,
            tag_name = "chemical_system",
            is_subclass = FALSE,
            attr_names = c("chemical_solver"),
            flatten_on_exp = character()
        ),
        class = "r2ogs6_chemical_system"
    )
}


#===== r2ogs6_solution =====


#'r2ogs6_solution
#'@description tag: solution
#'@param temperature string | double: Temperature
#'@param pressure string | double: Pressure
#'@param pe string | double: pe
#'@param components list: Components
#'@param charge_balance Optional: string: Charge balance
#'@export
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


    assertthat::assert_that(is.double(temperature))
    assertthat::assert_that(is.double(pressure))
    assertthat::assert_that(is.double(pe))
    assertthat::assert_that(is.character(components))
    names(components) <- rep("component", length(components))

    validate_is_null_or_string(charge_balance)

    structure(
        list(
            temperature = temperature,
            pressure = pressure,
            pe = pe,
            components = components,
            charge_balance = charge_balance,
            tag_name = "solution",
            is_subclass = TRUE,
            attr_names = character(),
            flatten_on_exp = character()
        ),
        class = "r2ogs6_solution"
    )
}


#===== r2ogs6_phase_component =====


#'r2ogs6_phase_component
#'@description S3 class describing .prj phase_component
#'@param name The component name
#'@param initial_amount The initial amount of the component
#'@param saturation_index The saturation index of the component
#'@export
r2ogs6_phase_component <- function(name,
                                   initial_amount,
                                   saturation_index) {

    #Coerce input
    initial_amount <- coerce_string_to_numeric(initial_amount)
    saturation_index <- coerce_string_to_numeric(saturation_index)

    new_r2ogs6_phase_component(name,
                               initial_amount,
                               saturation_index)
}


new_r2ogs6_phase_component <- function(name,
                                       initial_amount,
                                       saturation_index) {

    assertthat::assert_that(assertthat::is.string(name))
    assertthat::assert_that(is.double(initial_amount))
    assertthat::assert_that(is.double(saturation_index))

    structure(
        list(
            name = name,
            initial_amount = initial_amount,
            saturation_index = saturation_index,
            tag_name = "phase_component",
            is_subclass = TRUE,
            attr_names = character(),
            flatten_on_exp = character()
        ),
        class = "r2ogs6_phase_component"
    )
}


#===== r2ogs6_kinetic_reactant =====


#'r2ogs6_kinetic_reactant
#'@description S3 class describing .prj kinetic_reactant
#'@param name The component name
#'@param initial_amount The initial amount of the component
#'@param chemical_formula The chemical formula of the component
#'@param fix_amount Should the amount be fixed or not?
#'@export
r2ogs6_kinetic_reactant <- function(name,
                                   initial_amount,
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
                                       initial_amount,
                                       chemical_formula = NULL,
                                       fix_amount = NULL) {

    assertthat::assert_that(assertthat::is.string(name))
    assertthat::assert_that(is.double(initial_amount))

    validate_is_null_or_string(chemical_formula,
                               fix_amount)

    structure(
        list(
            name = name,
            initial_amount = initial_amount,
            chemical_formula = chemical_formula,
            fix_amount = fix_amount,
            tag_name = "kinetic_reactant",
            is_subclass = TRUE,
            attr_names = character(),
            flatten_on_exp = character()
        ),
        class = "r2ogs6_kinetic_reactant"
    )
}


#===== r2ogs6_rate =====


#'r2ogs6_rate
#'@description S3 class describing .prj rate
#'@param kinetic_reactant string: References a kinetic_reactant object
#'@param expression character: Statements
#'@export
r2ogs6_rate <- function(kinetic_reactant,
                        expression) {

    #Coerce input

    new_r2ogs6_rate(kinetic_reactant, expression)
}


new_r2ogs6_rate <- function(kinetic_reactant,
                            expression) {

    assertthat::assert_that(assertthat::is.string(kinetic_reactant))
    assertthat::assert_that(is.character(expression))
    names(expression) <- rep("statement", length(expression))

    structure(
        list(
            kinetic_reactant = kinetic_reactant,
            expression = expression,
            tag_name = "rate",
            is_subclass = TRUE,
            attr_names = character(),
            flatten_on_exp = character()
        ),
        class = "r2ogs6_rate"
    )
}

