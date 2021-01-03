
#===== r2ogs6_medium =====


#'r2ogs6_medium
#'@description tag: medium, a specific medium with optional id corresponding
#' to the MaterialIDs
#'@param phases list, r2ogs6_phase: Optional: Medium phases
#'@param properties list, r2ogs6_pr_property: Optional: Medium properties
#'@param id string | double: Optional: ID corresponding to the MaterialIDs
#'@export
r2ogs6_medium <- function(phases = NULL,
                          properties = NULL,
                          id = NULL) {

    new_r2ogs6_medium(phases,
                      properties,
                      id)
}


new_r2ogs6_medium <- function(phases = NULL,
                              properties = NULL,
                              id = NULL) {

    if(length(phases) != 0){
        validate_wrapper_list(phases, "r2ogs6_phase")
    }

    if(!is.null(properties)){
        validate_wrapper_list(properties, "r2ogs6_pr_property")
    }

    validate_is_null_or_string(id)

    structure(
        list(
            phases = phases,
            properties = properties,
            id = id,
            is_subclass = FALSE,
            attr_names = c("id"),
            flatten_on_exp = character()
        ),
        class = "r2ogs6_medium"
    )
}


#===== r2ogs6_pr_property =====


#'r2ogs6_pr_property
#'@description tag: property, a constitutive property
#'@param name string: Property name
#'@param type string: Property type
#'@param value Optional: string | double: ...
#'@param parameter_name Optional: string:
#'@param exponent Optional: string | double:
#'@param residual_liquid_saturation Optional: string | double:
#'@param residual_gas_saturation Optional: string | double:
#'@param p_b Optional: string | double: string | double:
#'@param independent_variable Optional: string:
#'@param curve Optional: string:
#'@param minimum_relative_permeability_liquid Optional:
#'@param lambda Optional: string | double:
#'@param cutoff_value Optional: string | double:
#'@param initial_permeability Optional: string:
#'@param reference_permeability Optional: string | double:
#'@param fitting_factor Optional: string | double:
#'@param cohesion Optional: string | double:
#'@param friction_angle Optional: string | double:
#'@param maximum_permeability Optional: string | double:
#'@param tensile_strength_parameter Optional: string | double:
#'@param entry_pressure Optional: string | double:
#'@param min_relative_permeability_liquid Optional: string | double:
#'@param min_relative_permeability_gas Optional: string | double:
#'@export
r2ogs6_pr_property <- function(name,
                               type,
                               value = NULL,
                               parameter_name = NULL,
                               exponent = NULL,
                               residual_liquid_saturation = NULL,
                               residual_gas_saturation = NULL,
                               p_b = NULL,
                               independent_variable = NULL,
                               curve = NULL,
                               minimum_relative_permeability_liquid = NULL,
                               lambda = NULL,
                               cutoff_value = NULL,
                               initial_permeability = NULL,
                               reference_permeability = NULL,
                               fitting_factor = NULL,
                               cohesion = NULL,
                               friction_angle = NULL,
                               maximum_permeability = NULL,
                               tensile_strength_parameter = NULL,
                               entry_pressure = NULL,
                               min_relative_permeability_liquid = NULL,
                               min_relative_permeability_gas = NULL) {

    #Coerce input
    value <- coerce_string_to_numeric(value)
    exponent <- coerce_string_to_numeric(exponent)
    residual_liquid_saturation <-
        coerce_string_to_numeric(residual_liquid_saturation)
    residual_gas_saturation <- coerce_string_to_numeric(residual_gas_saturation)
    p_b <- coerce_string_to_numeric(p_b)
    minimum_relative_permeability_liquid <-
        coerce_string_to_numeric(minimum_relative_permeability_liquid)
    lambda <- coerce_string_to_numeric(lambda)
    cutoff_value <- coerce_string_to_numeric(cutoff_value)
    reference_permeability <- coerce_string_to_numeric(reference_permeability)
    fitting_factor <- coerce_string_to_numeric(fitting_factor)
    cohesion <- coerce_string_to_numeric(cohesion)
    friction_angle <- coerce_string_to_numeric(friction_angle)
    maximum_permeability <- coerce_string_to_numeric(maximum_permeability)
    tensile_strength_parameter <-
        coerce_string_to_numeric(tensile_strength_parameter)
    entry_pressure <- coerce_string_to_numeric(entry_pressure)
    min_relative_permeability_liquid <-
        coerce_string_to_numeric(min_relative_permeability_liquid)
    min_relative_permeability_gas <-
        coerce_string_to_numeric(min_relative_permeability_gas)

    new_r2ogs6_pr_property(
        name,
        type,
        value,
        parameter_name,
        exponent,
        residual_liquid_saturation,
        residual_gas_saturation,
        p_b,
        independent_variable,
        curve,
        minimum_relative_permeability_liquid,
        lambda,
        cutoff_value,
        initial_permeability,
        reference_permeability,
        fitting_factor,
        cohesion,
        friction_angle,
        maximum_permeability,
        tensile_strength_parameter,
        entry_pressure,
        min_relative_permeability_liquid,
        min_relative_permeability_gas
    )
}


new_r2ogs6_pr_property <- function(name,
                                   type,
                                   value = NULL,
                                   parameter_name = NULL,
                                   exponent = NULL,
                                   residual_liquid_saturation = NULL,
                                   residual_gas_saturation = NULL,
                                   p_b = NULL,
                                   independent_variable = NULL,
                                   curve = NULL,
                                   minimum_relative_permeability_liquid =
                                       NULL,
                                   lambda = NULL,
                                   cutoff_value = NULL,
                                   initial_permeability = NULL,
                                   reference_permeability = NULL,
                                   fitting_factor = NULL,
                                   cohesion = NULL,
                                   friction_angle = NULL,
                                   maximum_permeability = NULL,
                                   tensile_strength_parameter = NULL,
                                   entry_pressure = NULL,
                                   min_relative_permeability_liquid = NULL,
                                   min_relative_permeability_gas = NULL) {


    assertthat::assert_that(assertthat::is.string(name))
    assertthat::assert_that(assertthat::is.string(type))

    validate_is_null_or_number(value,
                               exponent,
                               residual_liquid_saturation,
                               residual_gas_saturation,
                               p_b,
                               minimum_relative_permeability_liquid,
                               lambda,
                               cutoff_value,
                               reference_permeability,
                               fitting_factor,
                               cohesion,
                               friction_angle,
                               maximum_permeability,
                               tensile_strength_parameter,
                               entry_pressure,
                               min_relative_permeability_liquid,
                               min_relative_permeability_gas)

    validate_is_null_or_string(parameter_name,
                               independent_variable,
                               curve,
                               initial_permeability)

    structure(
        list(
            name = name,
            type = type,
            value = value,
            parameter_name = parameter_name,
            exponent = exponent,
            residual_liquid_saturation = residual_liquid_saturation,
            residual_gas_saturation = residual_gas_saturation,
            p_b = p_b,
            independent_variable = independent_variable,
            curve = curve,
            minimum_relative_permeability_liquid =
                minimum_relative_permeability_liquid,
            lambda = lambda,
            cutoff_value = cutoff_value,
            initial_permeability = initial_permeability,
            reference_permeability = reference_permeability,
            fitting_factor = fitting_factor,
            cohesion = cohesion,
            friction_angle = friction_angle,
            maximum_permeability = maximum_permeability,
            tensile_strength_parameter = tensile_strength_parameter,
            entry_pressure = entry_pressure,
            min_relative_permeability_liquid = min_relative_permeability_liquid,
            min_relative_permeability_gas = min_relative_permeability_gas,
            is_subclass = TRUE,
            attr_names = character(),
            flatten_on_exp = character()
        ),
        class = "r2ogs6_pr_property"
    )
}


#===== r2ogs6_phase =====


#'r2ogs6_phase
#'@description tag: phase, a coherent material with homogeneous properties
#'@param type string: Phase type
#' (get valid types with get_valid_phase_types())
#'@param properties list, r2ogs6_pr_property: Properties
#'@param components list, components
#'@export
r2ogs6_phase <- function(type,
                         properties,
                         components = NULL){

    #Make this more user friendly
    #...

    new_r2ogs6_phase(type,
                     properties,
                     components)
}


new_r2ogs6_phase <- function(type,
                             properties,
                             components = NULL) {

    assertthat::assert_that(assertthat::is.string(type))
    assertthat::assert_that(type %in% get_valid_phase_types())

    validate_wrapper_list(properties, "r2ogs6_ph_property")

    if(!is.null(components)){
        validate_wrapper_list(components, "r2ogs6_component")
    }

    structure(
        list(
            type = type,
            properties = properties,
            components = components,
            is_subclass = TRUE,
            attr_names = character(),
            flatten_on_exp = character()
        ),
        class = "r2ogs6_phase"
    )
}


get_valid_phase_types <- function(){
    valid_phase_types <- c("Gas",
                           "Solid",
                           "AqueousLiquid",
                           "NonAqueousLiquid")

    return(invisible(valid_phase_types))
}


#===== r2ogs6_ph_property =====


#'r2ogs6_ph_property
#'@description tag: property
#'@param name string: Property name
#'@param type string: Property type
#'@param value Optional: string | double: ...
#'@param reference_value Optional: string | double:
#'@param independent_variable Optional: list:
#'@param initial_porosity Optional: string:
#'@param minimal_porosity Optional: string | double:
#'@param maximal_porosity Optional: string | double:
#'@param parameter_name Optional: string:
#'@param exponents Optional: string | numeric:
#'@param offset Optional: string | double:
#'@param exponent Optional: list:
#'@param swelling_pressures Optional: string | numeric:
#'@param lower_saturation_limit Optional: string | double:
#'@param upper_saturation_limit Optional: string | double:
#'@param intrinsic_permeabilities Optional: string | numeric:
#'@export
r2ogs6_ph_property <- function(name,
                               type,
                               value = NULL,
                               reference_value = NULL,
                               independent_variable = NULL,
                               initial_porosity = NULL,
                               minimal_porosity = NULL,
                               maximal_porosity = NULL,
                               parameter_name = NULL,
                               exponents = NULL,
                               offset = NULL,
                               exponent = NULL,
                               swelling_pressures = NULL,
                               lower_saturation_limit = NULL,
                               upper_saturation_limit = NULL,
                               intrinsic_permeabilities = NULL) {


    #Coerce input
    value <- coerce_string_to_numeric(value)
    reference_value <- coerce_string_to_numeric(reference_value)
    minimal_porosity <- coerce_string_to_numeric(minimal_porosity)
    maximal_porosity <- coerce_string_to_numeric(maximal_porosity)
    exponents <- coerce_string_to_numeric(exponents, TRUE)
    offset <- coerce_string_to_numeric(offset)
    swelling_pressures <- coerce_string_to_numeric(swelling_pressures, TRUE)
    lower_saturation_limit <- coerce_string_to_numeric(lower_saturation_limit)
    upper_saturation_limit <- coerce_string_to_numeric(upper_saturation_limit)
    intrinsic_permeabilities <-
        coerce_string_to_numeric(intrinsic_permeabilities, TRUE)

    new_r2ogs6_ph_property(
        name,
        type,
        value,
        reference_value,
        independent_variable,
        initial_porosity,
        minimal_porosity,
        maximal_porosity,
        parameter_name,
        exponents,
        offset,
        exponent,
        swelling_pressures,
        lower_saturation_limit,
        upper_saturation_limit,
        intrinsic_permeabilities
    )
}


new_r2ogs6_ph_property <- function(name,
                                       type,
                                       value = NULL,
                                       reference_value = NULL,
                                       independent_variable = NULL,
                                       initial_porosity = NULL,
                                       minimal_porosity = NULL,
                                       maximal_porosity = NULL,
                                       parameter_name = NULL,
                                       exponents = NULL,
                                       offset = NULL,
                                       exponent = NULL,
                                       swelling_pressures = NULL,
                                       lower_saturation_limit = NULL,
                                       upper_saturation_limit = NULL,
                                       intrinsic_permeabilities = NULL) {

    assertthat::assert_that(assertthat::is.string(name))
    assertthat::assert_that(assertthat::is.string(type))

    validate_is_null_or_number(value,
                               reference_value,
                               minimal_porosity,
                               maximal_porosity,
                               offset,
                               lower_saturation_limit,
                               upper_saturation_limit)

    validate_is_null_or_numeric(exponents,
                                swelling_pressures,
                                intrinsic_permeabilities)

    validate_is_null_or_string(initial_porosity,
                               parameter_name)

    if (!is.null(independent_variable)) {
        independent_variable <-
            validate_param_list(independent_variable,
                                c("variable_name",
                                  "reference_condition",
                                  "slope"))
    }

    if (!is.null(exponent)) {
        exponent <- validate_param_list(exponent,
                                        c("variable_name",
                                          "reference_condition",
                                          "factor"))
    }

    structure(
        list(
            name = name,
            type = type,
            value = value,
            reference_value = reference_value,
            independent_variable = independent_variable,
            initial_porosity = initial_porosity,
            minimal_porosity = minimal_porosity,
            maximal_porosity = maximal_porosity,
            parameter_name = parameter_name,
            exponents = exponents,
            offset = offset,
            exponent = exponent,
            swelling_pressures = swelling_pressures,
            lower_saturation_limit = lower_saturation_limit,
            upper_saturation_limit = upper_saturation_limit,
            intrinsic_permeabilities = intrinsic_permeabilities,
            is_subclass = TRUE,
            attr_names = character(),
            flatten_on_exp = c("exponents",
                               "swelling_pressures",
                               "intrinsic_permeabilities")
        ),
        class = "r2ogs6_ph_property"
    )
}


#===== r2ogs6_component =====


#'r2ogs6_component
#'@description tag: component
#'@param name string:
#'@param properties list, r2ogs6_com_property:
#'@export
r2ogs6_component <- function(name,
                             properties){

    #Make this more user friendly
    #...

    new_r2ogs6_component(name,
                         properties)
}


new_r2ogs6_component <- function(name,
                                 properties) {

    assertthat::assert_that(assertthat::is.string(name))

    for(i in seq_len(length(properties))){
        print(attributes(properties[[i]]))
    }

    validate_wrapper_list(properties, "r2ogs6_com_property")

    structure(
        list(
            name = name,
            properties = properties,
            is_subclass = TRUE,
            attr_names = character(),
            flatten_on_exp = character()
        ),

        class = "r2ogs6_component"
    )
}


#===== r2ogs6_com_property =====


#'r2ogs6_com_property
#'@description tag: property
#'@param name string: Property name
#'@param type string: Property type
#'@param value Optional: string | double: ...
#'@param parameter_name Optional:
#'@export
r2ogs6_com_property <- function(name,
                                type,
                                value = NULL,
                                parameter_name = NULL) {

    #Coerce input
    value <- coerce_string_to_numeric(value)

    new_r2ogs6_com_property(name,
                            type,
                            value,
                            parameter_name)
}


new_r2ogs6_com_property <- function(name,
                                    type,
                                    value = NULL,
                                    parameter_name = NULL) {


    assertthat::assert_that(assertthat::is.string(name))
    assertthat::assert_that(assertthat::is.string(type))

    validate_is_null_or_number(value)
    validate_is_null_or_string(parameter_name)

    structure(
        list(
            name = name,
            type = type,
            value = value,
            parameter_name = parameter_name,
            is_subclass = TRUE,
            attr_names = character(),
            flatten_on_exp = character()
        ),
        class = "r2ogs6_com_property"
    )
}
