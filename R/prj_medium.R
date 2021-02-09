
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

    if(is.character(phases)){
        phases <- NULL
    }

    new_r2ogs6_medium(phases,
                      properties,
                      id)
}


new_r2ogs6_medium <- function(phases = NULL,
                              properties = NULL,
                              id = NULL) {

    if(length(phases) != 0){
        is_wrapper_list(phases, "r2ogs6_phase")
    }

    if(!is.null(properties)){
        is_wrapper_list(properties, "r2ogs6_pr_property")
    }

    are_null_or_strings(id)

    structure(
        list(
            phases = phases,
            properties = properties,
            id = id,
            xpath = "media/medium",
            attr_names = c("id"),
            flatten_on_exp = character()
        ),
        class = "r2ogs6_medium"
    )
}


#===== r2ogs6_pr_property =====


#'r2ogs6_pr_property
#'@description tag: property
#'@param name string:
#'@param type string:
#'@param value Optional:
#'@param parameter_name Optional:
#'@param exponent Optional:
#'@param residual_liquid_saturation Optional:
#'@param residual_gas_saturation Optional:
#'@param p_b Optional:
#'@param independent_variable Optional:
#'@param curve Optional:
#'@param minimum_relative_permeability_liquid Optional:
#'@param initial_permeability Optional:
#'@param maximum_permeability Optional:
#'@param lambda Optional:
#'@param cutoff_value Optional:
#'@param intrinsic_permeability Optional:
#'@param initial_aperture Optional:
#'@param mean_frac_distance Optional:
#'@param threshold_strain Optional:
#'@param fracture_normal Optional:
#'@param reference_permeability Optional:
#'@param fitting_factor Optional:
#'@param cohesion Optional:
#'@param friction_angle Optional:
#'@param tensile_strength_parameter Optional:
#'@param b1 Optional:
#'@param b2 Optional:
#'@param b3 Optional:
#'@param minimum_permeability Optional:
#'@param initial_porosity Optional:
#'@param entry_pressure Optional:
#'@param min_relative_permeability_liquid Optional:
#'@param min_relative_permeability_gas Optional:
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
                               initial_permeability = NULL,
                               maximum_permeability = NULL,
                               lambda = NULL,
                               cutoff_value = NULL,
                               intrinsic_permeability = NULL,
                               initial_aperture = NULL,
                               mean_frac_distance = NULL,
                               threshold_strain = NULL,
                               fracture_normal = NULL,
                               reference_permeability = NULL,
                               fitting_factor = NULL,
                               cohesion = NULL,
                               friction_angle = NULL,
                               tensile_strength_parameter = NULL,
                               b1 = NULL,
                               b2 = NULL,
                               b3 = NULL,
                               minimum_permeability = NULL,
                               initial_porosity = NULL,
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
        initial_permeability,
        maximum_permeability,
        lambda,
        cutoff_value,
        intrinsic_permeability,
        initial_aperture,
        mean_frac_distance,
        threshold_strain,
        fracture_normal,
        reference_permeability,
        fitting_factor,
        cohesion,
        friction_angle,
        tensile_strength_parameter,
        b1,
        b2,
        b3,
        minimum_permeability,
        initial_porosity,
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
                                   minimum_relative_permeability_liquid = NULL,
                                   initial_permeability = NULL,
                                   maximum_permeability = NULL,
                                   lambda = NULL,
                                   cutoff_value = NULL,
                                   intrinsic_permeability = NULL,
                                   initial_aperture = NULL,
                                   mean_frac_distance = NULL,
                                   threshold_strain = NULL,
                                   fracture_normal = NULL,
                                   reference_permeability = NULL,
                                   fitting_factor = NULL,
                                   cohesion = NULL,
                                   friction_angle = NULL,
                                   tensile_strength_parameter = NULL,
                                   b1 = NULL,
                                   b2 = NULL,
                                   b3 = NULL,
                                   minimum_permeability = NULL,
                                   initial_porosity = NULL,
                                   entry_pressure = NULL,
                                   min_relative_permeability_liquid = NULL,
                                   min_relative_permeability_gas = NULL) {


    assertthat::assert_that(assertthat::is.string(name))
    assertthat::assert_that(assertthat::is.string(type))

    are_null_or_numeric(value)

    are_null_or_numbers(exponent,
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

    are_null_or_strings(parameter_name,
                               independent_variable,
                               curve,
                               initial_permeability)

    structure(list(name = name,
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
                   initial_permeability = initial_permeability,
                   maximum_permeability = maximum_permeability,
                   lambda = lambda,
                   cutoff_value = cutoff_value,
                   intrinsic_permeability = intrinsic_permeability,
                   initial_aperture = initial_aperture,
                   mean_frac_distance = mean_frac_distance,
                   threshold_strain = threshold_strain,
                   fracture_normal = fracture_normal,
                   reference_permeability = reference_permeability,
                   fitting_factor = fitting_factor,
                   cohesion = cohesion,
                   friction_angle = friction_angle,
                   tensile_strength_parameter = tensile_strength_parameter,
                   b1 = b1,
                   b2 = b2,
                   b3 = b3,
                   minimum_permeability = minimum_permeability,
                   initial_porosity = initial_porosity,
                   entry_pressure = entry_pressure,
                   min_relative_permeability_liquid = min_relative_permeability_liquid,
                   min_relative_permeability_gas = min_relative_permeability_gas,
                   xpath = "media/medium/properties/property",
                   attr_names = character(),
                   flatten_on_exp = character()
    ),
    class = "r2ogs6_pr_property"
    )
}


#'ogs_get_medium_property
#'@description Returns a medium property based on the property name
#'@param medium r2ogs6_medium
#'@param name string: The property name
#'@return r2ogs6_pr_property
#'@export
ogs_get_medium_property <- function(medium, name){

    assertthat::assert_that(class(medium) == "r2ogs6_medium")
    assertthat::assert_that(assertthat::is.string(name))

    properties_names <- lapply(medium$properties, `[[`, "name")
    property <- properties[properties_names == name][[1]]

    return(invisible(property))
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
                         properties = NULL,
                         components = NULL){

    #Make this more user friendly
    #...

    new_r2ogs6_phase(type,
                     properties,
                     components)
}


new_r2ogs6_phase <- function(type,
                             properties = NULL,
                             components = NULL) {

    assertthat::assert_that(assertthat::is.string(type))
    assertthat::assert_that(type %in% get_valid_phase_types())

    if(!is.null(properties)){
        is_wrapper_list(properties, "r2ogs6_ph_property")
    }

    if(!is.null(components)){
        is_wrapper_list(components, "r2ogs6_component")
    }

    structure(
        list(
            type = type,
            properties = properties,
            components = components,
            xpath = "media/medium/phases/phase",
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
#'@param ... independent_variable
#'@export
r2ogs6_ph_property <- function(name,
                               type,
                               value = NULL,
                               reference_value = NULL,
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
                               intrinsic_permeabilities = NULL,
                               ...) {


    #Coerce input
    value <- coerce_string_to_numeric(value)
    reference_value <- coerce_string_to_numeric(reference_value)
    minimal_porosity <- coerce_string_to_numeric(minimal_porosity)
    maximal_porosity <- coerce_string_to_numeric(maximal_porosity)
    exponents <- coerce_string_to_numeric(exponents)
    offset <- coerce_string_to_numeric(offset)
    swelling_pressures <- coerce_string_to_numeric(swelling_pressures)
    lower_saturation_limit <- coerce_string_to_numeric(lower_saturation_limit)
    upper_saturation_limit <- coerce_string_to_numeric(upper_saturation_limit)
    intrinsic_permeabilities <-
        coerce_string_to_numeric(intrinsic_permeabilities)

    ellipsis_list <- list(...)
    independent_variable <-
        ellipsis_list[names(ellipsis_list) == "independent_variable"]

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

    are_null_or_numeric(value)

    are_null_or_numbers(
        reference_value,
        minimal_porosity,
        maximal_porosity,
        offset,
        lower_saturation_limit,
        upper_saturation_limit
    )

    are_null_or_numeric(exponents,
                        swelling_pressures,
                        intrinsic_permeabilities)

    are_null_or_strings(initial_porosity,
                        parameter_name)

    if (!is.null(independent_variable)) {
        independent_variable <- lapply(independent_variable, function(x){
            x <- coerce_names(x,
                              c("variable_name",
                                "reference_condition",
                                "slope"))
        })
    }

    if (!is.null(exponent)) {
        exponent <- coerce_names(exponent,
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
            xpath = "media/medium/phases/phase/properties/property",
            attr_names = character(),
            flatten_on_exp = c("exponents",
                               "swelling_pressures",
                               "intrinsic_permeabilities"),
            unwrap_on_exp = c("independent_variable")
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

    is_wrapper_list(properties, "r2ogs6_com_property")

    structure(
        list(
            name = name,
            properties = properties,
            xpath = "media/medium/phases/phase/components/component",
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

    are_null_or_numbers(value)
    are_null_or_strings(parameter_name)

    structure(
        list(
            name = name,
            type = type,
            value = value,
            parameter_name = parameter_name,
            xpath = paste0("media/medium/phases/phase/components/component/",
                           "properties/property"),
            attr_names = character(),
            flatten_on_exp = character()
        ),
        class = "r2ogs6_com_property"
    )
}
