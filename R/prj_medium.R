
#===== prj_medium =====


#' prj_medium
#' @description tag: medium, a specific medium with optional id corresponding
#'   to the MaterialIDs
#' @param phases list, prj_phase: Optional: Medium phases
#' @param properties list, prj_pr_property: Optional: Medium properties
#' @param id string | double: Optional: ID corresponding to the MaterialIDs
#' @example man/examples/ex_prj_medium.R
#' @export
prj_medium <- function(phases = NULL,
                          properties = NULL,
                          id = NULL) {

    if(is.character(phases)){
        phases <- NULL
    }

    new_prj_medium(phases,
                      properties,
                      id)
}


new_prj_medium <- function(phases = NULL,
                              properties = NULL,
                              id = NULL) {

    if(length(phases) != 0){
        is_wrapper_list(phases, "prj_phase")
    }

    if(!is.null(properties)){
        is_wrapper_list(properties, "prj_pr_property")
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
        class = "prj_medium"
    )
}


#===== prj_pr_property =====


#' prj_pr_property
#' @description tag: property
#' @param name string:
#' @param type string:
#' @param value Optional:
#' @param parameter_name Optional:
#' @param exponent Optional:
#' @param residual_liquid_saturation Optional:
#' @param residual_gas_saturation Optional:
#' @param initial_porosity Optional:
#' @param minimal_porosity Optional:
#' @param maximal_porosity Optional:
#' @param p_b Optional:
#' @param independent_variable Optional:
#' @param curve Optional:
#' @param minimum_relative_permeability_liquid Optional:
#' @param cutoff_value Optional:
#' @param lambda Optional:
#' @param min_relative_permeability Optional:
#' @param initial_permeability Optional:
#' @param maximum_permeability Optional:
#' @param intrinsic_permeability Optional:
#' @param initial_aperture Optional:
#' @param mean_frac_distance Optional:
#' @param mean_frac_distances Optional: numeric vector
#' @param threshold_strain Optional:
#' @param threshold_strains Optional: numeric vector
#' @param fracture_normal Optional:
#' @param fracture_normals Optional: numeric vector
#' @param fracture_rotation_xy Optional:
#' @param fracture_rotation_yz Optional:
#' @param reference_permeability Optional:
#' @param fitting_factor Optional:
#' @param cohesion Optional:
#' @param friction_angle Optional:
#' @param tensile_strength_parameter Optional:
#' @param b1 Optional:
#' @param b2 Optional:
#' @param b3 Optional:
#' @param minimum_permeability Optional:
#' @param entry_pressure Optional:
#' @param intrinsic_permeabilities Optional:
#' @param exponents Optional:
#' @example man/examples/ex_prj_pr_property.R
#' @export
prj_pr_property <- function(name,
                            type,
                            value = NULL,
                            parameter_name = NULL,
                            exponent = NULL,
                            residual_liquid_saturation = NULL,
                            residual_gas_saturation = NULL,
                            initial_porosity = NULL,
                            minimal_porosity = NULL,
                            maximal_porosity = NULL,
                            p_b = NULL,
                            independent_variable = NULL,
                            curve = NULL,
                            minimum_relative_permeability_liquid = NULL,
                            cutoff_value = NULL,
                            lambda = NULL,
                            min_relative_permeability = NULL,
                            initial_permeability = NULL,
                            maximum_permeability = NULL,
                            intrinsic_permeability = NULL,
                            initial_aperture = NULL,
                            mean_frac_distance = NULL,
                            mean_frac_distances = NULL,
                            threshold_strain = NULL,
                            threshold_strains = NULL,
                            fracture_normal = NULL,
                            fracture_normals = NULL,
                            fracture_rotation_xy = NULL,
                            fracture_rotation_yz = NULL,
                            reference_permeability = NULL,
                            fitting_factor = NULL,
                            cohesion = NULL,
                            friction_angle = NULL,
                            tensile_strength_parameter = NULL,
                            b1 = NULL,
                            b2 = NULL,
                            b3 = NULL,
                            minimum_permeability = NULL,
                            entry_pressure = NULL,
                            intrinsic_permeabilities = NULL,
                            exponents = NULL) {

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

    new_prj_pr_property(name,
                        type,
                        value,
                        parameter_name,
                        exponent,
                        residual_liquid_saturation,
                        residual_gas_saturation,
                        initial_porosity,
                        minimal_porosity,
                        maximal_porosity,
                        p_b,
                        independent_variable,
                        curve,
                        minimum_relative_permeability_liquid,
                        cutoff_value,
                        lambda,
                        min_relative_permeability,
                        initial_permeability,
                        maximum_permeability,
                        intrinsic_permeability,
                        initial_aperture,
                        mean_frac_distance,
                        mean_frac_distances,
                        threshold_strain,
                        threshold_strains,
                        fracture_normal,
                        fracture_normals,
                        fracture_rotation_xy,
                        fracture_rotation_yz,
                        reference_permeability,
                        fitting_factor,
                        cohesion,
                        friction_angle,
                        tensile_strength_parameter,
                        b1,
                        b2,
                        b3,
                        minimum_permeability,
                        entry_pressure,
                        intrinsic_permeabilities,
                        exponents)
}


new_prj_pr_property <- function(name,
                                type,
                                value = NULL,
                                parameter_name = NULL,
                                exponent = NULL,
                                residual_liquid_saturation = NULL,
                                residual_gas_saturation = NULL,
                                initial_porosity = NULL,
                                minimal_porosity = NULL,
                                maximal_porosity = NULL,
                                p_b = NULL,
                                independent_variable = NULL,
                                curve = NULL,
                                minimum_relative_permeability_liquid = NULL,
                                cutoff_value = NULL,
                                lambda = NULL,
                                min_relative_permeability = NULL,
                                initial_permeability = NULL,
                                maximum_permeability = NULL,
                                intrinsic_permeability = NULL,
                                initial_aperture = NULL,
                                mean_frac_distance = NULL,
                                mean_frac_distances = NULL,
                                threshold_strain = NULL,
                                threshold_strains = NULL,
                                fracture_normal = NULL,
                                fracture_normals = NULL,
                                fracture_rotation_xy = NULL,
                                fracture_rotation_yz = NULL,
                                reference_permeability = NULL,
                                fitting_factor = NULL,
                                cohesion = NULL,
                                friction_angle = NULL,
                                tensile_strength_parameter = NULL,
                                b1 = NULL,
                                b2 = NULL,
                                b3 = NULL,
                                minimum_permeability = NULL,
                                entry_pressure = NULL,
                                intrinsic_permeabilities = NULL,
                                exponents = NULL) {


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
                        entry_pressure)

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
                   initial_porosity = initial_porosity,
                   minimal_porosity = minimal_porosity,
                   maximal_porosity = maximal_porosity,
                   p_b = p_b,
                   independent_variable = independent_variable,
                   curve = curve,
                   minimum_relative_permeability_liquid =
                       minimum_relative_permeability_liquid,
                   cutoff_value = cutoff_value,
                   lambda = lambda,
                   min_relative_permeability = min_relative_permeability,
                   initial_permeability = initial_permeability,
                   maximum_permeability = maximum_permeability,
                   intrinsic_permeability = intrinsic_permeability,
                   initial_aperture = initial_aperture,
                   mean_frac_distance = mean_frac_distance,
                   mean_frac_distances = mean_frac_distances,
                   threshold_strain = threshold_strain,
                   threshold_strains = threshold_strains,
                   fracture_normal = fracture_normal,
                   fracture_normals = fracture_normals,
                   fracture_rotation_xy = fracture_rotation_xy,
                   fracture_rotation_yz = fracture_rotation_yz,
                   reference_permeability = reference_permeability,
                   fitting_factor = fitting_factor,
                   cohesion = cohesion,
                   friction_angle = friction_angle,
                   tensile_strength_parameter = tensile_strength_parameter,
                   b1 = b1,
                   b2 = b2,
                   b3 = b3,
                   minimum_permeability = minimum_permeability,
                   entry_pressure = entry_pressure,
                   intrinsic_permeabilities = intrinsic_permeabilities,
                   exponents = exponents,
                   xpath = "media/medium/properties/property",
                   attr_names = character(),
                   flatten_on_exp = c("value")
    ),
    class = "prj_pr_property"
    )
}


#' ogs6_get_medium_property
#' @description Returns a medium property based on the property name
#' @param medium prj_medium
#' @param name string: The property name
#' @return prj_pr_property
#' @export
ogs6_get_medium_property <- function(medium, name){

    assertthat::assert_that(class(medium) == "prj_medium")
    assertthat::assert_that(assertthat::is.string(name))

    properties_names <- lapply(medium$properties, `[[`, "name")
    property <- medium$properties[properties_names == name][[1]]

    return(invisible(property))
}


#===== prj_phase =====


#' prj_phase
#' @description tag: phase, a coherent material with homogeneous properties
#' @param type string: Phase type
#'   (get valid types with get_valid_phase_types())
#' @param properties list, prj_pr_property: Properties
#' @param components list, components
#' @example man/examples/ex_prj_phase.R
#' @export
prj_phase <- function(type,
                         properties = NULL,
                         components = NULL){

    #Make this more user friendly
    #...

    new_prj_phase(type,
                     properties,
                     components)
}


new_prj_phase <- function(type,
                             properties = NULL,
                             components = NULL) {

    assertthat::assert_that(assertthat::is.string(type))
    assertthat::assert_that(type %in% get_valid_phase_types())

    if(!is.null(properties)){
        is_wrapper_list(properties, "prj_ph_property")
    }

    if(!is.null(components)){
        is_wrapper_list(components, "prj_component")
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
        class = "prj_phase"
    )
}


get_valid_phase_types <- function(){
    valid_phase_types <- c("Gas",
                           "Solid",
                           "AqueousLiquid",
                           "NonAqueousLiquid")

    return(invisible(valid_phase_types))
}


#===== prj_ph_property =====


#' prj_ph_property
#' @description tag: property
#' @param name string:
#' @param type string:
#' @param value Optional:
#' @param reference_value Optional:
#' @param offset Optional:
#' @param exponent Optional:
#' @param parameter_name Optional:
#' @param swelling_pressures Optional:
#' @param exponents Optional:
#' @param lower_saturation_limit Optional:
#' @param upper_saturation_limit Optional:
#' @param ... independent_variable, dvalue
#' @example man/examples/ex_prj_ph_property.R
#' @export
prj_ph_property <- function(name,
                            type,
                            value = NULL,
                            reference_value = NULL,
                            offset = NULL,
                            exponent = NULL,
                            parameter_name = NULL,
                            swelling_pressures = NULL,
                            exponents = NULL,
                            lower_saturation_limit = NULL,
                            upper_saturation_limit = NULL,
                            tortuosity = NULL,
                            curve = NULL,
                            ...) {

    #Coerce input
    if (!is.list(value)) {
        value <- coerce_string_to_numeric(value)
    }

    reference_value <- coerce_string_to_numeric(reference_value)
    exponents <- coerce_string_to_numeric(exponents)
    offset <- coerce_string_to_numeric(offset)
    swelling_pressures <- coerce_string_to_numeric(swelling_pressures)
    lower_saturation_limit <- coerce_string_to_numeric(lower_saturation_limit)
    upper_saturation_limit <- coerce_string_to_numeric(upper_saturation_limit)
    tortuosity <- (coerce_string_to_numeric(tortuosity))

    ellipsis_list <- list(...)
    independent_variable <-
        ellipsis_list[names(ellipsis_list) == "independent_variable"]
    dvalue <- ellipsis_list[names(ellipsis_list) == "dvalue"]

    new_prj_ph_property(name,
                        type,
                        value,
                        dvalue,
                        reference_value,
                        independent_variable,
                        offset,
                        exponent,
                        parameter_name,
                        swelling_pressures,
                        exponents,
                        lower_saturation_limit,
                        upper_saturation_limit,
                        tortuosity,
                        curve)
}


new_prj_ph_property <- function(name,
                                type,
                                value = NULL,
                                dvalue = NULL,
                                reference_value = NULL,
                                independent_variable = NULL,
                                offset = NULL,
                                exponent = NULL,
                                parameter_name = NULL,
                                swelling_pressures = NULL,
                                exponents = NULL,
                                lower_saturation_limit = NULL,
                                upper_saturation_limit = NULL,
                                tortuosity = NULL,
                                curve = NULL) {

    are_strings(name,
                type)

    if (is.list(value)) {
        are_null_or_strings(value[[1]])
    } else {
        are_null_or_numeric(value)
    }

    if (!is.null(dvalue)) {
        dvalue <- lapply(dvalue, function(x){
            are_null_or_strings(x[[1]])
            x[[2]] <- coerce_string_to_numeric(x[[2]])
            return(x)
        })
    }

    are_null_or_numbers(
        reference_value,
        offset,
        lower_saturation_limit,
        upper_saturation_limit,
        tortuosity
    )

    are_null_or_numeric(swelling_pressures,
                        exponents)

    are_null_or_strings(parameter_name, curve)

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

    structure(list(name = name,
                   type = type,
                   value = value,
                   dvalue = dvalue,
                   reference_value = reference_value,
                   independent_variable = independent_variable,
                   offset = offset,
                   exponent = exponent,
                   parameter_name = parameter_name,
                   swelling_pressures = swelling_pressures,
                   exponents = exponents,
                   lower_saturation_limit = lower_saturation_limit,
                   upper_saturation_limit = upper_saturation_limit,
                   xpath = "media/medium/phases/phase/properties/property",
                   attr_names = character(),
                   flatten_on_exp = c("value", "exponents",
                                      "swelling_pressures"),
                   unwrap_on_exp = c("independent_variable", "dvalue")
    ),
    class = "prj_ph_property"
    )
}


#===== prj_component =====


#' prj_component
#' @description tag: component
#' @param name string:
#' @param properties list, prj_com_property:
#' @example man/examples/ex_prj_component.R
#' @export
prj_component <- function(name,
                             properties){

    #Make this more user friendly
    #...

    new_prj_component(name,
                         properties)
}


new_prj_component <- function(name,
                                 properties) {

    assertthat::assert_that(assertthat::is.string(name))

    is_wrapper_list(properties, "prj_com_property")

    structure(
        list(
            name = name,
            properties = properties,
            xpath = "media/medium/phases/phase/components/component",
            attr_names = character(),
            flatten_on_exp = character()
        ),

        class = "prj_component"
    )
}


#===== prj_com_property =====


#' prj_com_property
#' @description tag: property
#' @param name string: Property name
#' @param type string: Property type
#' @param value Optional: string | double: ...
#' @param parameter_name Optional:
#' @param reference_diffusion Optional: character
#' @param activation_energy Optional: string | double
#' @param reference_temperature Optional: numeric
#' @param triple_temperature Optional: numeric
#' @param triple_pressure Optional: numeric
#' @param critical_temperature Optional: numeric
#' @param critical_pressure Optional: numeric
#' @param reference_pressure Optional: numeric
#' @example man/examples/ex_prj_com_property.R
#' @export
prj_com_property <- function(name,
                             type,
                             value = NULL,
                             parameter_name = NULL,
                             reference_diffusion = NULL,
                             activation_energy = NULL,
                             reference_temperature = NULL,
                             triple_temperature = NULL,
                             triple_pressure = NULL,
                             critical_temperature = NULL,
                             critical_pressure = NULL,
                             reference_pressure= NULL) {



    #Coerce input
    value <- coerce_string_to_numeric(value)
    activation_energy <- coerce_string_to_numeric(activation_energy)
    reference_temperature <- coerce_string_to_numeric(reference_temperature)
    triple_temperature <- coerce_string_to_numeric(triple_temperature)
    triple_pressure <- coerce_string_to_numeric(triple_pressure)
    critical_temperature <- coerce_string_to_numeric(critical_temperature)
    critical_pressure <- coerce_string_to_numeric(critical_pressure)
    reference_pressure <- coerce_string_to_numeric(reference_pressure)

    new_prj_com_property(name,
                         type,
                         value,
                         parameter_name,
                         reference_diffusion,
                         activation_energy,
                         reference_temperature,
                         triple_temperature,
                         triple_pressure,
                         critical_temperature,
                         critical_pressure,
                         reference_pressure
                         )
}


new_prj_com_property <- function(name,
                                 type,
                                 value = NULL,
                                 parameter_name = NULL,
                                 reference_diffusion = NULL,
                                 activation_energy = NULL,
                                 reference_temperature = NULL,
                                 triple_temperature = NULL,
                                 triple_pressure = NULL,
                                 critical_temperature = NULL,
                                 critical_pressure = NULL,
                                 reference_pressure= NULL) {


    assertthat::assert_that(assertthat::is.string(name))
    assertthat::assert_that(assertthat::is.string(type))

    are_null_or_numbers(value)
    are_null_or_numeric(activation_energy)
    are_null_or_numeric(reference_temperature)
    are_null_or_numeric(triple_temperature)
    are_null_or_numeric(triple_pressure)
    are_null_or_numeric(critical_temperature)
    are_null_or_numeric(critical_pressure)
    are_null_or_numeric(reference_pressure)
    are_null_or_strings(parameter_name)
    are_null_or_strings(reference_diffusion)

    structure(
        list(
            name = name,
            type = type,
            value = value,
            parameter_name = parameter_name,
            reference_diffusion = reference_diffusion,
            activation_energy = activation_energy,
            reference_temperature = reference_temperature,
            triple_temperature = triple_temperature,
            triple_pressure = triple_pressure,
            critical_temperature = critical_temperature,
            critical_pressure = critical_pressure,
            reference_pressure = reference_pressure,
            xpath = paste0("media/medium/phases/phase/components/component/",
                           "properties/property"),
            attr_names = character(),
            flatten_on_exp = character()
        ),
        class = "prj_com_property"
    )
}
