
#===== r2ogs6_process =====


#'r2ogs6_process
#'@description tag: process (parent tag: processes)
#'@param name string:
#'@param type string:
#'@param integration_order string | double:
#'@param process_variables list, character:
#'@param secondary_variables Optional:
#'@param specific_body_force Optional:
#'@param constitutive_relation Optional:
#'@param solid_density Optional:
#'@param dimension Optional:
#'@param coupling_scheme Optional:
#'@param reference_solid_density Optional:
#'@param linear_thermal_expansion_coefficient Optional:
#'@param specific_heat_capacity Optional:
#'@param thermal_conductivity Optional:
#'@param darcy_gravity Optional:
#'@param reference_temperature Optional:
#'@param fracture_model Optional:
#'@param fracture_properties Optional:
#'@param jacobian_assembler Optional:
#'@param internal_length Optional:
#'@param mass_lumping Optional:
#'@param porosity Optional:
#'@param calculatesurfaceflux Optional:
#'@param intrinsic_permeability Optional:
#'@param specific_storage Optional:
#'@param fluid_viscosity Optional:
#'@param biot_coefficient Optional:
#'@param fluid_density Optional:
#'@param initial_effective_stress Optional:
#'@param initial_fracture_effective_stress Optional:
#'@param phasefield_parameters Optional:
#'@param deactivate_matrix_in_flow Optional:
#'@param borehole_heat_exchangers Optional:
#'@param temperature Optional:
#'@param reactive_system Optional:
#'@param fluid_specific_heat_source Optional:
#'@param fluid_specific_isobaric_heat_capacity Optional:
#'@param solid_hydraulic_permeability Optional:
#'@param solid_specific_heat_source Optional:
#'@param solid_heat_conductivity Optional:
#'@param solid_specific_isobaric_heat_capacity Optional:
#'@param tortuosity Optional:
#'@param diffusion_coefficient Optional:
#'@param solid_density_dry Optional:
#'@param solid_density_initial Optional:
#'@param characteristic_pressure Optional:
#'@param characteristic_temperature Optional:
#'@param characteristic_vapour_mass_fraction Optional:
#'@param output_element_matrices Optional:
#'@param material_property Optional:
#'@param diffusion_coeff_component_b Optional:
#'@param diffusion_coeff_component_a Optional:
#'@param hydro_crack_scheme Optional:
#'@param at_num Optional:
#'@param initial_stress Optional:
#'@param split_method Optional:
#'@param reg_param Optional:
#'@param thermal_parameters Optional:
#'@param non_advective_form Optional:
#'@param fluid Optional:
#'@param porous_medium Optional:
#'@param decay_rate Optional:
#'@param fluid_reference_density Optional:
#'@param retardation_factor Optional:
#'@param solute_dispersivity_longitudinal Optional:
#'@param solute_dispersivity_transverse Optional:
#'@param molecular_diffusion_coefficient Optional:
#'@param density_solid Optional:
#'@param latent_heat_evaporation Optional:
#'@param pf_irrv Optional:
#'@export
r2ogs6_process <- function(name,
                           type,
                           integration_order,
                           process_variables,
                           secondary_variables = NULL,
                           specific_body_force = NULL,
                           constitutive_relation = NULL,
                           solid_density = NULL,
                           dimension = NULL,
                           coupling_scheme = NULL,
                           reference_solid_density = NULL,
                           linear_thermal_expansion_coefficient = NULL,
                           specific_heat_capacity = NULL,
                           thermal_conductivity = NULL,
                           darcy_gravity = NULL,
                           reference_temperature = NULL,
                           fracture_model = NULL,
                           fracture_properties = NULL,
                           jacobian_assembler = NULL,
                           internal_length = NULL,
                           mass_lumping = NULL,
                           porosity = NULL,
                           calculatesurfaceflux = NULL,
                           intrinsic_permeability = NULL,
                           specific_storage = NULL,
                           fluid_viscosity = NULL,
                           biot_coefficient = NULL,
                           fluid_density = NULL,
                           initial_effective_stress = NULL,
                           initial_fracture_effective_stress = NULL,
                           phasefield_parameters = NULL,
                           deactivate_matrix_in_flow = NULL,
                           borehole_heat_exchangers = NULL,
                           temperature = NULL,
                           reactive_system = NULL,
                           fluid_specific_heat_source = NULL,
                           fluid_specific_isobaric_heat_capacity = NULL,
                           solid_hydraulic_permeability = NULL,
                           solid_specific_heat_source = NULL,
                           solid_heat_conductivity = NULL,
                           solid_specific_isobaric_heat_capacity = NULL,
                           tortuosity = NULL,
                           diffusion_coefficient = NULL,
                           solid_density_dry = NULL,
                           solid_density_initial = NULL,
                           characteristic_pressure = NULL,
                           characteristic_temperature = NULL,
                           characteristic_vapour_mass_fraction = NULL,
                           output_element_matrices = NULL,
                           material_property = NULL,
                           diffusion_coeff_component_b = NULL,
                           diffusion_coeff_component_a = NULL,
                           hydro_crack_scheme = NULL,
                           at_num = NULL,
                           initial_stress = NULL,
                           split_method = NULL,
                           reg_param = NULL,
                           thermal_parameters = NULL,
                           non_advective_form = NULL,
                           fluid = NULL,
                           porous_medium = NULL,
                           decay_rate = NULL,
                           fluid_reference_density = NULL,
                           retardation_factor = NULL,
                           solute_dispersivity_longitudinal = NULL,
                           solute_dispersivity_transverse = NULL,
                           molecular_diffusion_coefficient = NULL,
                           density_solid = NULL,
                           latent_heat_evaporation = NULL,
                           pf_irrv = NULL){

    #Coerce input
    integration_order <- coerce_string_to_numeric(integration_order)
    specific_body_force <- coerce_string_to_numeric(specific_body_force, TRUE)
    dimension <- coerce_string_to_numeric(dimension)
    reference_temperature <- coerce_string_to_numeric(reference_temperature)
    internal_length <- coerce_string_to_numeric(internal_length)
    porosity <- coerce_string_to_numeric(porosity)
    fluid_specific_heat_source <-
        coerce_string_to_numeric(fluid_specific_heat_source)
    fluid_specific_isobaric_heat_capacity <-
        coerce_string_to_numeric(fluid_specific_isobaric_heat_capacity)
    solid_hydraulic_permeability <-
        coerce_string_to_numeric(solid_hydraulic_permeability)
    solid_specific_heat_source <-
        coerce_string_to_numeric(solid_specific_heat_source)
    solid_heat_conductivity <-
        coerce_string_to_numeric(solid_heat_conductivity)
    solid_specific_isobaric_heat_capacity <-
        coerce_string_to_numeric(solid_specific_isobaric_heat_capacity)
    tortuosity <- coerce_string_to_numeric(tortuosity)
    diffusion_coefficient <- coerce_string_to_numeric(diffusion_coefficient)
    solid_density_dry <- coerce_string_to_numeric(solid_density_dry)
    solid_density_initial <- coerce_string_to_numeric(solid_density_initial)
    characteristic_pressure <- coerce_string_to_numeric(characteristic_pressure)
    characteristic_temperature <-
        coerce_string_to_numeric(characteristic_temperature)
    characteristic_vapour_mass_fraction <-
        coerce_string_to_numeric(characteristic_vapour_mass_fraction)
    output_element_matrices <- coerce_string_to_numeric(output_element_matrices)
    at_num <- coerce_string_to_numeric(at_num)
    split_method <- coerce_string_to_numeric(split_method)
    reg_param <- coerce_string_to_numeric(reg_param)
    deactivate_matrix_in_flow <-
        coerce_string_to_numeric(deactivate_matrix_in_flow)
    pf_irrv <- coerce_string_to_numeric(pf_irrv)

    new_r2ogs6_process(
        name,
        type,
        integration_order,
        process_variables,
        secondary_variables,
        specific_body_force,
        constitutive_relation,
        solid_density,
        dimension,
        coupling_scheme,
        reference_solid_density,
        linear_thermal_expansion_coefficient,
        specific_heat_capacity,
        thermal_conductivity,
        darcy_gravity,
        reference_temperature,
        fracture_model,
        fracture_properties,
        jacobian_assembler,
        internal_length,
        mass_lumping,
        porosity,
        calculatesurfaceflux,
        intrinsic_permeability,
        specific_storage,
        fluid_viscosity,
        biot_coefficient,
        fluid_density,
        initial_effective_stress,
        initial_fracture_effective_stress,
        phasefield_parameters,
        deactivate_matrix_in_flow,
        borehole_heat_exchangers,
        temperature,
        reactive_system,
        fluid_specific_heat_source,
        fluid_specific_isobaric_heat_capacity,
        solid_hydraulic_permeability,
        solid_specific_heat_source,
        solid_heat_conductivity,
        solid_specific_isobaric_heat_capacity,
        tortuosity,
        diffusion_coefficient,
        solid_density_dry,
        solid_density_initial,
        characteristic_pressure,
        characteristic_temperature,
        characteristic_vapour_mass_fraction,
        output_element_matrices,
        material_property,
        diffusion_coeff_component_b,
        diffusion_coeff_component_a,
        hydro_crack_scheme,
        at_num,
        initial_stress,
        split_method,
        reg_param,
        thermal_parameters,
        non_advective_form,
        fluid,
        porous_medium,
        decay_rate,
        fluid_reference_density,
        retardation_factor,
        solute_dispersivity_longitudinal,
        solute_dispersivity_transverse,
        molecular_diffusion_coefficient,
        density_solid,
        latent_heat_evaporation,
        pf_irrv
    )
}


new_r2ogs6_process <- function(name,
                               type,
                               integration_order,
                               process_variables,
                               secondary_variables = NULL,
                               specific_body_force = NULL,
                               constitutive_relation = NULL,
                               solid_density = NULL,
                               dimension = NULL,
                               coupling_scheme = NULL,
                               reference_solid_density = NULL,
                               linear_thermal_expansion_coefficient = NULL,
                               specific_heat_capacity = NULL,
                               thermal_conductivity = NULL,
                               darcy_gravity = NULL,
                               reference_temperature = NULL,
                               fracture_model = NULL,
                               fracture_properties = NULL,
                               jacobian_assembler = NULL,
                               internal_length = NULL,
                               mass_lumping = NULL,
                               porosity = NULL,
                               calculatesurfaceflux = NULL,
                               intrinsic_permeability = NULL,
                               specific_storage = NULL,
                               fluid_viscosity = NULL,
                               biot_coefficient = NULL,
                               fluid_density = NULL,
                               initial_effective_stress = NULL,
                               initial_fracture_effective_stress = NULL,
                               phasefield_parameters = NULL,
                               deactivate_matrix_in_flow = NULL,
                               borehole_heat_exchangers = NULL,
                               temperature = NULL,
                               reactive_system = NULL,
                               fluid_specific_heat_source = NULL,
                               fluid_specific_isobaric_heat_capacity = NULL,
                               solid_hydraulic_permeability = NULL,
                               solid_specific_heat_source = NULL,
                               solid_heat_conductivity = NULL,
                               solid_specific_isobaric_heat_capacity = NULL,
                               tortuosity = NULL,
                               diffusion_coefficient = NULL,
                               solid_density_dry = NULL,
                               solid_density_initial = NULL,
                               characteristic_pressure = NULL,
                               characteristic_temperature = NULL,
                               characteristic_vapour_mass_fraction = NULL,
                               output_element_matrices = NULL,
                               material_property = NULL,
                               diffusion_coeff_component_b = NULL,
                               diffusion_coeff_component_a = NULL,
                               hydro_crack_scheme = NULL,
                               at_num = NULL,
                               initial_stress = NULL,
                               split_method = NULL,
                               reg_param = NULL,
                               thermal_parameters = NULL,
                               non_advective_form = NULL,
                               fluid = NULL,
                               porous_medium = NULL,
                               decay_rate = NULL,
                               fluid_reference_density = NULL,
                               retardation_factor = NULL,
                               solute_dispersivity_longitudinal = NULL,
                               solute_dispersivity_transverse = NULL,
                               molecular_diffusion_coefficient = NULL,
                               density_solid = NULL,
                               latent_heat_evaporation = NULL,
                               pf_irrv = NULL) {

    #Basic validation
    assertthat::assert_that(assertthat::is.string(name))
    assertthat::assert_that(assertthat::is.string(type))
    assertthat::assert_that(assertthat::is.number(integration_order))

    validate_is_null_or_class_obj(material_property,
                                  "r2ogs6_material_property")

    process_variables <- validate_process_variables(process_variables)
    secondary_variables <- validate_secondary_variables(secondary_variables)

    validate_is_null_or_numeric(specific_body_force)

    validate_constitutive_relation(constitutive_relation)

    if(!is.null(darcy_gravity)){
        darcy_gravity <- validate_param_list(darcy_gravity,
                                             c("axis_id", "g"))
    }

    validate_is_null_or_class_obj(fracture_model, "r2ogs6_fracture_model")

    validate_is_null_or_class_obj(phasefield_parameters,
                                  "r2ogs6_phasefield_parameters")

    if(!is.null(calculatesurfaceflux)){
        calculatesurfaceflux <- validate_param_list(calculatesurfaceflux,
                                                    c("mesh", "property_name"))
    }

    if(!is.null(borehole_heat_exchangers)){
        validate_wrapper_list(borehole_heat_exchangers,
                              "r2ogs6_borehole_heat_exchanger")
    }

    if(!is.null(reactive_system)){
        reactive_system <- validate_param_list(reactive_system, c("type"))
    }

    if(!is.null(thermal_parameters)){
        thermal_parameters <-
            validate_param_list(thermal_parameters,
                                c("linear_thermal_expansion_coefficien",
                                  "specific_heat_capacity",
                                  "thermal_conductivity",
                                  "residual_thermal_conductivity"))
    }

    if(!is.null(porous_medium)){
        validate_wrapper_list(porous_medium,
                              "r2ogs6_porous_medium")
    }

    fluid <- validate_fluid(fluid)

    validate_is_null_or_string(coupling_scheme,
                               solid_density,
                               reference_solid_density,
                               linear_thermal_expansion_coefficient,
                               specific_heat_capacity,
                               thermal_conductivity,
                               intrinsic_permeability,
                               specific_storage,
                               fluid_viscosity,
                               biot_coefficient,
                               fluid_density,
                               initial_effective_stress,
                               initial_fracture_effective_stress,
                               temperature,
                               diffusion_coeff_component_b,
                               diffusion_coeff_component_a,
                               hydro_crack_scheme,
                               initial_stress,
                               decay_rate,
                               fluid_reference_density,
                               retardation_factor,
                               solute_dispersivity_longitudinal,
                               solute_dispersivity_transverse,
                               molecular_diffusion_coefficient,
                               density_solid,
                               latent_heat_evaporation)


    validate_is_null_or_number(dimension,
                               reference_temperature,
                               internal_length,
                               porosity,
                               fluid_specific_heat_source,
                               fluid_specific_isobaric_heat_capacity,
                               solid_hydraulic_permeability,
                               solid_specific_heat_source,
                               solid_heat_conductivity,
                               solid_specific_isobaric_heat_capacity,
                               tortuosity,
                               diffusion_coefficient,
                               solid_density_dry,
                               solid_density_initial,
                               characteristic_pressure,
                               characteristic_temperature,
                               characteristic_vapour_mass_fraction,
                               output_element_matrices,
                               at_num,
                               split_method,
                               reg_param,
                               deactivate_matrix_in_flow,
                               pf_irrv)

    validate_is_null_or_str_flag(mass_lumping,
                                 non_advective_form)

    structure(
        list(
            name = name,
            type = type,
            integration_order = integration_order,
            process_variables = process_variables,
            secondary_variables = secondary_variables,
            specific_body_force = specific_body_force,
            constitutive_relation = constitutive_relation,
            solid_density = solid_density,
            dimension = dimension,
            coupling_scheme = coupling_scheme,
            reference_solid_density = reference_solid_density,
            linear_thermal_expansion_coefficient =
                linear_thermal_expansion_coefficient,
            specific_heat_capacity = specific_heat_capacity,
            thermal_conductivity = thermal_conductivity,
            darcy_gravity = darcy_gravity,
            reference_temperature = reference_temperature,
            fracture_model = fracture_model,
            fracture_properties = fracture_properties,
            jacobian_assembler = jacobian_assembler,
            internal_length = internal_length,
            mass_lumping = mass_lumping,
            porosity = porosity,
            calculatesurfaceflux = calculatesurfaceflux,
            intrinsic_permeability = intrinsic_permeability,
            specific_storage = specific_storage,
            fluid_viscosity = fluid_viscosity,
            biot_coefficient = biot_coefficient,
            fluid_density = fluid_density,
            initial_effective_stress = initial_effective_stress,
            initial_fracture_effective_stress =
                initial_fracture_effective_stress,
            phasefield_parameters = phasefield_parameters,
            deactivate_matrix_in_flow = deactivate_matrix_in_flow,
            borehole_heat_exchangers = borehole_heat_exchangers,
            temperature = temperature,
            reactive_system = reactive_system,
            fluid_specific_heat_source = fluid_specific_heat_source,
            fluid_specific_isobaric_heat_capacity =
                fluid_specific_isobaric_heat_capacity,
            solid_hydraulic_permeability = solid_hydraulic_permeability,
            solid_specific_heat_source = solid_specific_heat_source,
            solid_heat_conductivity = solid_heat_conductivity,
            solid_specific_isobaric_heat_capacity =
                solid_specific_isobaric_heat_capacity,
            tortuosity = tortuosity,
            diffusion_coefficient = diffusion_coefficient,
            solid_density_dry = solid_density_dry,
            solid_density_initial = solid_density_initial,
            characteristic_pressure = characteristic_pressure,
            characteristic_temperature = characteristic_temperature,
            characteristic_vapour_mass_fraction =
                characteristic_vapour_mass_fraction,
            output_element_matrices = output_element_matrices,
            material_property = material_property,
            diffusion_coeff_component_b = diffusion_coeff_component_b,
            diffusion_coeff_component_a = diffusion_coeff_component_a,
            hydro_crack_scheme = hydro_crack_scheme,
            at_num = at_num,
            initial_stress = initial_stress,
            split_method = split_method,
            reg_param = reg_param,
            thermal_parameters = thermal_parameters,
            non_advective_form = non_advective_form,
            fluid = fluid,
            porous_medium = porous_medium,
            decay_rate = decay_rate,
            fluid_reference_density = fluid_reference_density,
            retardation_factor = retardation_factor,
            solute_dispersivity_longitudinal = solute_dispersivity_longitudinal,
            solute_dispersivity_transverse = solute_dispersivity_transverse,
            molecular_diffusion_coefficient = molecular_diffusion_coefficient,
            density_solid = density_solid,
            latent_heat_evaporation = latent_heat_evaporation,
            pf_irrv = pf_irrv,
            tag_name = "process",
            is_subclass = FALSE,
            attr_names = c("secondary_variable"),
            flatten_on_exp = c("specific_body_force")
        ),

        class = "r2ogs6_process"
    )
}


#===== r2ogs6_fracture_model =====


#'r2ogs6_fracture_model
#'@description tag: fracture_model
#'@param type string:
#'@param normal_stiffness string:
#'@param shear_stiffness string:
#'@param penalty_aperture_cutoff string | double:
#'@param tension_cutoff string | double:
#'@param fracture_toughness Optional: string:
#'@param peak_normal_traction Optional: string:
#'@param friction_angle Optional: string:
#'@param dilatancy_angle Optional: string:
#'@param cohesion Optional: string:
#'@param nonlinear_solver Optional: list:
#'@export
r2ogs6_fracture_model <- function(type,
                                  normal_stiffness,
                                  shear_stiffness,
                                  penalty_aperture_cutoff,
                                  tension_cutoff,
                                  fracture_toughness = NULL,
                                  peak_normal_traction = NULL,
                                  friction_angle = NULL,
                                  dilatancy_angle = NULL,
                                  cohesion = NULL,
                                  nonlinear_solver = NULL) {

    #Coerce input
    penalty_aperture_cutoff <- coerce_string_to_numeric(penalty_aperture_cutoff)
    tension_cutoff <- coerce_string_to_numeric(tension_cutoff)

    new_r2ogs6_fracture_model(type,
                              normal_stiffness,
                              shear_stiffness,
                              penalty_aperture_cutoff,
                              tension_cutoff,
                              fracture_toughness,
                              peak_normal_traction,
                              friction_angle,
                              dilatancy_angle,
                              cohesion,
                              nonlinear_solver)
}


new_r2ogs6_fracture_model <- function(type,
                                      normal_stiffness,
                                      shear_stiffness,
                                      penalty_aperture_cutoff,
                                      tension_cutoff,
                                      fracture_toughness = NULL,
                                      peak_normal_traction = NULL,
                                      friction_angle = NULL,
                                      dilatancy_angle = NULL,
                                      cohesion = NULL,
                                      nonlinear_solver = NULL) {

    validate_is_string(type,
                       normal_stiffness,
                       shear_stiffness)

    validate_is_number(penalty_aperture_cutoff,
                       tension_cutoff)

    validate_is_null_or_string(fracture_toughness,
                               peak_normal_traction,
                               friction_angle,
                               dilatancy_angle,
                               cohesion)

    if(!is.null(nonlinear_solver)){
        nonlinear_solver <- validate_param_list(nonlinear_solver,
                                                c("maximum_iterations",
                                                  "error_tolerance"))
    }

    structure(list(type = type,
                   normal_stiffness = normal_stiffness,
                   shear_stiffness = shear_stiffness,
                   penalty_aperture_cutoff = penalty_aperture_cutoff,
                   tension_cutoff = tension_cutoff,
                   fracture_toughness = fracture_toughness,
                   peak_normal_traction = peak_normal_traction,
                   friction_angle = friction_angle,
                   dilatancy_angle = dilatancy_angle,
                   cohesion = cohesion,
                   nonlinear_solver = nonlinear_solver,
                   is_subclass = TRUE,
                   attr_names = character(),
                   flatten_on_exp = character()
    ),
    class = "r2ogs6_fracture_model"
    )
}


#===== r2ogs6_fracture_properties =====


#'r2ogs6_fracture_properties
#'@description tag: fracture_properties
#'@param material_id string | double:
#'@param initial_aperture string:
#'@param specific_storage Optional: string:
#'@param biot_coefficient Optional: string:
#'@param permeability_model Optional: list:
#'@export
r2ogs6_fracture_properties <- function(material_id,
                                       initial_aperture,
                                       specific_storage = NULL,
                                       biot_coefficient = NULL,
                                       permeability_model = NULL) {

    #Coerce input
    material_id <- coerce_string_to_numeric(material_id)

    new_r2ogs6_fracture_properties(material_id,
                                   initial_aperture,
                                   specific_storage,
                                   biot_coefficient,
                                   permeability_model)
}


new_r2ogs6_fracture_properties <- function(material_id,
                                           initial_aperture,
                                           specific_storage = NULL,
                                           biot_coefficient = NULL,
                                           permeability_model = NULL) {

    validate_is_string(initial_aperture)

    validate_is_number(material_id)

    validate_is_null_or_string(specific_storage,
                               biot_coefficient)

    if(!is.null(permeability_model)){
        assertthat::assert_that(any(is.list(permeability_model),
                                    is.character(permeability_model)))

        assertthat::assert_that(any(length(permeability_model) == 1,
                                    length(permeability_model) == 2))

        if(length(permeability_model == 1)){
            permeability_model <-
                validate_param_list(permeability_model, c("type"))
        }else{
            permeability_model <-
                validate_param_list(permeability_model, c("type",
                                                          "value"))
        }
    }

    structure(list(material_id = material_id,
                   initial_aperture = initial_aperture,
                   specific_storage = specific_storage,
                   biot_coefficient = biot_coefficient,
                   permeability_model = permeability_model,
                   is_subclass = TRUE,
                   attr_names = character(),
                   flatten_on_exp = character()
    ),
    class = "r2ogs6_fracture_properties"
    )
}


#===== r2ogs6_jacobian_assembler =====


#'r2ogs6_jacobian_assembler
#'@description tag: jacobian_assembler
#'@param type string:
#'@param component_magnitudes Optional: string | double:
#'@param relative_epsilons Optional: string | double:
#'@export
r2ogs6_jacobian_assembler <- function(type,
                                      component_magnitudes = NULL,
                                      relative_epsilons = NULL) {

    # Add coercing utility here
    component_magnitudes <- coerce_string_to_numeric(component_magnitudes)
    relative_epsilons <- coerce_string_to_numeric(relative_epsilons)

    new_r2ogs6_jacobian_assembler(type,
                                  component_magnitudes,
                                  relative_epsilons)
}


new_r2ogs6_jacobian_assembler <- function(type,
                                          component_magnitudes = NULL,
                                          relative_epsilons = NULL) {

    validate_is_string(type)
    validate_is_null_or_number(component_magnitudes,
                               relative_epsilons)

    structure(list(type = type,
                   component_magnitudes = component_magnitudes,
                   relative_epsilons = relative_epsilons,
                   is_subclass = TRUE,
                   attr_names = character(),
                   flatten_on_exp = character()
    ),
    class = "r2ogs6_jacobian_assembler"
    )
}


#===== r2ogs6_phasefield_parameters =====


#'r2ogs6_phasefield_parameters
#'@description tag: phasefield_parameters
#'@param residual_stiffness string:
#'@param crack_resistance string:
#'@param crack_length_scale string:
#'@param kinetic_coefficient string:
#'@param history_field Optional: string:
#'@export
r2ogs6_phasefield_parameters <- function(residual_stiffness,
                                         crack_resistance,
                                         crack_length_scale,
                                         kinetic_coefficient,
                                         history_field = NULL) {

    # Add coercing utility here

    new_r2ogs6_phasefield_parameters(residual_stiffness,
                                     crack_resistance,
                                     crack_length_scale,
                                     kinetic_coefficient,
                                     history_field)
}


new_r2ogs6_phasefield_parameters <- function(residual_stiffness,
                                             crack_resistance,
                                             crack_length_scale,
                                             kinetic_coefficient,
                                             history_field = NULL) {

    validate_is_string(residual_stiffness,
                       crack_resistance,
                       crack_length_scale,
                       kinetic_coefficient)

    validate_is_null_or_string(history_field)

    structure(list(residual_stiffness = residual_stiffness,
                   crack_resistance = crack_resistance,
                   crack_length_scale = crack_length_scale,
                   kinetic_coefficient = kinetic_coefficient,
                   history_field = history_field,
                   is_subclass = TRUE,
                   attr_names = character(),
                   flatten_on_exp = character()
    ),
    class = "r2ogs6_phasefield_parameters"
    )
}


#===== r2ogs6_borehole_heat_exchanger =====


#See prj_borehole_heat_exchanger.R


#===== r2ogs6_material_property =====


#See prj_material_property.R


#===== r2ogs6_porous_medium =====


#See prj_porous_medium.R


#===== validation utility =====


validate_process_variables <- function(process_variables){

    assertthat::assert_that(is.list(process_variables) ||
                            is.character(process_variables))

    valid_pv_names <- c("process_variable",
                        "pressure",
                        "displacement",
                        "temperature",
                        "concentration",
                        "phasefield",
                        "fluid_pressure",
                        "vapour_mass_fraction",
                        "gas_pressure",
                        "capillary_pressure",
                        "liquid_pressure",
                        "overall_mass_density")

    for(i in seq_len(length(process_variables))){
        assertthat::assert_that(names(process_variables)[[i]] %in%
                                    valid_pv_names)
    }

    return(invisible(process_variables))
}


validate_secondary_variables <- function(secondary_variables){

    if(length(clean_up_imported_list(list(secondary_variables))) == 0){
        return(invisible(NULL))
    }

    secondary_variables

    if(!is.null(secondary_variables)){

        assertthat::assert_that(is.list(secondary_variables))

        for(i in seq_len(length(secondary_variables))){

            assertthat::assert_that(
                any(length(secondary_variables[[i]]) == 1,
                    length(secondary_variables[[i]]) == 2))

            if(length(secondary_variables[[i]]) == 1){
                secondary_variables[[i]] <-
                    c(secondary_variables[[i]][[1]],
                      secondary_variables[[i]][[1]])
            }

            names(secondary_variables[[i]]) <- c("internal_name",
                                                 "output_name")
        }

        names(secondary_variables) <- rep("secondary_variable",
                                          length(secondary_variables))
    }

    return(invisible(secondary_variables))
}


validate_constitutive_relation <- function(constitutive_relation){

    if(!is.null(constitutive_relation)){
        assertthat::assert_that(is.list(constitutive_relation) ||
                                is.character(constitutive_relation))

        valid_cr_names <- c("type",
                            "youngs_modulus",
                            "poissons_ratio",
                            "nonlinear_solver",
                            "behaviour",
                            "material_properties",
                            "shear_modulus",
                            "bulk_modulus",
                            "kappa",
                            "beta",
                            "gamma",
                            "hardening_modulus",
                            "alpha",
                            "delta",
                            "eps",
                            "m",
                            "alphap",
                            "deltap",
                            "epsp",
                            "mp",
                            "betap",
                            "gammap",
                            "tangent_type",
                            "damage_properties",
                            "youngs_moduli",
                            "shear_moduli",
                            "poissons_ratios",
                            "a",
                            "n",
                            "sigma0",
                            "q",
                            "kelvin_shear_modulus",
                            "kelvin_viscosity",
                            "maxwell_shear_modulus",
                            "maxwell_bulk_modulus",
                            "maxwell_viscosity",
                            "dependency_parameter_mk",
                            "dependency_parameter_mvk",
                            "dependency_parameter_mvm")

        for(i in seq_len(length(constitutive_relation))){
            assertthat::assert_that(names(constitutive_relation)[[i]] %in%
                                        valid_cr_names)
        }
    }

    return(invisible(constitutive_relation))
}


validate_fluid <- function(fluid){

    if(!is.null(fluid)){

        fluid <- validate_param_list(fluid, c("density", "viscosity"))

        fluid[[1]] <- validate_param_list(fluid[[1]], c("type", "value"))
        fluid[[2]] <- validate_param_list(fluid[[2]], c("type", "value"))
    }

    return(invisible(fluid))
}
