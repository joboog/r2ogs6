
#===== prj_process =====


#' prj_process
#' @description tag: process (parent tag: processes)
#' @param name string:
#' @param type string:
#' @param integration_order string | double:
#' @param process_variables list, character:
#' @param secondary_variables Optional:
#' @param specific_body_force Optional:
#' @param solid_density Optional:
#' @param dimension Optional:
#' @param coupling_scheme Optional:
#' @param darcy_gravity Optional:
#' @param reference_temperature Optional:
#' @param fracture_model Optional:
#' @param jacobian_assembler Optional:
#' @param internal_length Optional:
#' @param mass_lumping Optional:
#' @param porosity Optional:
#' @param calculatesurfaceflux Optional:
#' @param intrinsic_permeability Optional:
#' @param specific_storage Optional:
#' @param fluid_viscosity Optional:
#' @param biot_coefficient Optional:
#' @param fluid_density Optional:
#' @param initial_effective_stress Optional:
#' @param initial_fracture_effective_stress Optional:
#' @param phasefield_parameters Optional:
#' @param deactivate_matrix_in_flow Optional:
#' @param borehole_heat_exchangers Optional:
#' @param temperature Optional:
#' @param reactive_system Optional:
#' @param fluid_specific_heat_source Optional:
#' @param fluid_specific_isobaric_heat_capacity Optional:
#' @param solid_hydraulic_permeability Optional:
#' @param solid_specific_heat_source Optional:
#' @param solid_heat_conductivity Optional:
#' @param solid_specific_isobaric_heat_capacity Optional:
#' @param tortuosity Optional:
#' @param diffusion_coefficient Optional:
#' @param solid_density_dry Optional:
#' @param solid_density_initial Optional:
#' @param characteristic_pressure Optional:
#' @param characteristic_temperature Optional:
#' @param characteristic_vapour_mass_fraction Optional:
#' @param output_element_matrices Optional:
#' @param material_property Optional:
#' @param diffusion_coeff_component_b Optional:
#' @param diffusion_coeff_component_a Optional:
#' @param hydro_crack_scheme Optional:
#' @param at_num Optional:
#' @param initial_stress Optional:
#' @param split_method Optional:
#' @param reg_param Optional:
#' @param thermal_parameters Optional:
#' @param non_advective_form Optional:
#' @param fluid Optional:
#' @param porous_medium Optional:
#' @param decay_rate Optional:
#' @param fluid_reference_density Optional:
#' @param retardation_factor Optional:
#' @param solute_dispersivity_longitudinal Optional:
#' @param solute_dispersivity_transverse Optional:
#' @param molecular_diffusion_coefficient Optional:
#' @param pf_irrv Optional:
#' @param micro_porosity Optional:
#' @param explicit_hm_coupling_in_unsaturated_zone Optional:
#' @param simplified_elasticity Optional: character
#' @param chemically_induced_porosity_change Optional: character
#' @param use_server_communication Optional:
#' @param phasefield_model Optional:
#' @param irreversible_threshold Optional: numeric
#' @param tabular_file Optional: character
#' @param temperature_field Optional: character
#' @param use_stokes_brinkman_form Optional: character
#' @param energy_split_model Optional: string
#' @param softening_curve Optional: string
#' @param characteristic_length Optional: double
#' @param coupling_scheme_parameter Optional: double
#' @param numerical_stabilization Optional: list
#' @param aperture_size Optional: list
#' @param ... Optional: fracture_properties, constitutive_relation
#' @example man/examples/ex_prj_process.R
#' @export
prj_process <- function(name,
                           type,
                           integration_order,
                           process_variables,
                           secondary_variables = NULL,
                           specific_body_force = NULL,
                           solid_density = NULL,
                           dimension = NULL,
                           coupling_scheme = NULL,
                           darcy_gravity = NULL,
                           reference_temperature = NULL,
                           fracture_model = NULL,
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
                           pf_irrv = NULL,
                           micro_porosity = NULL,
                           explicit_hm_coupling_in_unsaturated_zone = NULL,
                           simplified_elasticity = NULL,
                           chemically_induced_porosity_change = NULL,
                           use_server_communication = NULL,
                           phasefield_model = NULL,
                           irreversible_threshold = NULL,
                           tabular_file = NULL,
                           temperature_field = NULL,
                           use_stokes_brinkman_form = NULL,
                           energy_split_model = NULL,
                           softening_curve = NULL,
                           characteristic_length = NULL,
                           coupling_scheme_parameter = NULL,
                           numerical_stabilization = NULL,
                           aperture_size = NULL,
                           ...){

    #Coerce input
    integration_order <- coerce_string_to_numeric(integration_order)
    specific_body_force <- coerce_string_to_numeric(specific_body_force)
    dimension <- coerce_string_to_numeric(dimension)
    internal_length <- coerce_string_to_numeric(internal_length)
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
    at_num <- coerce_string_to_numeric(at_num)
    split_method <- coerce_string_to_numeric(split_method)
    reg_param <- coerce_string_to_numeric(reg_param)
    pf_irrv <- coerce_string_to_numeric(pf_irrv)
    irreversible_threshold <- coerce_string_to_numeric(irreversible_threshold)
    characteristic_length <- coerce_string_to_numeric(characteristic_length)
    coupling_scheme_parameter <-
        coerce_string_to_numeric(coupling_scheme_parameter)

    ellipsis_list <- list(...)

    fracture_properties <-
        ellipsis_list[names(ellipsis_list) == "fracture_properties"]

    constitutive_relation <-
        ellipsis_list[names(ellipsis_list) == "constitutive_relation"]

    new_prj_process(
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
        pf_irrv,
        micro_porosity,
        explicit_hm_coupling_in_unsaturated_zone,
        simplified_elasticity,
        chemically_induced_porosity_change,
        use_server_communication,
        phasefield_model,
        irreversible_threshold,
        tabular_file,
        temperature_field,
        use_stokes_brinkman_form,
        energy_split_model,
        softening_curve,
        characteristic_length,
        coupling_scheme_parameter,
        numerical_stabilization,
        aperture_size
    )
}


new_prj_process <- function(name,
                            type,
                            integration_order,
                            process_variables,
                            secondary_variables = NULL,
                            specific_body_force = NULL,
                            constitutive_relation = NULL,
                            solid_density = NULL,
                            dimension = NULL,
                            coupling_scheme = NULL,
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
                            pf_irrv = NULL,
                            micro_porosity = NULL,
                            explicit_hm_coupling_in_unsaturated_zone = NULL,
                            simplified_elasticity = NULL,
                            chemically_induced_porosity_change = NULL,
                            use_server_communication = NULL,
                            phasefield_model = NULL,
                            irreversible_threshold = NULL,
                            tabular_file = NULL,
                            temperature_field = NULL,
                            use_stokes_brinkman_form = NULL,
                            energy_split_model = NULL,
                            softening_curve = NULL,
                            characteristic_length = NULL,
                            coupling_scheme_parameter = NULL,
                            numerical_stabilization = NULL,
                            aperture_size = NULL) {

    #Basic validation
    assertthat::assert_that(assertthat::is.string(name))
    assertthat::assert_that(assertthat::is.string(type))
    assertthat::assert_that(assertthat::is.number(integration_order))

    is_null_or_has_class(material_property,
                                  "prj_material_property")

    process_variables <- validate_process_variables(process_variables)
    secondary_variables <- validate_secondary_variables(secondary_variables)

    are_null_or_numeric(specific_body_force)

    if(!is.null(constitutive_relation)){
        is_wrapper_list(constitutive_relation,
                        "prj_constitutive_relation")
    }

    if(!is.null(darcy_gravity)){
        darcy_gravity <- coerce_names(darcy_gravity,
                                             c("axis_id", "g"))
    }

    is_null_or_has_class(fracture_model, "prj_fracture_model")

    is_null_or_has_class(phasefield_parameters,
                                  "prj_phasefield_parameters")

    is_null_or_has_class(micro_porosity,
                         "prj_micro_porosity")

    if(!is.null(calculatesurfaceflux)){
        if(length(calculatesurfaceflux) == 2){
            calculatesurfaceflux <- coerce_names(calculatesurfaceflux,
                                                 c("mesh", "property_name"))
        }else if(length(calculatesurfaceflux) == 3){
            calculatesurfaceflux <- coerce_names(calculatesurfaceflux,
                                                 c("mesh",
                                                   "property_name",
                                                   "output_mesh"))
        }

    }

    if(!is.null(borehole_heat_exchangers)){
        is_wrapper_list(borehole_heat_exchangers,
                              "prj_borehole_heat_exchanger")
    }

    if(!is.null(reactive_system)){
        reactive_system <- coerce_names(reactive_system, c("type"))
    }

    if(!is.null(thermal_parameters)){
        thermal_parameters <-
            coerce_names(thermal_parameters,
                                c("linear_thermal_expansion_coefficient",
                                  "specific_heat_capacity",
                                  "thermal_conductivity",
                                  "residual_thermal_conductivity"))
    }

    if(!is.null(porous_medium)){
        is_wrapper_list(porous_medium,
                        "prj_porous_medium")
    }

    fluid <- validate_fluid(fluid)


    are_null_or_strings(coupling_scheme,
                       solid_density,
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
                       porosity,
                       deactivate_matrix_in_flow,
                       output_element_matrices,
                       reference_temperature,
                       simplified_elasticity,
                       chemically_induced_porosity_change,
                       use_server_communication,
                       tabular_file,
                       temperature_field,
                       use_stokes_brinkman_form,
                       energy_split_model,
                       softening_curve)


    are_null_or_numbers(dimension,
                       internal_length,
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
                       at_num,
                       split_method,
                       reg_param,
                       pf_irrv,
                       irreversible_threshold,
                       characteristic_length,
                       coupling_scheme_parameter)

    if(!is.null(mass_lumping)){
        mass_lumping <- stringr::str_remove_all(mass_lumping, "[:space:]*")
    }

    are_null_or_string_flags(mass_lumping,
                                 non_advective_form)

    assertthat::assert_that(is.null(numerical_stabilization) |
                                is.list(numerical_stabilization))

    assertthat::assert_that(is.null(aperture_size) |
                                is.list(aperture_size))

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
            pf_irrv = pf_irrv,
            micro_porosity = micro_porosity,
            explicit_hm_coupling_in_unsaturated_zone =
                explicit_hm_coupling_in_unsaturated_zone,
            simplified_elasticity = simplified_elasticity,
            chemically_induced_porosity_change =
                chemically_induced_porosity_change,
            use_server_communication = use_server_communication,
            phasefield_model = phasefield_model,
            irreversible_threshold = irreversible_threshold,
            tabular_file = tabular_file,
            temperature_field = temperature_field,
            use_stokes_brinkman_form = use_stokes_brinkman_form,
            energy_split_model = energy_split_model,
            softening_curve = softening_curve,
            characteristic_length = characteristic_length,
            coupling_scheme_parameter = coupling_scheme_parameter,
            numerical_stabilization = numerical_stabilization,
            aperture_size = aperture_size,
            xpath = "processes/process",
            attr_names = c("secondary_variable"),
            flatten_on_exp = c("specific_body_force"),
            unwrap_on_exp = c("fracture_properties", "constitutive_relation")
        ),

        class = "prj_process"
    )
}


#===== prj_constitutive_relation =====


#' prj_constitutive_relation
#' @description tag: constitutive_relation
#' @param type string:
#' @param id Optional:
#' @param youngs_modulus Optional:
#' @param poissons_ratio Optional:
#' @param nonlinear_solver Optional:
#' @param behaviour Optional:
#' @param material_properties Optional:
#' @param shear_modulus Optional:
#' @param bulk_modulus Optional:
#' @param kappa Optional:
#' @param beta Optional:
#' @param gamma Optional:
#' @param hardening_modulus Optional:
#' @param alpha Optional:
#' @param delta Optional:
#' @param eps Optional:
#' @param m Optional:
#' @param alphap Optional:
#' @param deltap Optional:
#' @param epsp Optional:
#' @param mp Optional:
#' @param betap Optional:
#' @param gammap Optional:
#' @param tangent_type Optional:
#' @param damage_properties Optional:
#' @param youngs_moduli Optional:
#' @param shear_moduli Optional:
#' @param poissons_ratios Optional:
#' @param a Optional:
#' @param n Optional:
#' @param sigma0 Optional:
#' @param q Optional:
#' @param kelvin_shear_modulus Optional:
#' @param kelvin_viscosity Optional:
#' @param maxwell_shear_modulus Optional:
#' @param maxwell_bulk_modulus Optional:
#' @param maxwell_viscosity Optional:
#' @param dependency_parameter_mk Optional:
#' @param dependency_parameter_mvk Optional:
#' @param dependency_parameter_mvm Optional:
#' @example man/examples/ex_prj_constitutive_relation.R
#' @export
prj_constitutive_relation <- function(type,
                                         id = NULL,
                                         youngs_modulus = NULL,
                                         poissons_ratio = NULL,
                                         nonlinear_solver = NULL,
                                         behaviour = NULL,
                                         material_properties = NULL,
                                         shear_modulus = NULL,
                                         bulk_modulus = NULL,
                                         kappa = NULL,
                                         beta = NULL,
                                         gamma = NULL,
                                         hardening_modulus = NULL,
                                         alpha = NULL,
                                         delta = NULL,
                                         eps = NULL,
                                         m = NULL,
                                         alphap = NULL,
                                         deltap = NULL,
                                         epsp = NULL,
                                         mp = NULL,
                                         betap = NULL,
                                         gammap = NULL,
                                         tangent_type = NULL,
                                         damage_properties = NULL,
                                         youngs_moduli = NULL,
                                         shear_moduli = NULL,
                                         poissons_ratios = NULL,
                                         a = NULL,
                                         n = NULL,
                                         sigma0 = NULL,
                                         q = NULL,
                                         kelvin_shear_modulus = NULL,
                                         kelvin_viscosity = NULL,
                                         maxwell_shear_modulus = NULL,
                                         maxwell_bulk_modulus = NULL,
                                         maxwell_viscosity = NULL,
                                         dependency_parameter_mk = NULL,
                                         dependency_parameter_mvk = NULL,
                                         dependency_parameter_mvm = NULL) {

    # Add coercing utility here
    nonlinear_solver <- validate_process_nls(nonlinear_solver)

    new_prj_constitutive_relation(type,
                                     id,
                                     youngs_modulus,
                                     poissons_ratio,
                                     nonlinear_solver,
                                     behaviour,
                                     material_properties,
                                     shear_modulus,
                                     bulk_modulus,
                                     kappa,
                                     beta,
                                     gamma,
                                     hardening_modulus,
                                     alpha,
                                     delta,
                                     eps,
                                     m,
                                     alphap,
                                     deltap,
                                     epsp,
                                     mp,
                                     betap,
                                     gammap,
                                     tangent_type,
                                     damage_properties,
                                     youngs_moduli,
                                     shear_moduli,
                                     poissons_ratios,
                                     a,
                                     n,
                                     sigma0,
                                     q,
                                     kelvin_shear_modulus,
                                     kelvin_viscosity,
                                     maxwell_shear_modulus,
                                     maxwell_bulk_modulus,
                                     maxwell_viscosity,
                                     dependency_parameter_mk,
                                     dependency_parameter_mvk,
                                     dependency_parameter_mvm)
}


new_prj_constitutive_relation <- function(type,
                                             id = NULL,
                                             youngs_modulus = NULL,
                                             poissons_ratio = NULL,
                                             nonlinear_solver = NULL,
                                             behaviour = NULL,
                                             material_properties = NULL,
                                             shear_modulus = NULL,
                                             bulk_modulus = NULL,
                                             kappa = NULL,
                                             beta = NULL,
                                             gamma = NULL,
                                             hardening_modulus = NULL,
                                             alpha = NULL,
                                             delta = NULL,
                                             eps = NULL,
                                             m = NULL,
                                             alphap = NULL,
                                             deltap = NULL,
                                             epsp = NULL,
                                             mp = NULL,
                                             betap = NULL,
                                             gammap = NULL,
                                             tangent_type = NULL,
                                             damage_properties = NULL,
                                             youngs_moduli = NULL,
                                             shear_moduli = NULL,
                                             poissons_ratios = NULL,
                                             a = NULL,
                                             n = NULL,
                                             sigma0 = NULL,
                                             q = NULL,
                                             kelvin_shear_modulus = NULL,
                                             kelvin_viscosity = NULL,
                                             maxwell_shear_modulus = NULL,
                                             maxwell_bulk_modulus = NULL,
                                             maxwell_viscosity = NULL,
                                             dependency_parameter_mk = NULL,
                                             dependency_parameter_mvk = NULL,
                                             dependency_parameter_mvm = NULL) {
    structure(list(type = type,
                   id = id,
                   youngs_modulus = youngs_modulus,
                   poissons_ratio = poissons_ratio,
                   nonlinear_solver = nonlinear_solver,
                   behaviour = behaviour,
                   material_properties = material_properties,
                   shear_modulus = shear_modulus,
                   bulk_modulus = bulk_modulus,
                   kappa = kappa,
                   beta = beta,
                   gamma = gamma,
                   hardening_modulus = hardening_modulus,
                   alpha = alpha,
                   delta = delta,
                   eps = eps,
                   m = m,
                   alphap = alphap,
                   deltap = deltap,
                   epsp = epsp,
                   mp = mp,
                   betap = betap,
                   gammap = gammap,
                   tangent_type = tangent_type,
                   damage_properties = damage_properties,
                   youngs_moduli = youngs_moduli,
                   shear_moduli = shear_moduli,
                   poissons_ratios = poissons_ratios,
                   a = a,
                   n = n,
                   sigma0 = sigma0,
                   q = q,
                   kelvin_shear_modulus = kelvin_shear_modulus,
                   kelvin_viscosity = kelvin_viscosity,
                   maxwell_shear_modulus = maxwell_shear_modulus,
                   maxwell_bulk_modulus = maxwell_bulk_modulus,
                   maxwell_viscosity = maxwell_viscosity,
                   dependency_parameter_mk = dependency_parameter_mk,
                   dependency_parameter_mvk = dependency_parameter_mvk,
                   dependency_parameter_mvm = dependency_parameter_mvm,
                   xpath = "processes/process/constitutive_relation",
                   attr_names = c("id", "material_property"),
                   flatten_on_exp = character()
    ),
    class = "prj_constitutive_relation"
    )
}


#===== prj_fracture_model =====


#' prj_fracture_model
#' @description tag: fracture_model
#' @param type string:
#' @param normal_stiffness string:
#' @param shear_stiffness string:
#' @param penalty_aperture_cutoff string | double:
#' @param tension_cutoff string | double:
#' @param fracture_toughness Optional: string:
#' @param peak_normal_traction Optional: string:
#' @param friction_angle Optional: string:
#' @param dilatancy_angle Optional: string:
#' @param cohesion Optional: string:
#' @param nonlinear_solver Optional: list:
#' @example man/examples/ex_prj_fracture_model.R
#' @export
prj_fracture_model <- function(type,
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

    new_prj_fracture_model(type,
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


new_prj_fracture_model <- function(type,
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

    are_strings(type,
                       normal_stiffness,
                       shear_stiffness)

    are_numbers(penalty_aperture_cutoff,
                       tension_cutoff)

    are_null_or_strings(fracture_toughness,
                               peak_normal_traction,
                               friction_angle,
                               dilatancy_angle,
                               cohesion)

    nonlinear_solver <- validate_process_nls(nonlinear_solver)

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
                   xpath = "processes/process/fracture_model",
                   attr_names = character(),
                   flatten_on_exp = character()
    ),
    class = "prj_fracture_model"
    )
}


#===== prj_fracture_properties =====


#' prj_fracture_properties
#' @description tag: fracture_properties
#' @param material_id string | double:
#' @param initial_aperture string:
#' @param specific_storage Optional: string:
#' @param biot_coefficient Optional: string:
#' @param permeability_model Optional: list:
#' @example man/examples/ex_prj_fracture_properties.R
#' @export
prj_fracture_properties <- function(material_id,
                                       initial_aperture,
                                       specific_storage = NULL,
                                       biot_coefficient = NULL,
                                       permeability_model = NULL) {

    #Coerce input
    material_id <- coerce_string_to_numeric(material_id)

    new_prj_fracture_properties(material_id,
                                   initial_aperture,
                                   specific_storage,
                                   biot_coefficient,
                                   permeability_model)
}


new_prj_fracture_properties <- function(material_id,
                                           initial_aperture,
                                           specific_storage = NULL,
                                           biot_coefficient = NULL,
                                           permeability_model = NULL) {

    are_strings(initial_aperture)

    are_numbers(material_id)

    are_null_or_strings(specific_storage,
                               biot_coefficient)



    if(!is.null(permeability_model)){
        assertthat::assert_that(any(is.list(permeability_model),
                                    is.character(permeability_model)))

        assertthat::assert_that(length(permeability_model) == 1 ||
                                    length(permeability_model) == 2)

        if(length(permeability_model) == 1){
            permeability_model <-
                coerce_names(permeability_model, c("type"))
        }else{
            permeability_model <-
                coerce_names(permeability_model, c("type",
                                                          "value"))
        }
    }

    structure(list(material_id = material_id,
                   initial_aperture = initial_aperture,
                   specific_storage = specific_storage,
                   biot_coefficient = biot_coefficient,
                   permeability_model = permeability_model,
                   xpath = "processes/process/fracture_properties",
                   attr_names = character(),
                   flatten_on_exp = character()
    ),
    class = "prj_fracture_properties"
    )
}


#===== prj_jacobian_assembler =====


#' prj_jacobian_assembler
#' @description tag: jacobian_assembler
#' @param type string:
#' @param component_magnitudes Optional: string | double:
#' @param relative_epsilons Optional: string | double:
#' @example man/examples/ex_prj_jacobian_assembler.R
#' @export
prj_jacobian_assembler <- function(type,
                                      component_magnitudes = NULL,
                                      relative_epsilons = NULL) {

    # Add coercing utility here
    component_magnitudes <- coerce_string_to_numeric(component_magnitudes)
    relative_epsilons <- coerce_string_to_numeric(relative_epsilons)

    new_prj_jacobian_assembler(type,
                                  component_magnitudes,
                                  relative_epsilons)
}


new_prj_jacobian_assembler <- function(type,
                                          component_magnitudes = NULL,
                                          relative_epsilons = NULL) {

    are_strings(type)

    are_null_or_numeric(component_magnitudes,
                                relative_epsilons)

    structure(list(type = type,
                   component_magnitudes = component_magnitudes,
                   relative_epsilons = relative_epsilons,
                   xpath = "processes/process/jacobian_assembler",
                   attr_names = character(),
                   flatten_on_exp = c("component_magnitudes",
                                      "relative_epsilons")
    ),
    class = "prj_jacobian_assembler"
    )
}


#===== prj_phasefield_parameters =====


#' prj_phasefield_parameters
#' @description tag: phasefield_parameters
#' @param residual_stiffness string:
#' @param crack_resistance string:
#' @param crack_length_scale string:
#' @param kinetic_coefficient string:
#' @param history_field Optional: string:
#' @example man/examples/ex_prj_phasefield_parameters.R
#' @export
prj_phasefield_parameters <- function(residual_stiffness,
                                         crack_resistance,
                                         crack_length_scale,
                                         kinetic_coefficient = NULL,
                                         history_field = NULL) {

    # Add coercing utility here

    new_prj_phasefield_parameters(residual_stiffness,
                                     crack_resistance,
                                     crack_length_scale,
                                     kinetic_coefficient,
                                     history_field)
}


new_prj_phasefield_parameters <- function(residual_stiffness,
                                             crack_resistance,
                                             crack_length_scale,
                                             kinetic_coefficient = NULL,
                                             history_field = NULL) {

    are_strings(residual_stiffness,
                       crack_resistance,
                       crack_length_scale)

    are_null_or_strings(kinetic_coefficient,
                        history_field)

    structure(list(residual_stiffness = residual_stiffness,
                   crack_resistance = crack_resistance,
                   crack_length_scale = crack_length_scale,
                   kinetic_coefficient = kinetic_coefficient,
                   history_field = history_field,
                   xpath = "processes/process/phasefield_parameters",
                   attr_names = character(),
                   flatten_on_exp = character()
    ),
    class = "prj_phasefield_parameters"
    )
}


#===== prj_borehole_heat_exchanger =====


#See prj_borehole_heat_exchanger.R


#===== prj_material_property =====


#See prj_material_property.R


#===== prj_porous_medium =====


#See prj_porous_medium.R


#===== prj_micro_porosity =====


#' prj_micro_porosity
#' @description tag: micro_porosity
#' @param mass_exchange_coefficient Required:
#' @param nonlinear_solver Required:
#' @export
prj_micro_porosity <- function(mass_exchange_coefficient,
                               nonlinear_solver) {

    # Add coercing utility here
    nonlinear_solver <- validate_process_nls(nonlinear_solver, nullable = F)

    new_prj_micro_porosity(mass_exchange_coefficient,
                           nonlinear_solver)
}


new_prj_micro_porosity <- function(mass_exchange_coefficient,
                                   nonlinear_solver) {
    structure(list(mass_exchange_coefficient = mass_exchange_coefficient,
                   nonlinear_solver = nonlinear_solver,
                   xpath = "processes/process/micro_porosity",
                   attr_names = c(),
                   flatten_on_exp = character()
    ),
    class = "prj_micro_porosity"
    )
}


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
                        "liquid_velocity",
                        "overall_mass_density",
                        "total_molar_fraction_contaminant")

    for(i in seq_len(length(process_variables))){
        assertthat::assert_that(names(process_variables)[[i]] %in%
                                    valid_pv_names)
    }

    return(invisible(process_variables))
}


validate_secondary_variables <- function(secondary_variables){

    if(is.character(secondary_variables) ||
       is.null(secondary_variables)){
        return(invisible(NULL))
    }

    secondary_variables <-
        clean_imported_list(secondary_variables)


    for (i in seq_len(length(secondary_variables))) {
        assertthat::assert_that(length(secondary_variables[[i]]) == 2 ||
                                    length(secondary_variables[[i]]) == 3)

        if (length(secondary_variables[[i]]) == 2) {
            secondary_variables[[i]] <-
                coerce_names(secondary_variables[[i]],
                                    c("internal_name",
                                      "output_name"))
        }else{
            secondary_variables[[i]] <-
                coerce_names(secondary_variables[[i]],
                                    c("type",
                                      "internal_name",
                                      "output_name"))
        }
    }

    names(secondary_variables) <- rep("secondary_variable",
                                      length(secondary_variables))

    return(invisible(secondary_variables))
}


validate_fluid <- function(fluid){

    if(!is.null(fluid)){

        fluid <- coerce_names(fluid, c("density", "viscosity"))

        fluid[[1]] <- coerce_names(fluid[[1]], c("type", "value"))
        fluid[[2]] <- coerce_names(fluid[[2]], c("type", "value"))
    }

    return(invisible(fluid))
}

validate_process_nls <- function(nonlinear_solver, nullable = T){

    nls_names <- c("maximum_iterations",
               "residuum_tolerance",
               "increment_tolerance")

    if(nullable){
        nonlinear_solver <-
            is_null_or_coerce_names(nonlinear_solver, nls_names)
    }else{
        nonlinear_solver <- coerce_names(nonlinear_solver, nls_names)
    }

    return(invisible(nonlinear_solver))
}
