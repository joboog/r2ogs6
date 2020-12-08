
#===== r2ogs6_material_property =====


#'r2ogs6_material_property
#'@description tag: material_property
#'@param fluid r2ogs6_fluid:
#'@param porous_medium r2ogs6_porous_medium:
#'@export
r2ogs6_material_property <- function(fluid,
                                     porous_medium) {

    # Add coercing utility here

    new_r2ogs6_material_property(fluid,
                                 porous_medium)
}


new_r2ogs6_material_property <- function(fluid,
                                         porous_medium) {

    assertthat::assert_that(class(fluid) == "r2ogs6_fluid")
    assertthat::assert_that(class(porous_medium) == "r2ogs6_porous_medium")

    structure(list(fluid = fluid,
                   porous_medium = porous_medium,
                   is_subclass = TRUE,
                   attr_names = character(),
                   flatten_on_exp = character()
    ),
    class = "r2ogs6_material_property"
    )
}


#===== r2ogs6_fluid =====


#'r2ogs6_fluid
#'@description tag: fluid
#'@param liquid_density list:
#'@param gas_density list:
#'@param liquid_viscosity list:
#'@param gas_viscosity list:
#'@param specific_heat_capacity_solid Optional:
#'@param specific_heat_capacity_water Optional:
#'@param specific_heat_capacity_air Optional:
#'@param specific_heat_capacity_water_vapor Optional:
#'@param thermal_conductivity_dry_solid Optional:
#'@param thermal_conductivity_wet_solid Optional:
#'@export
r2ogs6_fluid <- function(liquid_density,
                         gas_density,
                         liquid_viscosity,
                         gas_viscosity,
                         specific_heat_capacity_solid = NULL,
                         specific_heat_capacity_water = NULL,
                         specific_heat_capacity_air = NULL,
                         specific_heat_capacity_water_vapor = NULL,
                         thermal_conductivity_dry_solid = NULL,
                         thermal_conductivity_wet_solid = NULL) {

    # Add coercing utility here

    new_r2ogs6_fluid(liquid_density,
                     gas_density,
                     liquid_viscosity,
                     gas_viscosity,
                     specific_heat_capacity_solid,
                     specific_heat_capacity_water,
                     specific_heat_capacity_air,
                     specific_heat_capacity_water_vapor,
                     thermal_conductivity_dry_solid,
                     thermal_conductivity_wet_solid)
}


new_r2ogs6_fluid <- function(liquid_density,
                             gas_density,
                             liquid_viscosity,
                             gas_viscosity,
                             specific_heat_capacity_solid = NULL,
                             specific_heat_capacity_water = NULL,
                             specific_heat_capacity_air = NULL,
                             specific_heat_capacity_water_vapor = NULL,
                             thermal_conductivity_dry_solid = NULL,
                             thermal_conductivity_wet_solid = NULL) {


    type_value_names <- c("type", "value")

    liquid_density <- validate_param_list(liquid_density, type_value_names)

    gas_density <- validate_param_list(gas_density, c("type",
                                                      "molar_mass"))

    liquid_viscosity <- validate_param_list(liquid_viscosity, type_value_names)

    gas_viscosity <- validate_param_list(gas_viscosity, type_value_names)

    specific_heat_capacity_solid <-
        validate_param_list(specific_heat_capacity_solid, type_value_names)

    specific_heat_capacity_water <-
        validate_param_list(specific_heat_capacity_water, type_value_names)

    specific_heat_capacity_air <-
        validate_param_list(specific_heat_capacity_air, type_value_names)

    specific_heat_capacity_water_vapor <-
        validate_param_list(specific_heat_capacity_water_vapor,
                            type_value_names)

    thermal_conductivity_dry_solid <-
        validate_param_list(thermal_conductivity_dry_solid,
                            type_value_names)

    thermal_conductivity_wet_solid <-
        validate_param_list(thermal_conductivity_wet_solid,
                            type_value_names)

    structure(list(liquid_density = liquid_density,
                   gas_density = gas_density,
                   liquid_viscosity = liquid_viscosity,
                   gas_viscosity = gas_viscosity,
                   specific_heat_capacity_solid = specific_heat_capacity_solid,
                   specific_heat_capacity_water = specific_heat_capacity_water,
                   specific_heat_capacity_air = specific_heat_capacity_air,
                   specific_heat_capacity_water_vapor =
                       specific_heat_capacity_water_vapor,
                   thermal_conductivity_dry_solid =
                       thermal_conductivity_dry_solid,
                   thermal_conductivity_wet_solid =
                       thermal_conductivity_wet_solid,
                   is_subclass = TRUE,
                   attr_names = character(),
                   flatten_on_exp = character()
    ),
    class = "r2ogs6_fluid"
    )
}
