
#===== r2ogs6_borehole_heat_exchanger =====


#'r2ogs6_borehole_heat_exchanger
#'@description tag: borehole_heat_exchanger
#'@param type string:
#'@param flow_and_temperature_control r2ogs6_flow_and_temperature_control:
#'@param borehole list:
#'@param grout list:
#'@param pipes r2ogs6_pipes:
#'@param refrigerant list:
#'@param use_bhe_pipe_network Optional: string ("true" | "false"):
#'@export
r2ogs6_borehole_heat_exchanger <- function(type,
                                           flow_and_temperature_control,
                                           borehole,
                                           grout,
                                           pipes,
                                           refrigerant,
                                           use_bhe_pipe_network = NULL) {

    # Add coercing utility here

    new_r2ogs6_borehole_heat_exchanger(type,
                                       flow_and_temperature_control,
                                       borehole,
                                       grout,
                                       pipes,
                                       refrigerant,
                                       use_bhe_pipe_network)
}


new_r2ogs6_borehole_heat_exchanger <- function(type,
                                               flow_and_temperature_control,
                                               borehole,
                                               grout,
                                               pipes,
                                               refrigerant,
                                               use_bhe_pipe_network = NULL) {

    are_strings(type)
    assertthat::assert_that(class(flow_and_temperature_control) ==
                                "r2ogs6_flow_and_temperature_control")
    borehole <- coerce_names(borehole, c("length", "diameter"))
    grout <- coerce_names(grout, c("density",
                                          "porosity",
                                          "specific_heat_capacity",
                                          "thermal_conductivity"))

    assertthat::assert_that(class(pipes) == "r2ogs6_pipes")

    refrigerant <- coerce_names(refrigerant, c("density",
                                                      "viscosity",
                                                      "specific_heat_capacity",
                                                      "thermal_conductivity",
                                                      "reference_temperature"))
    are_null_or_string_flags(use_bhe_pipe_network)

    structure(list(type = type,
                   flow_and_temperature_control = flow_and_temperature_control,
                   borehole = borehole,
                   grout = grout,
                   pipes = pipes,
                   refrigerant = refrigerant,
                   use_bhe_pipe_network = use_bhe_pipe_network,
                   xpath = paste0("processes/process/borehole_heat_exchangers/",
                   "borehole_heat_exchanger"),
                   attr_names = character(),
                   flatten_on_exp = character()
    ),
    class = "r2ogs6_borehole_heat_exchanger"
    )
}


#===== r2ogs6_flow_and_temperature_control =====


#'r2ogs6_flow_and_temperature_control
#'@description tag: flow_and_temperature_control
#'@param type string:
#'@param flow_rate Optional: string | double:
#'@param temperature_curve Optional: string:
#'@param power Optional: string | double:
#'@param power_curve Optional: string:
#'@param flow_rate_curve Optional: string:
#'@export
r2ogs6_flow_and_temperature_control <- function(type,
                                                flow_rate = NULL,
                                                temperature_curve = NULL,
                                                power = NULL,
                                                power_curve = NULL,
                                                flow_rate_curve = NULL) {

    # Coerce input
    flow_rate <- coerce_string_to_numeric(flow_rate)
    power <- coerce_string_to_numeric(power)

    new_r2ogs6_flow_and_temperature_control(type,
                                            flow_rate,
                                            temperature_curve,
                                            power,
                                            power_curve,
                                            flow_rate_curve)
}


new_r2ogs6_flow_and_temperature_control <- function(type,
                                                    flow_rate = NULL,
                                                    temperature_curve = NULL,
                                                    power = NULL,
                                                    power_curve = NULL,
                                                    flow_rate_curve = NULL) {

    are_strings(type)

    are_null_or_strings(temperature_curve,
                               power_curve,
                               flow_rate_curve)

    are_null_or_numbers(flow_rate,
                               power)

    structure(list(type = type,
                   flow_rate = flow_rate,
                   temperature_curve = temperature_curve,
                   power = power,
                   power_curve = power_curve,
                   flow_rate_curve = flow_rate_curve,
                   xpath = paste0("processes/process/borehole_heat_exchangers/",
                                  "borehole_heat_exchanger/",
                                  "flow_and_temperature_control"),
                   attr_names = character(),
                   flatten_on_exp = character()
    ),
    class = "r2ogs6_flow_and_temperature_control"
    )
}


#===== r2ogs6_pipes =====


#'r2ogs6_pipes
#'@description tag: pipes
#'@param longitudinal_dispersion_length string | double:
#'@param inlet Optional: list:
#'@param outlet Optional: list:
#'@param distance_between_pipes Optional: string | double:
#'@param outer Optional: list:
#'@param inner Optional: list:
#'@export
r2ogs6_pipes <- function(longitudinal_dispersion_length,
                         inlet = NULL,
                         outlet = NULL,
                         distance_between_pipes = NULL,
                         outer = NULL,
                         inner = NULL) {

    # Coerce input
    longitudinal_dispersion_length <-
        coerce_string_to_numeric(longitudinal_dispersion_length)

    distance_between_pipes <-
        coerce_string_to_numeric(distance_between_pipes)

    new_r2ogs6_pipes(longitudinal_dispersion_length,
                     inlet,
                     outlet,
                     distance_between_pipes,
                     outer,
                     inner)
}


new_r2ogs6_pipes <- function(longitudinal_dispersion_length,
                             inlet = NULL,
                             outlet = NULL,
                             distance_between_pipes = NULL,
                             outer = NULL,
                             inner = NULL) {

    are_numbers(longitudinal_dispersion_length)

    inlet_outlet_names <- c("diameter",
                            "wall_thickness",
                            "wall_thermal_conductivity")

    inlet <- is_null_or_coerce_names(inlet, inlet_outlet_names)
    outlet <- is_null_or_coerce_names(outlet, inlet_outlet_names)

    are_null_or_numbers(distance_between_pipes)

    outer <- is_null_or_coerce_names(outer, inlet_outlet_names)
    inner <- is_null_or_coerce_names(inner, inlet_outlet_names)

    structure(list(longitudinal_dispersion_length =
                       longitudinal_dispersion_length,
                   inlet = inlet,
                   outlet = outlet,
                   distance_between_pipes = distance_between_pipes,
                   outer = outer,
                   inner = inner,
                   xpath = paste0("processes/process/borehole_heat_exchangers/",
                                  "borehole_heat_exchanger/pipes"),
                   attr_names = character(),
                   flatten_on_exp = character()
    ),
    class = "r2ogs6_pipes"
    )
}