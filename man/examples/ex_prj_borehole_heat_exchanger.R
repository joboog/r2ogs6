r2ogs6_borehole_heat_exchanger(
    type = "1U",
    flow_and_temperature_control = r2ogs6_flow_and_temperature_control(
        type = "TemperatureCurveConstantFlow",
        flow_rate = 2e-04,
        temperature_curve = "inflow_temperature"
    ),
    borehole = list(length = "18.0",
                    diameter = "0.13"),
    grout = list(
        density = "2190.0",
        porosity = "0.0",
        heat_capacity = "1735.160",
        thermal_conductivity = "0.806"
    ),
    pipes = r2ogs6_pipes(
        longitudinal_dispersion_length = 0.001,
        inlet = list(
            diameter = " 0.013665",
            wall_thickness = "0.003035",
            wall_thermal_conductivity = "0.39"
        ),
        outlet = list(
            diameter = "0.013665",
            wall_thickness = "0.003035",
            wall_thermal_conductivity = "0.39"
        ),
        distance_between_pipes = 0.053
    ),
    refrigerant = list(
        density = "992.92",
        viscosity = "0.00067418",
        specific_heat_capacity = "4068",
        thermal_conductivity = "0.62863",
        reference_temperature = "298.15"
    )
)
