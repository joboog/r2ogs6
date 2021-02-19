prj_process_variable(
    name = "pressure",
    components = 1,
    order = 1,
    initial_condition = "pressure0",
    boundary_conditions = list(
        boundary_condition = prj_boundary_condition(
            type = "Neumann",
            parameter = "flux_in",
            geometrical_set = "cube_1x1x1_geometry",
            geometry = "left",
            component = 0
        )
    )
)
