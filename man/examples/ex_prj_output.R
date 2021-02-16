prj_output(
    type = "VTK",
    prefix = "flow_free_expansion",
    variables = list(
        variable = "displacement",
        variable = "pressure"
    ),
    suffix = "_ts_{:timestep}_t_{:time}",
    timesteps = list(pair = list(1,
                                 each_steps = 1000))
)
