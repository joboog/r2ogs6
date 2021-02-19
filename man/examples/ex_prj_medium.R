prj_medium(
    phases = list(
        phase = prj_phase(
            type = "Gas",
            properties = list(
                property = prj_ph_property(name = "viscosity",
                                              type = "Constant",
                                              value = 1e-05)
            )
        ),
        phase = prj_phase(
            type = "Solid",
            properties = list(
                property = prj_ph_property(name = "porosity",
                                              type = "Constant",
                                              value = 0.3)
            )
        )
    ),
    properties = list(
        property = prj_pr_property(name = "reference_temperature",
                                      type = "Constant",
                                      value = 293.15)
    )
)
