r2ogs6_medium(
    phases = list(
        phase = r2ogs6_phase(
            type = "Gas",
            properties = list(
                property = r2ogs6_ph_property(name = "viscosity",
                                              type = "Constant",
                                              value = 1e-05)
            )
        ),
        phase = r2ogs6_phase(
            type = "Solid",
            properties = list(
                property = r2ogs6_ph_property(name = "porosity",
                                              type = "Constant",
                                              value = 0.3)
            )
        )
    ),
    properties = list(
        property = r2ogs6_pr_property(name = "reference_temperature",
                                      type = "Constant",
                                      value = 293.15)
    )
)
