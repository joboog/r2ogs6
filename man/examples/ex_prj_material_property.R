r2ogs6_material_property(
    fluid = r2ogs6_fluid(
        liquid_density = list(type = "Constant",
                              value = " 1.e3 "),
        gas_density = list(type = "IdealGasLaw",
                           molar_mass = " 0.002 "),
        liquid_viscosity = list(type = "Constant",
                                value = " 3.171e-11 "),
        gas_viscosity = list(type = "Constant",
                             value = " 2.8539e-13 ")
    ),
    porous_medium = list(
        porous_medium = r2ogs6_porous_medium(
            id = 0,
            permeability = list(permeability_tensor_entries = "kappa1",
                                type = "Constant"),
            porosity = list(type = "Constant",
                            porosity_parameter = "constant_porosity_parameter"),
            storage = list(type = "Constant",
                           value = " 0.0 "),
            capillary_pressure = r2ogs6_capillary_pressure(
                type = "vanGenuchten",
                pd = 2e+06,
                sr = 0.4,
                smax = 1,
                m = 0.3288590604,
                pc_max = 2e+07,
                has_regularized = "true"
            ),
            relative_permeability = list(
                relative_permeability = r2ogs6_relative_permeability(
                    type = "NonWettingPhaseVanGenuchten",
                    sr = 0,
                    smax = 0.6,
                    m = 0.3288590604,
                    krel_min = 0,
                    id = "0"
                )
            )
        )
    )
)
