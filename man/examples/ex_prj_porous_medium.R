prj_porous_medium(
    id = 0,
    permeability = list(permeability_tensor_entries = "kappa1",
                        type = "Constant"),
    porosity = list(type = "Constant",
                    porosity_parameter = "constant_porosity_parameter"),
    storage = list(type = "Constant",
                   value = " 0.0 "),
    capillary_pressure = prj_capillary_pressure(
        type = "vanGenuchten",
        pd = 2e+06,
        sr = 0.4,
        smax = 1,
        m = 0.3288590604,
        pc_max = 2e+07,
        has_regularized = "true"
    ),
    relative_permeability = list(
        relative_permeability = prj_relative_permeability(
            type = "NonWettingPhaseVanGenuchten",
            sr = 0,
            smax = 0.6,
            m = 0.3288590604,
            krel_min = 0,
            id = "0"
        )
    )
)
