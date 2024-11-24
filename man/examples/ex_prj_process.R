prj_process(
    name = "HM",
    type = "HYDRO_MECHANICS",
    integration_order = 3,
    process_variables = list(displacement = "displacement",
                             pressure = "pressure"),
    secondary_variables = list(
        secondary_variable = c(internal_name = "sigma_xx", output_name = "sigma_xx"),
        secondary_variable = c(internal_name = "sigma_yy", output_name = "sigma_yy")
    ),
    specific_body_force = c(0, 0, 0),
    constitutive_relation = prj_constitutive_relation(
        type = "LinearElasticIsotropic",
        youngs_modulus = "E",
        poissons_ratio = "nu"
    )
)
