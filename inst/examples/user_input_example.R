library(r2ogs6)

ogs6_obj <- OGS6$new(
    sim_name = flow_free_expansion,
    sim_id = 1,
    sim_path = "D:\\OGS_Sim\\",
    ogs_bin_path = "D:\\Programme\\OpenGeoSys\\ogs-6.3.2-Windows-10.0.14393-x64-python-3.7.2-de-utils\\bin\\"
)


pick_vtu_file(ogs6_obj)


ogs6_obj$add_gml(
    r2ogs6_gml(
        name = "cube_1x1x1_geometry",
        points = tibble::tibble(
            x = c(0, 0, 0, 0, 1, 1, 1, 1),
            y = c(0, 0, 1, 1, 0, 0, 1, 1),
            z = c(0, 1, 1, 0, 0, 1, 1, 0),
            name = c("origin", "", "", "", "", "", "", "")
        ),
        polylines = list(
            list("front_left",
                 c(0, 1)),
            list("front_right",
                 c(4, 5)),
            list("front_bottom",
                 c(0, 4)),
            list("front_top",
                 c(1, 5)),
            list("bottom_left",
                 c(0, 3)),
            list("bottom_right",
                 c(4, 7)),
            list("top_left",
                 c(1, 2)),
            list("top_right",
                 c(5, 6)),
            list("back_left",
                 c(2, 3)),
            list("back_right",
                 c(6, 7)),
            list("back_bottom",
                 c(3, 7)),
            list("back_top",
                 c(2, 6))
        ),
        surfaces = list(
            list("left",
                 c(0, 1, 2),
                 c(0, 3, 2)),
            list("right",
                 c(4, 6, 5),
                 c(4, 6, 7)),
            list("top",
                 c(1, 2, 5),
                 c(5, 2, 6)),
            list("bottom",
                 c(0, 3, 4),
                 c(4, 3, 7)),
            list("front",
                 c(0, 1, 4),
                 c(4, 1, 5)),
            list("back",
                 c(2, 3, 6),
                 c(6, 3, 7))
        )
    )
)


ogs6_obj$add_process(
    r2ogs6_process(
        name = "HM",
        type = "HYDRO_MECHANICS",
        integration_order = 3,
        dimension = 3,
        constitutive_relation = list(
            type = "LinearElasticIsotropic",
            youngs_modulus = "E",
            poissons_ratio = "nu"
        ),
        process_variables = list(displacement = "displacement",
                                 pressure = "pressure"),
        secondary_variables = list(
            secondary_variable = c(internal_name = "sigma_xx", output_name = "sigma_xx"),
            secondary_variable = c(internal_name = "sigma_yy", output_name = "sigma_yy"),
            secondary_variable = c(internal_name = "sigma_zz", output_name = "sigma_zz"),
            secondary_variable = c(internal_name = "sigma_xy", output_name = "sigma_xy"),
            secondary_variable = c(internal_name = "epsilon_xx", output_name = "epsilon_xx"),
            secondary_variable = c(internal_name = "epsilon_yy", output_name = "epsilon_yy"),
            secondary_variable = c(internal_name = "epsilon_zz", output_name = "epsilon_zz"),
            secondary_variable = c(internal_name = "epsilon_xy", output_name = "epsilon_xy"),
            secondary_variable = c(internal_name = "velocity", output_name = "velocity")
        ),
        specific_body_force = c(0, 0, 0),
        coupling_scheme = NULL
    )
)


ogs6_obj$add_medium(r2ogs6_medium(
    phases = list(
        r2ogs6_medium_phase(
            type = "Gas",
            properties = list(
                r2ogs6_medium_property(
                    name = "viscosity",
                    type = "Constant",
                    value = 1e-05,
                    ... = NULL
                ),
                r2ogs6_medium_property(
                    name = "density",
                    type = "IdealGasLaw",
                    value = NULL,
                    ... = NULL
                ),
                r2ogs6_medium_property(
                    name = "molar_mass",
                    type = "Constant",
                    value = 0.0289643977872068,
                    ... = NULL
                )
            )
        ),
        r2ogs6_medium_phase(
            type = "Solid",
            properties = list(
                r2ogs6_medium_property(
                    name = "porosity",
                    type = "Constant",
                    value = 0.3,
                    ... = NULL
                ),
                r2ogs6_medium_property(
                    name = "density",
                    type = "Constant",
                    value = 1430,
                    ... = NULL
                ),
                r2ogs6_medium_property(
                    name = "biot_coefficient",
                    type = "Constant",
                    value = 0.6,
                    ... = NULL
                )
            )
        )
    ),
    properties = list(
        r2ogs6_medium_property(
            name = "reference_temperature",
            type = "Constant",
            value = 293.15,
            ... = NULL
        ),
        r2ogs6_medium_property(
            name = "permeability",
            type = "Constant",
            value = 1e-05,
            ... = NULL
        )
    ),
    id = NULL
))


ogs6_obj$add_time_loop(
    r2ogs6_time_loop(
        processes = list(
            r2ogs6_tl_process(
                ref = "HM",
                nonlinear_solver = "basic_newton",
                convergence_criterion = list(
                    type = "DeltaX",
                    norm_type = "NORM2",
                    reltol = "1e-8"
                ),
                time_discretization = list(type = "BackwardEuler"),
                time_stepping = list(
                    type = "FixedTimeStepping",
                    t_initial = 0,
                    t_end = 10000,
                    timesteps = list(pair = c(`repeat` = 1000, delta_t = 10))
                )
            )
        ),
        output = r2ogs6_tl_output(
            type = "VTK",
            prefix = "flow_free_expansion_pcs_{:process_id}",
            suffix = "_ts_{:timestep}_t_{:time}",
            timesteps = list(pair = c(
                `repeat` = 1, each_steps = 1000
            )),
            variables = list(
                variable = "displacement",
                variable = "pressure",
                variable = "sigma_xx",
                variable = "sigma_yy",
                variable = "sigma_zz",
                variable = "sigma_xy",
                variable = "epsilon_xx",
                variable = "epsilon_yy",
                variable = "epsilon_zz",
                variable = "epsilon_xy",
                variable = "velocity"
            ),
            compress_output = NULL
        ),
        global_processes_coupling = NULL
    )
)


ogs6_obj$add_parameter(r2ogs6_parameter(
    name = "E",
    type = "Constant",
    values = 1e+10
))


ogs6_obj$add_parameter(r2ogs6_parameter(
    name = "nu",
    type = "Constant",
    values = 0.3
))


ogs6_obj$add_parameter(r2ogs6_parameter(
    name = "displacement0",
    type = "Constant",
    values = c(0, 0, 0)
))


ogs6_obj$add_parameter(r2ogs6_parameter(
    name = "pressure0",
    type = "Constant",
    values = 1e+05
))


ogs6_obj$add_parameter(r2ogs6_parameter(
    name = "pressure_load",
    type = "Constant",
    values = -60000
))


ogs6_obj$add_parameter(r2ogs6_parameter(
    name = "zero",
    type = "Constant",
    values = 0
))


ogs6_obj$add_parameter(r2ogs6_parameter(
    name = "flux_in",
    type = "Constant",
    values = 1e-04
))


ogs6_obj$add_process_variable(
    r2ogs6_process_variable(
        name = "displacement",
        components = 3,
        order = 2,
        initial_condition = "displacement0",
        boundary_conditions = list(
            r2ogs6_boundary_condition(
                type = "Dirichlet",
                parameter = "zero",
                component = 1,
                mesh = NULL,
                geometrical_set = "cube_1x1x1_geometry",
                geometry = "front"
            ),
            r2ogs6_boundary_condition(
                type = "Dirichlet",
                parameter = "zero",
                component = 0,
                mesh = NULL,
                geometrical_set = "cube_1x1x1_geometry",
                geometry = "left"
            ),
            r2ogs6_boundary_condition(
                type = "Dirichlet",
                parameter = "zero",
                component = 2,
                mesh = NULL,
                geometrical_set = "cube_1x1x1_geometry",
                geometry = "bottom"
            ),
            r2ogs6_boundary_condition(
                type = "Neumann",
                parameter = "pressure_load",
                component = 1,
                mesh = NULL,
                geometrical_set = "cube_1x1x1_geometry",
                geometry = "back"
            ),
            r2ogs6_boundary_condition(
                type = "Neumann",
                parameter = "pressure_load",
                component = 0,
                mesh = NULL,
                geometrical_set = "cube_1x1x1_geometry",
                geometry = "right"
            ),
            r2ogs6_boundary_condition(
                type = "Neumann",
                parameter = "pressure_load",
                component = 2,
                mesh = NULL,
                geometrical_set = "cube_1x1x1_geometry",
                geometry = "top"
            )
        )
    )
)


ogs6_obj$add_process_variable(
    r2ogs6_process_variable(
        name = "pressure",
        components = 1,
        order = 1,
        initial_condition = "pressure0",
        boundary_conditions = list(
            r2ogs6_boundary_condition(
                type = "Neumann",
                parameter = "flux_in",
                component = 0,
                mesh = NULL,
                geometrical_set = "cube_1x1x1_geometry",
                geometry = "left"
            )
        )
    )
)


ogs6_obj$add_nonlinear_solver(
    r2ogs6_nonlinear_solver(
        name = "basic_newton",
        type = "Newton",
        max_iter = 50,
        linear_solver = "general_linear_solver"
    )
)


ogs6_obj$add_linear_solver(
    r2ogs6_linear_solver(
        name = "general_linear_solver",
        eigen = list(
            solver_type = "BiCGSTAB",
            precon_type = "ILUT",
            max_iteration_step = 10000,
            error_tolerance = 1e-16
        ),
        lis = "-i bicgstab -p ilu -tol 1e-16 -maxiter 10000",
        petsc = NULL
    )
)


run_simulation(ogs6_obj)
