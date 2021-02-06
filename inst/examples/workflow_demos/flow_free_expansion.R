


library(r2ogs6)

#===== Set up simulation object =====


# First make sure you're ready to go by setting r2ogs6.default_ogs_bin_path.
# You can do this by commenting out the line below and modifying the path to
# fit your system.

# options("r2ogs6.default_ogs_bin_path" = "your_path_here")


# Then we can create a simulation object.

ogs6_obj <- OGS6$new(sim_name = "flow_free_expansion",
                     sim_id = 1,
                     sim_path = "D:/OGS_Sim/")


ogs6_obj$add_gml(
    OGS6_gml$new(
        name = "cube_1x1x1_geometry",
        points = tibble::tibble(
            x = c(0, 0, 0, 0, 1, 1, 1, 1),
            y = c(0, 0, 1, 1, 0, 0, 1, 1),
            z = c(0, 1, 1, 0, 0, 1, 1, 0),
            name = c("origin", "", "", "", "", "", "", "")
        ),
        polylines = list(
            polyline = list("front_left",
                            c(pnt = 0, pnt = 1)),
            polyline = list("front_right",
                            c(pnt = 4, pnt = 5)),
            polyline = list("front_bottom",
                            c(pnt = 0, pnt = 4)),
            polyline = list("front_top",
                            c(pnt = 1, pnt = 5)),
            polyline = list("bottom_left",
                            c(pnt = 0, pnt = 3)),
            polyline = list("bottom_right",
                            c(pnt = 4, pnt = 7)),
            polyline = list("top_left",
                            c(pnt = 1, pnt = 2)),
            polyline = list("top_right",
                            c(pnt = 5, pnt = 6)),
            polyline = list("back_left",
                            c(pnt = 2, pnt = 3)),
            polyline = list("back_right",
                            c(pnt = 6, pnt = 7)),
            polyline = list("back_bottom",
                            c(pnt = 3, pnt = 7)),
            polyline = list("back_top",
                            c(pnt = 2, pnt = 6))
        ),
        surfaces = list(
            surface = list(
                name = "left",
                element = c(p1 = 0, p2 = 1, p3 = 2),
                element = c(p1 = 0, p2 = 3, p3 = 2)
            ),
            surface = list(
                name = "right",
                element = c(p1 = 4, p2 = 6, p3 = 5),
                element = c(p1 = 4, p2 = 6, p3 = 7)
            ),
            surface = list(
                name = "top",
                element = c(p1 = 1, p2 = 2, p3 = 5),
                element = c(p1 = 5, p2 = 2, p3 = 6)
            ),
            surface = list(
                name = "bottom",
                element = c(p1 = 0, p2 = 3, p3 = 4),
                element = c(p1 = 4, p2 = 3, p3 = 7)
            ),
            surface = list(
                name = "front",
                element = c(p1 = 0, p2 = 1, p3 = 4),
                element = c(p1 = 4, p2 = 1, p3 = 5)
            ),
            surface = list(
                name = "back",
                element = c(p1 = 2, p2 = 3, p3 = 6),
                element = c(p1 = 6, p2 = 3, p3 = 7)
            )
        )
    )
)

ogs6_obj$add_vtu(
    "D:/Programme/OpenGeoSys/ogs-master-Tests-Data/HydroMechanics/IdealGas/flow_free_expansion/cube_1x1x1_quad.vtu",
    FALSE
)

ogs6_obj$add(
    r2ogs6_process(
        name = "HM",
        type = "HYDRO_MECHANICS",
        integration_order = 3,
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
        dimension = 3,
        constitutive_relation = r2ogs6_constitutive_relation(
            type = "LinearElasticIsotropic",
            youngs_modulus = "E",
            poissons_ratio = "nu"
        )
    )
)


ogs6_obj$add(
    r2ogs6_process_variable(
        name = "displacement",
        components = 3,
        order = 2,
        initial_condition = "displacement0",
        boundary_conditions = list(
            boundary_condition = r2ogs6_boundary_condition(
                type = "Dirichlet",
                parameter = "zero",
                geometrical_set = "cube_1x1x1_geometry",
                geometry = "front",
                component = 1
            ),
            boundary_condition = r2ogs6_boundary_condition(
                type = "Dirichlet",
                parameter = "zero",
                geometrical_set = "cube_1x1x1_geometry",
                geometry = "left",
                component = 0
            ),
            boundary_condition = r2ogs6_boundary_condition(
                type = "Dirichlet",
                parameter = "zero",
                geometrical_set = "cube_1x1x1_geometry",
                geometry = "bottom",
                component = 2
            ),
            boundary_condition = r2ogs6_boundary_condition(
                type = "Neumann",
                parameter = "pressure_load",
                geometrical_set = "cube_1x1x1_geometry",
                geometry = "back",
                component = 1
            ),
            boundary_condition = r2ogs6_boundary_condition(
                type = "Neumann",
                parameter = "pressure_load",
                geometrical_set = "cube_1x1x1_geometry",
                geometry = "right",
                component = 0
            ),
            boundary_condition = r2ogs6_boundary_condition(
                type = "Neumann",
                parameter = "pressure_load",
                geometrical_set = "cube_1x1x1_geometry",
                geometry = "top",
                component = 2
            )
        )
    )
)


ogs6_obj$add(
    r2ogs6_process_variable(
        name = "pressure",
        components = 1,
        order = 1,
        initial_condition = "pressure0",
        boundary_conditions = list(
            boundary_condition = r2ogs6_boundary_condition(
                type = "Neumann",
                parameter = "flux_in",
                geometrical_set = "cube_1x1x1_geometry",
                geometry = "left",
                component = 0
            )
        )
    )
)


ogs6_obj$add(r2ogs6_medium(
    phases = list(
        phase = r2ogs6_phase(
            type = "Gas",
            properties = list(
                property = r2ogs6_ph_property(name = "viscosity",
                                              type = "Constant",
                                              value = 1e-05),
                property = r2ogs6_ph_property(name = "density",
                                              type = "IdealGasLaw"),
                property = r2ogs6_ph_property(name = "molar_mass",
                                              type = "Constant",
                                              value = 0.0289643977872068)
            )
        ),
        phase = r2ogs6_phase(
            type = "Solid",
            properties = list(
                property = r2ogs6_ph_property(name = "porosity",
                                              type = "Constant",
                                              value = 0.3),
                property = r2ogs6_ph_property(name = "density",
                                              type = "Constant",
                                              value = 1430),
                property = r2ogs6_ph_property(name = "biot_coefficient",
                                              type = "Constant",
                                              value = 0.6)
            )
        )
    ),
    properties = list(
        property = r2ogs6_pr_property(name = "reference_temperature",
                                      type = "Constant",
                                      value = 293.15),
        property = r2ogs6_pr_property(name = "permeability",
                                      type = "Constant",
                                      value = 1e-05)
    )
))


ogs6_obj$add(r2ogs6_time_loop(
    processes = list(
        process = r2ogs6_tl_process(
            ref = "HM",
            nonlinear_solver = "basic_newton",
            convergence_criterion = r2ogs6_convergence_criterion(
                type = "DeltaX",
                norm_type = "NORM2",
                reltol = 1e-08
            ),
            time_discretization = list(type = "BackwardEuler"),
            time_stepping = r2ogs6_time_stepping(
                type = "FixedTimeStepping",
                t_initial = 0,
                t_end = 10000,
                timesteps = list(pair = list(1000,
                                             delta_t = 10))
            )
        )
    ),
    output = r2ogs6_output(
        type = "VTK",
        prefix = "flow_free_expansion",
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
        suffix = "_ts_{:timestep}_t_{:time}",
        timesteps = list(pair = list(1,
                                     each_steps = 1000))
    )
))


ogs6_obj$add(
    r2ogs6_linear_solver(
        name = "general_linear_solver",
        eigen = r2ogs6_eigen(
            solver_type = "BiCGSTAB",
            precon_type = "ILUT",
            max_iteration_step = 10000,
            error_tolerance = 1e-16
        ),
        lis = "-i bicgstab -p ilu -tol 1e-16 -maxiter 10000"
    )
)


ogs6_obj$add(
    r2ogs6_nonlinear_solver(
        name = "basic_newton",
        type = "Newton",
        max_iter = 50,
        linear_solver = "general_linear_solver"
    )
)


ogs6_obj$add(r2ogs6_parameter(name = "E",
                              type = "Constant",
                              value = 1e+10))


ogs6_obj$add(r2ogs6_parameter(name = "nu",
                              type = "Constant",
                              value = 0.3))


ogs6_obj$add(r2ogs6_parameter(
    name = "displacement0",
    type = "Constant",
    values = c(0, 0, 0)
))


ogs6_obj$add(r2ogs6_parameter(
    name = "pressure0",
    type = "Constant",
    values = 1e+05
))


ogs6_obj$add(r2ogs6_parameter(
    name = "pressure_load",
    type = "Constant",
    values = -60000
))


ogs6_obj$add(r2ogs6_parameter(name = "zero",
                              type = "Constant",
                              value = 0))


ogs6_obj$add(r2ogs6_parameter(name = "flux_in",
                              type = "Constant",
                              value = 1e-04))


run_simulation(ogs6_obj)
