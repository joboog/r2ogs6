#This is where the user defines his data.


#============================== SET UP SIMULATION OBJECT ================================

# my_ogs6_obj <- new_ogs6( sim_io = list(input = list(), output = list()),
#                          sim_name = "my_sim",
#                          sim_id = 1L,
#                          sim_path = "simulations/")

#R6
ogs6_obj <- OGS6$new( sim_name = "my_sim",
                         sim_id = 1L,
                         sim_path = "D:\\OGS_Sim",
                         ogs_bin_path = "")


#============================== ADD .gml DATA ================================

#(source: HydroComponent Benchmarks -> IdealGas -> flow_free_expansion -> cube_1x1x1.gml)

# #Add gml object and give it a name
# input_add_gml_obj(my_ogs6_obj, "cube_1x1x1_geometry")
#



# # #Add some points
# input_add_gml_points(my_ogs6_obj, tibble::tibble(x = c(0, 0, 0, 0, 1, 1, 1, 1),
#                                                  y = c(0, 0, 1, 1, 0, 0, 1, 1),
#                                                  z = c(0, 1, 1, 0, 0, 1, 1, 0),
#                                                  name = c("origin", rep("", 7))))
#
# #Add some polylines
# input_add_gml_polylines(my_ogs6_obj, list(list(name = "front_left", c(0, 1)),
#                                           list(name = "front_right", c(4, 5)),
#                                           list(name = "front_bottom", c(0, 4)),
#                                           list(name = "front_top", c(1, 5)),
#                                           list(name = "bottom_left", c(0, 3)),
#                                           list(name = "bottom_right", c(4, 7)),
#                                           list(name = "top_left", c(1, 2)),
#                                           list(name = "top_right", c(5, 6)),
#                                           list(name = "back_left", c(2, 3)),
#                                           list(name = "back_right", c(6, 7)),
#                                           list(name = "back_bottom", c(3, 7)),
#                                           list(name = "back_top", c(2, 6))))
#
# #Add some surfaces
# input_add_gml_surfaces(my_ogs6_obj, list(list(name = "left", c(0, 1, 2), c(0, 3, 2)),
#                                          list(name = "right", c(4, 6, 5), c(4, 6, 7)),
#                                          list(name = "top", c(1, 2, 5), c(5, 2, 6)),
#                                          list(name = "bottom", c(0, 3, 4), c(4, 3, 7)),
#                                          list(name = "front", c(0, 1, 4), c(4, 1, 5)),
#                                          list(name = "back", c(2, 3, 6), c(6, 3, 7))))


#============================== ADD .vtu DATA ================================

#Let's add our data for the .vtk file, which contains information about our mesh.

# #This calls an external script (which is why we needed to specify some paths earlier)
# generate_structured_mesh(my_ogs6_obj, "line", "--lx 3")


#============================== ADD .prj DATA ================================

#Let's add our data for the .prj file, arguably the most important file.

# #Add prj object and give it a name
# input_add_prj_obj(my_ogs6_obj)

#We need:
#1. Either one mesh and one geometry file OR multiple mesh files (for the latter case: Skip to
# previous part and call generate_structured_mesh at least one more time)

# #2. One or more process elements:
# input_add(r2ogs6_process(name = "HM",
#                          type = "HYDRO_MECHANICS",
#                          integration_order = 3,
#                          dimension = 2,
#                          constitutive_relation = list(type = "LinearElasticIsotropic",
#                                                       youngs_modulus = "E",
#                                                       poissons_ratio = "nu"),
#                          process_variables = list(displacement = "displacement",
#                                                   pressure = "pressure"),
#                          secondary_variables = list(c("sigma_xx", "sigma_xx"),
#                                                     c("sigma_yy", "sigma_yy"),
#                                                     c("sigma_zz", "sigma_zz"),
#                                                     c("sigma_xy", "sigma_xy"),
#                                                     c("epsilon_xx", "epsilon_xx"),
#                                                     c("epsilon_yy", "epsilon_yy"),
#                                                     c("epsilon_zz", "epsilon_zz"),
#                                                     c("epsilon_xy", "epsilon_xy"),
#                                                     c("velocity", "velocity")),
#                          specific_body_force = c(0, 0)),
#           ogs6_obj)
#
# #3. One time_loop element:
# input_add(r2ogs6_time_loop(processes = ,
#                                output = r2ogs6_process_output(type = "",
#                                                                   prefix = "",
#                                                                   timesteps = list(),
#                                                                   variables = list(),
#                                                                   suffix = "",
#                                                                   compress_output = FALSE)
#                                ),
#           ogs6_obj)
#
#4. One or more medium elements:
# input_add(r2ogs6_medium(phases = list(r2ogs6_medium_phase(type = Gas,
#                                                           properties = list(r2ogs6_medium_property(name = "viscosity",
#                                                                                                    type = "Constant",
#                                                                                                    value = 1e-5)))),
#                         properties = list(r2ogs6_medium_property(name = "reference_temperature",
#                                                                  type = "Constant",
#                                                                  value = 293.15)),
#                             id = 1),
#           ogs6_obj)
#
# #5. One or more parameters:
# input_add(r2ogs6_parameter(name = "E",
#                            type = "Constant",
#                            values = 1e9),
#           ogs6_obj)
#
# input_add(r2ogs6_parameter(name = "nu",
#                            type = "Constant",
#                            values = .3),
#           ogs6_obj)


#6. One or more process variables
# input_add(r2ogs6_process_variable(name = "displacement",
#                                   components = 2,
#                                   order = 2,
#                                   initial_condition = "displacement0",
#                                   boundary_conditions = list(r2ogs6_boundary_condition())),
#           ogs6_obj)
#
# input_add(r2ogs6_process_variable(name = "pressure",
#                                   components = 1,
#                                   order = 1,
#                                   initial_condition = "pressure0",
#                                   boundary_conditions = list(r2ogs6_boundary_condition(type = "Neumann",
#                                                                                        parameter = "flux",
#                                                                                        component = 0,
#                                                                                        geometrical_set = "square_1x1_geometry",
#                                                                                        geometry = "left")
#                                                              )
#                                   ),
#           ogs6_obj)
#
# #7. One or more nonlinear solvers
# input_add(r2ogs6_nonlinear_solver(name = "basic_newton",
#                                   type = "Newton",
#                                   max_iter = 50,
#                                   linear_solver = "general_linear_solver"),
#           ogs6_obj)
#
# #8. One or more linear solvers
# input_add(r2ogs6_linear_solver(name = "general_linear_solver",
#                                eigen = list(solver_type = "BiCGSTAB",
#                                             precon_type = "ILUT",
#                                             max_iteration_step = 10000,
#                                             error_tolerance = 1e-16),
#                                lis = "-i bicgstab -p ilu -tol 1e-16 -maxiter 10000",
#                                petsc = NULL),
#           ogs6_obj)


#OPTIONAL: A test definition

#============================== Execution ================================

#Calls ogs6 object validator functions, exports all necessary files and starts OpenGeoSys6

#run_simulation(my_ogs6_obj)

