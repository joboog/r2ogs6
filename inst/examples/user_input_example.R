detach("package:r2ogs6", unload=TRUE)
library(r2ogs6)


#This is where the user defines their data.

#============================== SET UP SIMULATION OBJECT ================================

#If you want to test this, don't forget to alter the paths to fit your system!

#First, we need to create a simulation object.

ogs6_obj <- OGS6$new(sim_name = "my_sim",
                     sim_id = 1,
                     sim_path = "D:\\OGS_Sim\\",
                     ogs_bin_path = "D:\\Programme\\OpenGeoSys\\ogs-6.3.2-Windows-10.0.14393-x64-python-3.7.2-de-utils\\bin\\")


#============================== ADD .gml DATA ================================

#(source: HydroComponent Benchmarks -> IdealGas -> flow_no_strain -> square_1x1.gml)

#Add some geometry
input_add(r2ogs6_gml(name = "square_1x1_geometry",
                     points = tibble::tibble(x = c(0, 0, 1, 1),
                                             y = c(0, 1, 0, 1),
                                             z = c(0, 0, 0, 0),
                                             name = c("origin", rep("", 3))),
                     polylines = list(list(name = "left", c(0, 1)),
                                      list(name = "right", c(2, 3)),
                                      list(name = "bottom", c(0, 2)),
                                      list(name = "top", c(1, 3)))),
          ogs6_obj)

#============================== ADD .vtu DATA ================================

#Let's add our data for the .vtk file, which contains information about our mesh.

# #This calls an external script (which is why we needed to specify some paths earlier)
# generate_structured_mesh(ogs6_obj, "-e quad --lx 1 --ly 1 --lz 1")

#To make sure our geometry is right, let's copy the .vtu from the benchmark
# we're trying to reverse engineer...

pick_vtu_file(ogs6_obj)


#============================== ADD .prj DATA ================================

#Let's add our data for the .prj file, arguably the most important file.
#(source: HydroComponent Benchmarks -> IdealGas -> flow_no_strain -> flow_no_strain.prj)

#We need:
#1. Either one mesh and one geometry file OR multiple mesh files (for the latter case: Skip to
# previous part and call generate_structured_mesh at least one more time)

#2. One or more process elements:
input_add(r2ogs6_process(name = "HM",
                         type = "HYDRO_MECHANICS",
                         integration_order = 3,
                         dimension = 2,
                         constitutive_relation = c(type = "LinearElasticIsotropic",
                                                   youngs_modulus = "E",
                                                   poissons_ratio = "nu"),
                         process_variables = c(displacement = "displacement",
                                               pressure = "pressure"),
                         secondary_variables = list(c("sigma_xx", "sigma_xx"),
                                                    c("sigma_yy", "sigma_yy"),
                                                    c("sigma_zz", "sigma_zz"),
                                                    c("sigma_xy", "sigma_xy"),
                                                    c("epsilon_xx", "epsilon_xx"),
                                                    c("epsilon_yy", "epsilon_yy"),
                                                    c("epsilon_zz", "epsilon_zz"),
                                                    c("epsilon_xy", "epsilon_xy"),
                                                    c("velocity", "velocity")),
                         specific_body_force = c(0, 0)),
          ogs6_obj)

#3. One time_loop element:
input_add(r2ogs6_time_loop(processes = list(r2ogs6_tl_process(ref = "HM",
                                                              nonlinear_solver = "basic_newton",
                                                              convergence_criterion = list(type = "PerComponentDeltaX",
                                                                                           norm_type = "NORM2",
                                                                                           reltols = "5e-8 1e10 1e10"),
                                                              time_discretization = list(type = "BackwardEuler"),
                                                              time_stepping = list(type = "FixedTimeStepping",
                                                                                   t_initial = 0,
                                                                                   t_end = 100,
                                                                                   timesteps = list(pair = list(rep = 1,
                                                                                                                delta_t = 0.1))
                                                                                   )
                                                              )),
                           output = r2ogs6_tl_output(type = "VTK",
                                                     prefix = "flow_no_strain_pcs_{:process_id}",
                                                     suffix = "_ts_{:timestep}_t_{:time}",
                                                     timesteps = list(pair = list(rep = 1,
                                                                              each_steps = 1000)),
                                                     variables = list("displacement",
                                                                      "pressure",
                                                                      "sigma_xx",
                                                                      "sigma_yy",
                                                                      "sigma_zz",
                                                                      "sigma_xy",
                                                                      "epsilon_xx",
                                                                      "epsilon_yy",
                                                                      "epsilon_zz",
                                                                      "epsilon_xy",
                                                                      "velocity"))),
          ogs6_obj)

#4. One or more medium elements:
input_add(r2ogs6_medium(phases = list(r2ogs6_medium_phase(type = "Gas",
                                                          properties = list(r2ogs6_medium_property(name = "viscosity",
                                                                                                   type = "Constant",
                                                                                                   value = 1e-5),
                                                                            r2ogs6_medium_property(name = "density",
                                                                                                   type = "IdealGasLaw"),
                                                                            r2ogs6_medium_property(name = "molar_mass",
                                                                                                   type = "Constant",
                                                                                                   value = 0.028964397787206768))),
                                      r2ogs6_medium_phase(type = "Solid",
                                                          properties = list(r2ogs6_medium_property(name = "porosity",
                                                                                                   type = "Constant",
                                                                                                   value = 0.03),
                                                                            r2ogs6_medium_property(name = "density",
                                                                                                   type = "Constant",
                                                                                                   value = 2.17e3),
                                                                            r2ogs6_medium_property(name = "biot_coefficient",
                                                                                                   type = "Constant",
                                                                                                   value = 0.6)))),
                        properties = list(r2ogs6_medium_property(name = "reference_temperature",
                                                                 type = "Constant",
                                                                 value = 293.15),
                                          r2ogs6_medium_property(name = "permeability",
                                                                 type = "Constant",
                                                                 value = 1e-4))),
          ogs6_obj)

#5. One or more parameters:
input_add(r2ogs6_parameter(name = "E",
                           type = "Constant",
                           values = 1e9),
          ogs6_obj)

input_add(r2ogs6_parameter(name = "nu",
                           type = "Constant",
                           values = .3),
          ogs6_obj)

input_add(r2ogs6_parameter(name = "displacement0",
                           type = "Constant",
                           values = c(0, 0)),
          ogs6_obj)

input_add(r2ogs6_parameter(name = "pressure0",
                           type = "Constant",
                           values = 1e5),
          ogs6_obj)

input_add(r2ogs6_parameter(name = "zero",
                           type = "Constant",
                           values = 0),
          ogs6_obj)

input_add(r2ogs6_parameter(name = "flux",
                           type = "Constant",
                           values = 1e-3),
          ogs6_obj)


#6. One or more process variables
input_add(r2ogs6_process_variable(name = "displacement",
                                  components = 2,
                                  order = 2,
                                  initial_condition = "displacement0",
                                  boundary_conditions = list(r2ogs6_boundary_condition(type = "Dirichlet",
                                                                                       parameter = "zero",
                                                                                       component = 0,
                                                                                       geometrical_set = "square_1x1_geometry",
                                                                                       geometry = "left"),
                                                             r2ogs6_boundary_condition(type = "Dirichlet",
                                                                                       parameter = "zero",
                                                                                       component = 1,
                                                                                       geometrical_set = "square_1x1_geometry",
                                                                                       geometry = "bottom"),
                                                             r2ogs6_boundary_condition(type = "Dirichlet",
                                                                                       parameter = "zero",
                                                                                       component = 0,
                                                                                       geometrical_set = "square_1x1_geometry",
                                                                                       geometry = "right"),
                                                             r2ogs6_boundary_condition(type = "Dirichlet",
                                                                                       parameter = "zero",
                                                                                       component = 1,
                                                                                       geometrical_set = "square_1x1_geometry",
                                                                                       geometry = "top"))),
          ogs6_obj)

input_add(r2ogs6_process_variable(name = "pressure",
                                  components = 1,
                                  order = 1,
                                  initial_condition = "pressure0",
                                  boundary_conditions = list(r2ogs6_boundary_condition(type = "Neumann",
                                                                                       parameter = "flux",
                                                                                       component = 0,
                                                                                       geometrical_set = "square_1x1_geometry",
                                                                                       geometry = "left")
                                                             )
                                  ),
          ogs6_obj)

#7. One or more nonlinear solvers
input_add(r2ogs6_nonlinear_solver(name = "basic_newton",
                                  type = "Newton",
                                  max_iter = 50,
                                  linear_solver = "general_linear_solver"),
          ogs6_obj)

#8. One or more linear solvers
input_add(r2ogs6_linear_solver(name = "general_linear_solver",
                               eigen = list(solver_type = "BiCGSTAB",
                                            precon_type = "ILUT",
                                            max_iteration_step = 10000,
                                            error_tolerance = 1e-16),
                               lis = "-i bicgstab -p ilu -tol 1e-16 -maxiter 10000"),
          ogs6_obj)


#OPTIONAL: A test definition

#============================== Execution ================================

#Calls ogs6 object validator functions, exports all necessary files and starts OpenGeoSys6

run_simulation(ogs6_obj)

