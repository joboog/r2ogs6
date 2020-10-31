#This is where the user defines his data.


#============================== SET UP SIMULATION OBJECT ================================

# my_ogs6_obj <- new_ogs6( sim_io = list(input = list(), output = list()),
#                          sim_name = "my_sim",
#                          sim_id = 1L,
#                          sim_path = "simulations/")


my_ogs6_obj <- OGS6$new( sim_name = "my_sim",
                         sim_id = 1L,
                         sim_path = "simulations/")


#============================== ADD .gml DATA ================================

#(source: HydroComponent Benchmarks -> IdealGas -> flow_free_expansion -> cube_1x1x1.gml)

# #Add gml object and give it a name
# input_add_gml_obj(my_ogs6_obj, "cube_1x1x1_geometry")
#



# #Add some points
input_add_gml_points(my_ogs6_obj, tibble::tibble(x = c(0, 0, 0, 0, 1, 1, 1, 1),
                                                 y = c(0, 0, 1, 1, 0, 0, 1, 1),
                                                 z = c(0, 1, 1, 0, 0, 1, 1, 0),
                                                 name = c("origin", rep("", 7))))
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

#(source: HydroComponent Benchmarks -> IdealGas -> flow_free_expansion -> cube_1x1x1.vtu)

# #Add vtu object and give it a name
# input_add_vtu_obj(my_ogs6_obj, list(type="UnstructuredGrid",
#                              version="0.1",
#                              byte_order="LittleEndian",
#                              header_type="UInt32",
#                              compressor="vtkZLibDataCompressor"))
#
# #Add an unstructured grid to the vtu object
# input_add_vtu_unstructured_grid(my_ogs6_obj, list())
#
# #Add appended data to the vtu object
# input_add_vtu_appended_data(my_ogs6_obj, encoding = "", data = "")
#
# #... (WIP)


#============================== ADD .prj DATA ================================

#There is no documentation for the constructors (yet), but with classes instead of just lists,
#the user could get way more info (also, I can build some nice validator friends)


#Define processes (refer to ?new_r2ogs6_process for info on valid arguments)
# processes <- list(new_r2ogs_prj_process(name = "HM",
#                                  type = "HYDRO_MECHANICS",
#                                  integration_order = 3,
#                                  dimension = 3,
#                                  constitutive_relation = list(type = "LinearElasticIsotropic",
#                                                               youngs_modulus = "E",
#                                                               poissons_ratio = "nu"),
#                                  process_variables = list(displacement = "displacement",
#                                                           pressure = "pressure"),
#                                  secondary_variables = list(stub = 0),
#                                  specific_body_force = c(0, 0, 0))
#                   )


#Define media (refer to ?new_r2ogs6_prj_medium for info on valid arguments)
media <- list(new_r2ogs6_prj_medium(phases = list(phase = list(type = "Gas",
                                                       properties = list(property = list(name = "viscosity",
                                                                                         type = "Constant",
                                                                                         value = 1e-5)))
                                                  ),
                                    properties = list(property = list(name = "reference_temperature",
                                                              type = "Constant",
                                                              value = 293.15)
                                                      )
                            )
              )


time_loop <- list(processes = list(),
                  output = list(type = "VTK",
                                prefix = "",
                                timesteps = list(),
                                variables = list(),
                                suffix = "_ts_{:timestep}_t_{:time}")
                  )


#Define parameters (refer to ?new_r2ogs6_parameter for info on valid arguments)
parameters <- list(new_r2ogs6_prj_parameter(name = "E",
                                    type = "Constant",
                                    value = 10e9)
                   )


#Define process variables
process_variables <- list(process_variable = list(name = "displacement",
                                                  components = 3,
                                                  order = 2,
                                                  initial_condition = "displacement0",
                                                  boundary_conditions = list(boundary_condition = list()))
                          )


#Define nonlinear solvers
nonlinear_solvers <- list(nonlinear_solver = list(name = "basic_newton",
                                                  type = "Newton",
                                                  max_iter = 50,
                                                  linear_solver = "general_linear_solver")
                          )


linear_solvers <- list(linear_solver = list(name = "general_linear_solver",
                                            lis = "-i bicgstab -p ilu -tol 1e-16 -maxiter 10000",
                                            eigen = list(solver_type = "BiCGSTAB",
                                                         precon_type = "ILUT",
                                                         max_iteration_step = 10000,
                                                         error_tolerance = 1e-16))
                       )



#============================== Execution (to be moved to another file later!) ================================

#============================== .gml ================================

#Call ogs6 object validator functions
#...

# #Now export all necessary files
# my_gml_xml <- gml_data_to_xml(geo_name, my_gml_points, my_gml_polylines, my_gml_surfaces)
#
#
# #Let's export that as a new .gml XML file
# export_xml_to_file(my_gml_xml, "test.gml")

#Now the .gml XML file should be in the r2ogs6 folder (not a perfect location but ok for a little test)

#The only thing missing from the new file compared to the original is the stylesheet declaration...
#I haven't found a way to add that one yet, but it should also be possible somehow


#============================== RUN SIMULATION ================================

#Calls ogs6 object validator functions, exports all necessary files and starts OpenGeoSys6

#run_simulation(my_ogs6_obj)

