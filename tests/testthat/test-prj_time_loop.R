
test_that("new_r2ogs6_time_loop basic validation is working", {

    expect_error(new_r2ogs6_time_loop(list("missing"),

                                      r2ogs6_tl_output(type = "VTK",
                                                                prefix = "flow_no_strain_pcs_{:process_id}",
                                                                suffix = "_ts_{:timestep}_t_{:time}",
                                                                timesteps = list(pair = list(rep = 1,
                                                                                        each_steps = 1000)),
                                                                variables = list("displacement")
                                                             )
                                      )
                 )

    expect_error(new_r2ogs6_time_loop(list(r2ogs6_tl_process(ref = "HM",
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

                                      list(type = "VTK",
                                           prefix = "flow_no_strain_pcs_{:process_id}",
                                           suffix = "_ts_{:timestep}_t_{:time}",
                                           timesteps = list(pair = list(rep = 1,
                                                                        each_steps = 1000)),
                                           variables = list("wrong_format"))
                                      )
                 )
})



test_that("new_r2ogs6_tl_process basic validation is working", {



})


test_that("new_r2ogs6_tl_output basic validation is working", {



})


