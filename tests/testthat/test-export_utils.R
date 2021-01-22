
test_that("to_node works for atomic values", {

    my_string <- "Hello there"

    str_node <- to_node(my_string)
    str_xml <- xml2::as_xml_document(str_node)

    expect_equal(xml2::xml_text(xml2::xml_find_first(str_xml,
                                                     "/my_string")),
                 "Hello there")
})

test_that("to_node works for vectors", {

    my_vect = c(a = 1, b = 2, c = 3)

    vect_node <- to_node(my_vect, "vect")
    vect_xml <- xml2::as_xml_document(vect_node)

    expect_equal(xml2::xml_text(xml2::xml_find_first(vect_xml,
                                                     "/vect/a")),
                 "1")
})


test_that("to_node works for attribute vectors", {

    my_attr_list <- list(a = c(id = 0, name = "Alice"),
                         b = c(id = 3))

    attr_node <- to_node(my_attr_list,
                         object_name = "my_attr_list",
                         c("a", "b"))

    attr_xml <- xml2::as_xml_document(attr_node)

    expect_equal(xml2::xml_attrs(xml2::xml_find_first(attr_xml,
                                                     "/my_attr_list/a")),
                 c(id = "0", name = "Alice"))
    expect_equal(xml2::xml_attrs(xml2::xml_find_first(attr_xml,
                                                      "/my_attr_list/b")),
                 c(id = "3"))
})


test_that("to_node reads parameter names implicitly if not given", {

    test_class <- function(x){
        structure(list(x = x,
                       y = "there"),
                  class = "test_class")
    }

    test_obj <- test_class("Hi")
    test_node <- to_node(test_obj$x)

    test_xml <- xml2::as_xml_document(test_node)

    expect_equal(xml2::xml_text(xml2::xml_find_first(test_xml, "/x")), "Hi")
})


test_that("to_node works for simple classes", {

    #Test for a single class element
    parameter <- r2ogs6_parameter(name = "pressure0",
                                  type = "Constant",
                                  values = 1e5)

    parameter_node <- to_node(parameter)
    parameter_xml <- xml2::as_xml_document(parameter_node)

    expect_equal(xml2::xml_text(xml2::xml_find_first(parameter_xml,
                                                     "/parameter/name")),
                 "pressure0")
    expect_equal(xml2::xml_double(xml2::xml_find_first(parameter_xml,
                                                     "/parameter/values")),
                 1e5)

    #Test for a wrapper list
    parameter_2 <- r2ogs6_parameter(name = "pressure1",
                                    type = "Constant",
                                    values = c(0, 0))

    para_wrapper <- list(parameter, parameter_2)
    wrapper_node <- to_node(para_wrapper, "parameters")
    wrapper_xml <- xml2::as_xml_document(wrapper_node)

    expect_equal(length(
        xml2::xml_find_all(wrapper_xml, "/parameters/parameter")), 2)

    expect_equal(xml2::xml_text(
        xml2::xml_find_all(wrapper_xml, "/parameters/parameter/values")[[2]]),
        "0 0")
})


test_that("to_node works for classes that have lists as parameters", {

    insitu <- r2ogs6_insitu(c("script_1",
                              "script_2",
                              "script_3"))

    insitu_node <- to_node(insitu)
    insitu_xml <- xml2::as_xml_document(insitu_node)

    expect_equal(length(xml2::xml_find_all(insitu_xml,
                                           "/insitu/scripts/*")), 3)
})


test_that("to_node works for classes that have subclasses", {

    process_variable <- r2ogs6_process_variable(
        name = "pressure",
        components = 1,
        order = 1,
        initial_condition = "pressure0",
        boundary_conditions = list(
            r2ogs6_boundary_condition(
                type = "Neumann",
                parameter = "flux",
                component = 0,
                geometrical_set = "square_1x1_geometry",
                geometry = "left"
            )
        )
    )

    process_variable_node <- to_node(process_variable)
    process_variable_xml <- xml2::as_xml_document(process_variable_node)

    expect_equal(length(xml2::xml_find_all(
        process_variable_xml, "/process_variable/boundary_conditions/*")), 1)

    expect_equal(xml2::xml_text(
        xml2::xml_find_first(
            process_variable_xml,
            "/process_variable/boundary_conditions/boundary_condition/geometry"
        )
    ),
    "left")
})

test_that("to_node works for classes that have attributes", {

    tl_process <- r2ogs6_tl_process(
        ref = "HM",
        nonlinear_solver = "basic_newton",
        convergence_criterion = r2ogs6_convergence_criterion(
            type = "PerComponentDeltaX",
            norm_type = "NORM2",
            reltols = "5e-8 1e10 1e10"
        ),
        time_discretization = list(type = "BackwardEuler"),
        time_stepping = r2ogs6_time_stepping(
            type = "FixedTimeStepping",
            t_initial = 0,
            t_end = 100,
            timesteps = list(pair = list(rep = 1,
                                         delta_t = 0.1))
        )
    )

    tl_process_node <- to_node(tl_process)
    tl_process_xml <- xml2::as_xml_document(tl_process_node)

    attrs <- xml2::xml_attrs(xml2::xml_find_first(tl_process_xml, "/process"))
    expect_equal(attrs, c(ref = "HM"))
})


test_that("to_node works for classes that have non-exported wrappers", {

    #Test for a single class element
    parameter <- r2ogs6_parameter(name = "pressure0",
                                  type = "Constant",
                                  values = 1e5,
                                  index_values = list("1", "1 2"),
                                  index_values = list("2", "2 3"))

    parameter_node <- to_node(parameter)
    parameter_xml <- xml2::as_xml_document(parameter_node)

    index_value_nodes <- xml2::xml_find_all(parameter_xml,
                                            "/parameter/index_values")

    expect_equal(length(index_value_nodes), 2)
})


test_that("to_node works for r2ogs6_process class", {

    process <- r2ogs6_process(
        name = "HM",
        type = "HYDRO_MECHANICS",
        integration_order = 3,
        dimension = 2,
        constitutive_relation = r2ogs6_constitutive_relation(
            type = "LinearElasticIsotropic",
            youngs_modulus = "E",
            poissons_ratio = "nu"
        ),
        process_variables = c(displacement = "displacement",
                              pressure = "pressure"),
        secondary_variables = list(
            c("sigma_xx", "sigma_xx"),
            c("sigma_yy", "sigma_yy")
        ),
        specific_body_force = c(0, 0)
    )

    process_node <- to_node(process)
    process_xml <- xml2::as_xml_document(process_node)

    attrs <- xml2::xml_attrs(
        xml2::xml_find_first(process_xml,
                             "/process/secondary_variables/secondary_variable"))

    expect_equal(attrs, c(internal_name = "sigma_xx", output_name = "sigma_xx"))
})
