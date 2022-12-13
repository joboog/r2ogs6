
test_that("export_prj works for OGS6_gml", {

    # Get extdata directory and create folder for the test
    extdata_path <- system.file("extdata/", package = "r2ogs6")
    test_path <- paste0(extdata_path, "/export_prj_test")
    dir.create(test_path)

    # Define prj_path and OGS6 object, then read in .prj file
    ogs6_obj <- OGS6$new(sim_name = "flow_free_expansion",
                         sim_path = test_path)

    prj_path <- (system.file("extdata/benchmarks/flow_free_expansion",
                             "flow_free_expansion.prj", package = "r2ogs6"))

    read_in_prj(ogs6_obj, prj_path, read_in_gml = T)

    # test export of prj and gml only
    export_prj(ogs6_obj, copy_ext_files = F)
    expect_equal(sort(list.files(ogs6_obj$sim_path)),
                 sort(c("flow_free_expansion.gml", "flow_free_expansion.prj")))
    file.remove(paste0(ogs6_obj$sim_path, "flow_free_expansion.gml"))
    file.remove(paste0(ogs6_obj$sim_path, "flow_free_expansion.prj"))

    export_prj(ogs6_obj, copy_ext_files = T)
    expect_equal(sort(list.files(ogs6_obj$sim_path)),
                 sort(c("cube_1x1x1_quad.vtu", "flow_free_expansion.gml",
                   "flow_free_expansion.prj")))

    # Tidy up by deleting the folder we created
    unlink(test_path, recursive = TRUE)
})

test_that("export_prj works for referenced *.gml, *.vtu, *.py files", {

    # Get extdata directory and create folder for the test
    extdata_path <- system.file("extdata/", package = "r2ogs6")
    test_path <- paste0(extdata_path, "/export_prj_test")
    dir.create(test_path)

    # Define prj_path and OGS6 object, then read in .prj file
    ogs6_obj <- OGS6$new(sim_name = "square_1e3_laplace_eq",
                         sim_path = test_path)

    prj_path <- system.file(
        "extdata/benchmarks/square_1x1_SteadyStateDiffusion_Python",
        "square_1e3_laplace_eq.prj", package = "r2ogs6")

    read_in_prj(ogs6_obj, prj_path, read_in_gml = F)

    # test export of prj only
    export_prj(ogs6_obj, copy_ext_files = F)
    expect_equal(list.files(ogs6_obj$sim_path), "square_1e3_laplace_eq.prj")

    file.remove(paste0(ogs6_obj$sim_path, "square_1e3_laplace_eq.prj"))

    # test with copying referenced files
    export_prj(ogs6_obj, copy_ext_files = T)
    expect_equal(sort(list.files(ogs6_obj$sim_path)),
                 sort(c("bcs_laplace_eq.py", "square_1e3_laplace_eq.prj",
                   "square_1x1_quad_1e3.vtu", "square_1x1.gml")))

    # Tidy up by deleting the folder we created
    unlink(test_path, recursive = TRUE)
})

test_that("export_prj works for referenced *.dat files", {

    # Get extdata directory and create folder for the test
    extdata_path <- system.file("extdata/", package = "r2ogs6")
    test_path <- paste0(extdata_path, "/export_prj_test")
    dir.create(test_path)

    # Define prj_path and OGS6 object, then read in .prj file
    ogs6_obj <- OGS6$new(sim_name = "exchange",
                         sim_path = test_path)

    prj_path <- system.file("extdata/benchmarks/CationExchange",
                                 "exchange.prj", package = "r2ogs6")

    read_in_prj(ogs6_obj, prj_path, read_in_gml = F)

    # test export of prj only
    export_prj(ogs6_obj, copy_ext_files = F)
    expect_equal(list.files(ogs6_obj$sim_path), "exchange.prj")

    file.remove(paste0(ogs6_obj$sim_path, "exchange.prj"))

    # test with copying referenced files
    export_prj(ogs6_obj, copy_ext_files = T)
    expect_equal(sort(list.files(ogs6_obj$sim_path)),
                 sort(c("exchange_downstream.vtu", "exchange_upstream.vtu",
                   "exchange.prj", "exchange.vtu", "phreeqc.dat",
                   "ReactiveDomain_exchange.vtu")))

    # Tidy up by deleting the folder we created
    unlink(test_path, recursive = TRUE)
})

test_that("export_prj works for referenced *.msh files", {

    # Get extdata directory and create folder for the test
    extdata_path <- system.file("extdata/", package = "r2ogs6")
    test_path <- paste0(extdata_path, "/export_prj_test")
    dir.create(test_path)

    # Define prj_path and OGS6 object, then read in .prj file
    ogs6_obj <- OGS6$new(sim_name = "t1_1Dsource",
                         sim_path = test_path)

    prj_path <- system.file("extdata/benchmarks/t1_1Dsource",
                            "t1_1Dsource.prj", package = "r2ogs6")

    suppressWarnings(read_in_prj(ogs6_obj, prj_path, read_in_gml = F))

    # test with copying referenced files
    export_prj(ogs6_obj, copy_ext_files = T)
    expect_equal(sort(list.files(ogs6_obj$sim_path)),
                 sort(c("t1_1Dsource.gml", "t1_1Dsource.msh",
                        "t1_1Dsource.prj")))

    # Tidy up by deleting the folder we created
    unlink(test_path, recursive = TRUE)
})



test_that("export_prj works for /*$include", {

    # Get extdata directory and create folder for the test
    extdata_path <- system.file("extdata/", package = "r2ogs6")
    test_path <- paste0(extdata_path, "/export_prj_test")
    dir.create(test_path)

    # Define prj_path and OGS6 object, then read in .prj file
    ogs6_obj <- OGS6$new(sim_name = "circle_1e1_axi",
                         sim_path = test_path)

    prj_path <- (system.file("extdata/benchmarks/Elliptic/circle_radius_1",
                             "circle_1e1_axi.prj", package = "r2ogs6"))
    incl_path <- (system.file("extdata/benchmarks/Elliptic/other_dir",
                              "SteadyStateDiffusion.xml", package = "r2ogs6"))

    read_in_prj(ogs6_obj, prj_path, read_in_gml = F)

    # test export of prj only
    export_prj(ogs6_obj, copy_ext_files = F)
    expect_equal(list.files(ogs6_obj$sim_path), "circle_1e1_axi.prj")

    file.remove(paste0(ogs6_obj$sim_path, "circle_1e1_axi.prj"))

    # test with copying referenced files
    export_prj(ogs6_obj, copy_ext_files = T)
    expect_equal(sort(list.files(ogs6_obj$sim_path)),
                 sort(c("circle_1_axi.gml", "circle_1e1_axi.prj",
                   "line_1_lines_1e1.vtu", "SteadyStateDiffusion.xml")))

    # Tidy up by deleting the folder we created
    unlink(test_path, recursive = TRUE)
})

test_that("export_prj works for top-level include", {

    # Get extdata directory and create folder for the test
    extdata_path <- system.file("extdata/", package = "r2ogs6")
    test_path <- paste0(extdata_path, "/export_prj_test")
    dir.create(test_path)

    # Define prj_path and OGS6 object, then read in .prj file
    ogs6_obj <- OGS6$new(sim_name = "x_strain_y_flow",
                         sim_path = test_path)
    prj_path <-
        system.file(
            "extdata/benchmarks/OrthotropicEmbeddedFracturePermeability",
            "x_strain_y_flow.prj", package = "r2ogs6")
    incl_path <-
        system.file(
            "extdata/benchmarks/OrthotropicEmbeddedFracturePermeability",
            "cube_strain_flow_data.include", package = "r2ogs6")

    # tests without reading the include
    read_in_prj(ogs6_obj, prj_path, read_in_gml = F)
    # export of prj only
    export_prj(ogs6_obj, copy_ext_files = F)
    expect_equal(list.files(ogs6_obj$sim_path), "x_strain_y_flow.prj")
    file.remove(paste0(ogs6_obj$sim_path, "x_strain_y_flow.prj"))
    # export with copying referenced files
    export_prj(ogs6_obj, copy_ext_files = T)
    expect_equal(sort(list.files(ogs6_obj$sim_path)),
                 sort(c("x_strain_y_flow.prj","cube_strain_flow_data.include")))
    file.remove(paste0(ogs6_obj$sim_path, "x_strain_y_flow.prj"))
    file.remove(paste0(ogs6_obj$sim_path, "cube_strain_flow_data.include"))


    # tests with reading include
    rm(ogs6_obj)
    ref_path <-
        system.file(
            "extdata/benchmarks/OrthotropicEmbeddedFracturePermeability",
            "ref_x_strain_y_flow.prj", package = "r2ogs6")

    ogs6_obj <- OGS6$new(sim_name = "x_strain_y_flow", sim_path = test_path)
    read_in_prj(ogs6_obj, prj_path, read_in_gml = F, read_includes = T)
    # export of prj only
    export_prj(ogs6_obj, copy_ext_files = F)
    ref_xml <- xml2::read_xml(ref_path)
    test_xml <- xml2::read_xml(paste0(ogs6_obj$sim_path,
                                     "x_strain_y_flow.prj"))
    ref <- xml2::as_list(ref_xml)
    test <- xml2::as_list(test_xml)
    test$OpenGeoSysProject$mesh[[1]] <-
        basename(test$OpenGeoSysProject$mesh[[1]])
    test$OpenGeoSysProject$geometry[[1]] <-
        basename(test$OpenGeoSysProject$geometry[[1]])
    expect_equal(ref, test)
    expect_equal(list.files(ogs6_obj$sim_path), "x_strain_y_flow.prj")
    file.remove(paste0(ogs6_obj$sim_path, "x_strain_y_flow.prj"))

    # export with copying referenced files
    export_prj(ogs6_obj, copy_ext_files = T)
    expect_equal(sort(list.files(ogs6_obj$sim_path)),
                 sort(c("cube_1x1x1.gml", "x_strain_y_flow.prj",
                        "cube_1x1x1_quad.vtu")))

    # Tidy up by deleting the folder we created
    unlink(test_path, recursive = TRUE)
})