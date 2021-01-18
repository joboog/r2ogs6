
# helper function to skip tests if we don't have python dependencies
skip_if_python_modules_missing <- function() {

    used_modules <- c("vtk",
                      "vtk.numpy_interface.dataset_adapter")

    lapply(used_modules, function(x){
        if(!reticulate::py_module_available(x)){
            skip(paste(x, "not available for testing"))
        }
    })
}


#===== OGS6_pvd =====


test_that("OGS6_pvd initialization works", {

    pvd_path <- system.file("extdata/benchmarks/flow_no_strain",
                            "flow_no_strain.pvd",
                            package = "r2ogs6")

    ogs6_pvd <- OGS6_pvd$new(pvd_path)

    expect_equal(length(ogs6_pvd$vtu_paths), 2)
    expect_equal(ogs6_pvd$vtu_paths[[1]],
                 "flow_no_strain_ts_0_t_0.000000.vtu")
    expect_equal(ogs6_pvd$vtu_paths[[2]],
                 "flow_no_strain_ts_1000_t_100.000000.vtu")
})


test_that("OGS6_pvd$get_timestep_by_vtu_path works", {

    pvd_path <- system.file("extdata/benchmarks/flow_no_strain",
                            "flow_no_strain.pvd",
                            package = "r2ogs6")

    ogs6_pvd <- OGS6_pvd$new(pvd_path)

    vtu_path <- "flow_no_strain_ts_1000_t_100.000000.vtu"

    timestep <- ogs6_pvd$get_timestep_by_vtu_path(vtu_path = vtu_path)

    expect_equal(timestep, 99.9999999999986)
})


test_that("OGS6_pvd$get_PointData_time_tibble works", {

    skip_if_python_modules_missing()

    pvd_path <- system.file("extdata/benchmarks/flow_no_strain",
                            "flow_no_strain.pvd",
                            package = "r2ogs6")

    ogs6_pvd <- OGS6_pvd$new(pvd_path)

    Names <- c("pressure",
               "velocity")

    time_tibble <- ogs6_pvd$get_PointData_time_tibble(Names = Names)
    expect_equal(length(time_tibble), 341)
    expect_equal(length(time_tibble[[1]]), 2)
})


test_that("OGS6_pvd$get_PointData_at_timestep works", {

    skip_if_python_modules_missing()

    pvd_path <- system.file("extdata/benchmarks/flow_no_strain",
                            "flow_no_strain.pvd",
                            package = "r2ogs6")

    ogs6_pvd <- OGS6_pvd$new(pvd_path)

    point_data <- ogs6_pvd$get_PointData_at_timestep(point_ids = c(0, 1, 2),
                                                     Names = "HydraulicFlow",
                                                     timestep = 0)

    expect_equal(length(point_data),3)
    expect_equal(names(point_data), c("p0", "p1", "p2"))
})


#===== OGS6_vtu =====


test_that("OGS6_vtu initialization works", {

    vtu_path <- system.file("extdata/benchmarks/flow_free_expansion",
                            "cube_1x1x1.vtu",
                            package = "r2ogs6")

    vtu_obj <- OGS6_vtu$new(vtu_path = vtu_path)

    expect_equal("vtkmodules.vtkCommonDataModel.vtkUnstructuredGrid" %in%
                     class(vtu_obj$vtkUnstructuredGrid), TRUE)
})


test_that("OGS6_vtu$get_data_for_points works", {

    vtu_path <- system.file("extdata/benchmarks/flow_no_strain",
                            "flow_no_strain_ts_1000_t_100.000000.vtu",
                            package = "r2ogs6")

    vtu_obj <- OGS6_vtu$new(vtu_path = vtu_path)

    point_data <- vtu_obj$get_data_for_points(c(0, 1, 2), "HydraulicFlow")
    expect_equal(length(point_data), 3)
})


#===== generate_structured_mesh =====


#Add test...


#===== OGS6_vtu$point_data has expected format =====


test_that("point_data works", {

    skip_if_python_modules_missing()

    vtu_path <- system.file("extdata/benchmarks/flow_free_expansion",
                            "flow_free_expansion_ts_1000_t_10000.000000.vtu",
                            package = "r2ogs6")

    vtu_obj <- OGS6_vtu$new(vtu_path = vtu_path)
    point_data <- vtu_obj$point_data[["HydraulicFlow"]]
    expect_equal(class(point_data), "array")
})
