
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
})


test_that("OGS6_pvd$get_PointData_at_timestep works", {

    skip_if_python_modules_missing()

    pvd_path <- system.file("extdata/benchmarks/flow_no_strain",
                            "flow_no_strain.pvd",
                            package = "r2ogs6")

    ogs6_pvd <- OGS6_pvd$new(pvd_path)

    point_data <- ogs6_pvd$get_PointData_at_timestep(point_ids = 0,
                                                     Names = "HydraulicFlow",
                                                     timestep = 0)

    expect_equal(length(point_data), 1)
    expect_equal(names(point_data), "p0")
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


#===== generate_structured_mesh =====


#Add test...


#===== get_PointData =====


test_that("get_PointData works", {

    skip_if_python_modules_missing()

    vtu_path <- system.file("extdata/benchmarks/flow_free_expansion",
                            "flow_free_expansion_ts_1000_t_10000.000000.vtu",
                            package = "r2ogs6")

    vtu_obj <- OGS6_vtu$new(vtu_path = vtu_path)

    pd_data_array <- vtu_obj$get_PointData(Name = "HydraulicFlow")

    expect_equal(class(pd_data_array), "array")
})
