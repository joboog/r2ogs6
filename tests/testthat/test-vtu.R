
# helper function to skip tests if we don't have python dependencies
skip_if_python_modules_missing <- function() {

    used_modules <- c("vtk",
                      "vtk.numpy_interface.dataset_adapter",
                      "zlib")

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

    expect_equal(timestep, "99.9999999999986")
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

    expect_equal(length(time_tibble), 2)
})


#===== OGS6_vtu =====


test_that("read_in_vtu works", {

    vtu_path <- system.file("extdata/benchmarks/flow_free_expansion",
                            "cube_1x1x1.vtu",
                            package = "r2ogs6")

    ogs6_vtu <- read_in_vtu(vtu_path = vtu_path)

    expect_equal("vtkmodules.vtkCommonDataModel.vtkUnstructuredGrid" %in%
                     class(ogs6_vtu$vtkUnstructuredGrid), TRUE)
})

#===== generate_structured_mesh =====


#Add test...


#===== read_in_PointData_DataArray =====


test_that("read_in_PointData_DataArray works", {

    skip_if_python_modules_missing()

    vtu_path <- system.file("extdata/benchmarks/flow_free_expansion",
                            "flow_free_expansion_ts_1000_t_10000.000000.vtu",
                            package = "r2ogs6")

    vtu_obj <- read_in_vtu(vtu_path)
    pd_data_array <- vtu_obj$get_PointData_DataArray(Name = "HydraulicFlow")

    expect_equal(class(pd_data_array), "array")
})


#===== General .vtk library tests =====


test_that("zlib decompressing works as expected", {

    skip_if_python_modules_missing()

    py_env <- reticulate::py_run_string(
        paste(
            "import zlib",
            "test_data = bytearray('123', 'utf-8')",
            "compr_data = zlib.compress(test_data)",
            "decompr_data = zlib.decompress(compr_data)",
            "check = test_data.decode('utf-8') == decompr_data.decode('utf-8')",
            sep = "\n"
        ),
        convert = TRUE
    )

    expect_equal(py_env$check, TRUE)
})
