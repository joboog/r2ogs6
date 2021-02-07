
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


#===== OGS6_vtu =====


test_that("OGS6_vtu initialization works", {

    vtu_path <- system.file("extdata/benchmarks/flow_free_expansion",
                            "cube_1x1x1.vtu",
                            package = "r2ogs6")

    vtu_obj <- OGS6_vtu$new(vtu_path = vtu_path)

    expect_equal("vtkmodules.vtkCommonDataModel.vtkUnstructuredGrid" %in%
                     class(vtu_obj$vtkUnstructuredGrid), TRUE)

    expect_equal(vtu_obj$number_of_points, 8)
    expect_equal(vtu_obj$number_of_cells, 1)

    expect_equal(dim(vtu_obj$points), as.integer(c(8, 3)))
})


test_that("OGS6_vtu$get_field_data works", {

    vtu_path <- system.file("extdata/benchmarks/flow_no_strain",
                            "flow_no_strain_ts_1000_t_100.000000.vtu",
                            package = "r2ogs6")

    vtu_obj <- OGS6_vtu$new(vtu_path = vtu_path)

    field_data <- vtu_obj$get_field_data("epsilon_ip")
    expect_equal(length(field_data), 1)
})


test_that("OGS6_vtu$get_point_coords works", {

    vtu_path <- system.file("extdata/benchmarks/flow_free_expansion",
                            "cube_1x1x1.vtu",
                            package = "r2ogs6")

    vtu_obj <- OGS6_vtu$new(vtu_path = vtu_path)

    point_list <- vtu_obj$get_point_coords(c(0, 1))
    expect_equal(point_list, list(c(0, 0, 0), c(1, 0, 0)))
})


test_that("OGS6_vtu$get_point_data_at() works", {

    vtu_path <- system.file("extdata/benchmarks/flow_no_strain",
                            "flow_no_strain_ts_1000_t_100.000000.vtu",
                            package = "r2ogs6")

    vtu_obj <- OGS6_vtu$new(vtu_path = vtu_path)

    tbl <- vtu_obj$get_point_data_at(c(0.01, 0, 0),
                                     keys = "epsilon_xx")
    expect_equal(tbl$epsilon_xx, 3.282899e-15)


    bigger_tbl <- vtu_obj$get_point_data_at(list(c(0.01, 0, 0),
                                                 c(0.42, 0, 0)),
                                            keys = "epsilon_xx")
    expect_equal(bigger_tbl$epsilon_xx[[2]], 1.233661e-16)
})


test_that("OGS6_vtu$get_point_data works", {

    vtu_path <- system.file("extdata/benchmarks/flow_no_strain",
                            "flow_no_strain_ts_1000_t_100.000000.vtu",
                            package = "r2ogs6")

    vtu_obj <- OGS6_vtu$new(vtu_path = vtu_path)

    point_data <- vtu_obj$get_point_data(c(0, 1, 2), "HydraulicFlow")
    expect_equal(nrow(point_data), 3)
    expect_equal(ncol(point_data), 5)
})


test_that("OGS6_vtu$get_cell_data works", {

    vtu_path <- system.file("extdata/benchmarks/flow_no_strain",
                            "flow_no_strain_ts_1000_t_100.000000.vtu",
                            package = "r2ogs6")

    vtu_obj <- OGS6_vtu$new(vtu_path = vtu_path)

    cell_data <- vtu_obj$get_cell_data(c(0, 1), "permeability")
    expect_equal(nrow(cell_data), 2)
})


test_that("OGS6_vtu$vtkPointLocator works", {

    vtu_path <- system.file("extdata/benchmarks/flow_no_strain",
                            "flow_no_strain_ts_1000_t_100.000000.vtu",
                            package = "r2ogs6")

    vtu_obj <- OGS6_vtu$new(vtu_path = vtu_path)

    point_id <- vtu_obj$vtkPointLocator$FindClosestPoint(c(0.01, 0, 0))
    expect_equal(point_id, 0)
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
