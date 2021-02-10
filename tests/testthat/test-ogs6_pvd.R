

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

    timestep <- ogs6_pvd$timestep_by_vtu(vtu_path = vtu_path)

    expect_equal(timestep, 99.9999999999986)
})


test_that("OGS6_pvd$get_point_data_at works", {

    pvd_path <- system.file("extdata/benchmarks/flow_no_strain",
                            "flow_no_strain.pvd",
                            package = "r2ogs6")

    ogs6_pvd <- OGS6_pvd$new(pvd_path)


    tbl_from_id <- ogs6_pvd$get_point_data(0,
                                           keys = c("epsilon_xx",
                                                     "epsilon_xy"))

    # Test for DataArray where NumberOfComponents == 1
    tbl_from_coords <- ogs6_pvd$get_point_data_at(coordinates = c(0, 0, 0),
                                                  keys = c("epsilon_xx",
                                                            "epsilon_xy"))

    expect_equal(tbl_from_coords$epsilon_xx, tbl_from_id$epsilon_xx)
    expect_equal(tbl_from_coords$epsilon_xy, tbl_from_id$epsilon_xy)
})


test_that("OGS6_pvd$get_point_data works", {

    pvd_path <- system.file("extdata/benchmarks/flow_no_strain",
                            "flow_no_strain.pvd",
                            package = "r2ogs6")

    ogs6_pvd <- OGS6_pvd$new(pvd_path)

    # Test for DataArray where NumberOfComponents == 1
    tbl_simple <- ogs6_pvd$get_point_data(point_ids = c(0, 1, 2),
                                          keys = c("epsilon_xx",
                                                    "epsilon_xy"))

    expect_equal(nrow(tbl_simple), 6)

    # Test for DataArray where NumberOfComponents(displacement) == 2
    tbl_nocomp <- ogs6_pvd$get_point_data(point_ids = c(0, 1, 2),
                                          keys = c("displacement",
                                                    "epsilon_xx"))

    expect_equal(nrow(tbl_nocomp), 6)
})


test_that("OGS6_pvd$get_cell_data works", {

    pvd_path <- system.file("extdata/benchmarks/flow_no_strain",
                            "flow_no_strain.pvd",
                            package = "r2ogs6")

    ogs6_pvd <- OGS6_pvd$new(pvd_path)

    # Test for DataArray where NumberOfComponents(displacement) == 2
    tbl_nocomp <- ogs6_pvd$get_cell_data(
        cell_ids = c(0, 1, 2),
        keys = c("permeability",
                  "principal_stress_values"))

    expect_equal(nrow(tbl_nocomp), 6)
})
