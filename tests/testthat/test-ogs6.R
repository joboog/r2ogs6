


test_that("OGS6 clear function works as expected", {

    ogs6_obj <- OGS6$new(sim_name = "sim",
                         sim_id = 1,
                         sim_path = "sim_path",
                         ogs_bin_path = "ogs_bin_path",
                         test_mode = TRUE)

    input_add(r2ogs6_parameter(name = "pressure0",
                               type = "Constant",
                               values = 1e5),
              ogs6_obj)

    expect_equal(length(ogs6_obj$parameters), 1)
    expect_warning(ogs6_obj$clear(c("pamameter", "parameters")))
    expect_equal(length(ogs6_obj$parameters), 0)
})