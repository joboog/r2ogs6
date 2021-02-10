


test_that("OGS6$clear() works as expected", {

    ogs6_obj <- OGS6$new(
        sim_name = "sim",
        sim_path = "sim_path")

    ogs6_obj$add(r2ogs6_parameter(
        name = "pressure0",
        type = "Constant",
        values = 1e5
    ))

    expect_equal(length(ogs6_obj$parameters), 1)
    expect_warning(ogs6_obj$clear(c("elephant", "parameters")))
    expect_equal(length(ogs6_obj$parameters), 0)
})


test_that("OGS6$add() works", {

    ogs6_obj <- OGS6$new(
        sim_name = "sim",
        sim_path = "sim_path")

    ogs6_obj$add(r2ogs6_parameter(
        name = "pressure0",
        type = "Constant",
        values = 1e5
    ))

    expect_error(ogs6_obj$add("my_script.py"))
    expect_equal(length(ogs6_obj$parameters), 1)
    expect_equal(ogs6_obj$parameters[[1]]$values, 1e5)
})

