


test_that("OGS6_Ensemble initialization works", {

    ogs6_obj <- OGS6$new(
        sim_name = "sim",
        sim_id = 1,
        sim_path = "sim_path",
        ogs_bin_path = "ogs_bin_path",
        test_mode = TRUE
    )

    ogs6_obj$add_parameter(r2ogs6_parameter(
        name = "pressure0",
        type = "Constant",
        value = 1
    ))


    ogs6_ens <- OGS6_Ensemble$new(
        ogs6_obj = ogs6_obj,
        parameters = list(list(ogs6_obj$parameters[[1]]$value, c(2, 3, 4)))
    )

    expect_equal(length(ogs6_ens$ensemble), 4)
    expect_equal(ogs6_ens$ensemble[[2]]$parameters[[1]]$value, 2)
    expect_equal(ogs6_ens$ensemble[[3]]$parameters[[1]]$value, 3)
    expect_equal(ogs6_ens$ensemble[[4]]$parameters[[1]]$value, 4)

    expect_error(OGS6_Ensemble$new(
        ogs6_obj = ogs6_obj,
        parameters = list(list(ogs6_obj$parameters[[1]]$value, c(2, 3, 4)),
                          list(ogs6_obj$parameters[[1]]$type, c(2)))
    ))

    expect_error(OGS6_Ensemble$new(
        ogs6_obj = ogs6_obj,
        parameters = list(list(ogs6_obj$parameters[[1]]$curve, c("a", "b")))
    ))
})
