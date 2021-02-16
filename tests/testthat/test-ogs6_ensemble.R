


test_that("OGS6_Ensemble initialization works", {

    ogs6_obj <- OGS6$new(
        sim_name = "sim",
        sim_path = "sim_path")

    ogs6_obj$add(prj_parameter(
        name = "pressure0",
        type = "Constant",
        value = 1
    ))

    ogs6_obj$add(prj_parameter(
        name = "pressure1",
        type = "Constant",
        value = 4
    ))

    # Test with sequential_mode and percentages_mode off
    ogs6_ens_noseq_noper <- OGS6_Ensemble$new(
        ogs6_obj = ogs6_obj,
        parameters = list(list(ogs6_obj$parameters[[1]]$value, c(2, 3)),
                          list(ogs6_obj$parameters[[2]]$value, c(5, 6))),
        sequential_mode = FALSE,
        percentages_mode = FALSE
    )

    expect_equal(length(ogs6_ens_noseq_noper$ensemble), 2)
    expect_equal(ogs6_ens_noseq_noper$ensemble[[1]]$parameters[[1]]$value, 2)
    expect_equal(ogs6_ens_noseq_noper$ensemble[[2]]$parameters[[1]]$value, 3)
    expect_equal(ogs6_ens_noseq_noper$ensemble[[1]]$parameters[[2]]$value, 5)
    expect_equal(ogs6_ens_noseq_noper$ensemble[[2]]$parameters[[2]]$value, 6)

    # Test with sequential_mode on and percentages_mode off
    ogs6_ens_seq_noper <- OGS6_Ensemble$new(
        ogs6_obj = ogs6_obj,
        parameters = list(a = list(ogs6_obj$parameters[[1]]$value, c(2, 3)),
                          b = list(ogs6_obj$parameters[[2]]$value, c(5, 6))),
        sequential_mode = TRUE,
        percentages_mode = FALSE
    )

    expect_equal(length(ogs6_ens_seq_noper$ensemble), 4)
    expect_equal(ogs6_ens_seq_noper$ensemble[[4]]$parameters[[2]]$value, 6)


    # Test with sequential_mode off and percentages_mode on
    ogs6_ens_noseq_per <- OGS6_Ensemble$new(
        ogs6_obj = ogs6_obj,
        parameters = list(list(ogs6_obj$parameters[[1]]$value, c(50, -75)),
                          list(ogs6_obj$parameters[[2]]$value, c(50, -75))),
        sequential_mode = FALSE,
        percentages_mode = TRUE
    )

    expect_equal(length(ogs6_ens_noseq_per$ensemble), 2)
    expect_equal(ogs6_ens_noseq_per$ensemble[[1]]$parameters[[1]]$value, 1.5)
    expect_equal(ogs6_ens_noseq_per$ensemble[[2]]$parameters[[2]]$value, 1)
})


test_that("ogs6_get_combinations works", {

    a <- c(1, 2, 3, 4)
    b <- c("a", "b", "c")
    c <- c("+", "-")

    combinations <- ogs6_get_combinations(a, b, c)
    a_long <- combinations[[1]]
    b_long <- combinations[[2]]
    c_long <- combinations[[3]]

    expect_equal(length(a_long), 24)
    expect_equal(length(b_long), 24)
    expect_equal(length(c_long), 24)
})

