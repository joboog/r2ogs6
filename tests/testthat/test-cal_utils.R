
ogs6_obj <- OGS6$new(sim_name = "theis", tmp_dir)

prj_path <- system.file("extdata/benchmarks/AxiSymTheis",
                        "axisym_theis.prj", package = "r2ogs6")

read_in_prj(ogs6_obj, prj_path, read_in_gml = T)

test_that("Only existing components can be changed", {
    expect_error(ogs6_obj$update_component(
        list(list("ogs6_obj$media[[1]]$materials", 0.003))
        ))
    expect_error(ogs6_obj$update_component(
        list(list("ogs6_obj$media[[1]$properties", 0.003))
    ))
})

test_that("Only accepts parameters in right format", {
    expect_error(ogs6_obj$update_component(
        list("ogs6_obj$media[[1]]$properties", 0.003)
    ))
    expect_error(ogs6_obj$update_component(
        list(c("ogs6_obj$media[[1]]$properties", 0.003),
        c("ogs6_obj$media[[1]]$phases[[1]]$properties[[1]]$value", 0.03))
    ))

    expect_silent(ogs6_obj$update_component(
        list(list("ogs6_obj$media[[1]]$properties", 0.003),
             list("ogs6_obj$media[[1]]$phases[[1]]$properties[[1]]$value", 0.03))
    ))
})
