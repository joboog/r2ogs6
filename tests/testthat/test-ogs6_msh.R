test_that("OGS6_msh initialization works", {

    msh_path <- system.file("extdata/benchmarks/t1_1Dsource",
                            "t1_1Dsource.msh",
                            package = "r2ogs6")

    msh_obj <- OGS6_msh$new(msh_path = msh_path)

    expect_equal(class(msh_obj)[1], "OGS6_msh")
    expect_equal(class(msh_obj$msh_path), "character")
    expect_true(grepl("\\.msh$", msh_obj$msh_path))
})