
test_that("new_r2ogs6_parameter basic validation is working", {

    expect_error(r2ogs6_parameter("my_param", c(0, 0), c(0, 0)))
    expect_error(r2ogs6_parameter(c(0, 0), "my_type", c(0, 0)))

})
