
test_that("new_prj_parameter basic validation is working", {

    expect_error(prj_parameter("my_param", c(0, 0), c(0, 0)))
    expect_error(prj_parameter(c(0, 0), "my_type", c(0, 0)))

})
