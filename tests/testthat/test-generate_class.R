
test_that("flags_to_con_str works", {

    flags <- c(a = TRUE, b = TRUE, c = FALSE)

    str <- flags_to_con_str(flags)

    expect_equal(str, "a,\nb,\nc = NULL")
})
