test_that("validate_points function checks ...", {

    point_list <- list(x = c(0, 0), y = c(1, 1), z = c(0, 1))

    #Check class (should expect a tibble, not a list)
    expect_error(validate_points(point_list))

    point_tibble_inv_0 <- tibble::tibble(a = c(0, 0), b = c(1, 1), c = c(0, 1))
    point_tibble_inv_1 <- tibble::tibble(x = c(0, 0), y = c(1, 1), z = c(0))
    point_tibble_inv_2 <- tibble::tibble(x = c(0, 0), y = c(1, 1), z = c(0, 1), name = c("oh", "oh"))

    #Check column names
    expect_error(validate_points(point_tibble_inv_0))

    #Check if duplicate points and point names are detected
    expect_error(validate_points(point_tibble_inv_1))
    expect_warning(validate_points(point_tibble_inv_2))

    point_tibble_val <- tibble::tibble(x = c(0, 0), y = c(1, 1), z = c(0, 1), name = c("this", "works"))
    point_tibble_val_2 <- tibble::tibble(x = c(0, 0), y = c(1, 1), z = c(0, 1))

    expect_invisible(validate_points(point_tibble_val))
    expect_invisible(validate_points(point_tibble_val_2))
})
