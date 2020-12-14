test_that("validate_points function checks if input is a tibble with the right contents", {

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


test_that("validate_polylines function checks if input is a list with the right contents", {

    polyline_tibble <- tibble::tibble(name = "tibble", polyline = c(c(1, 2)))

    #Check class (should expect a list, not a tibble)
    expect_error(validate_polylines(polyline_tibble))

    polyline_list_inv_0 <- list(name = "missing_polyline_list", c(1,2))
    polyline_list_inv_1 <- list(list(name = "wrong_length", c(0, 1, 2), c(1, 2)))
    polyline_list_inv_2 <- list(list(name = 42, c(1, 2)))
    polyline_list_inv_3 <- list(list(name = "wrong_points", c("this", "should", "fail")))

    expect_error(validate_polylines(polyline_list_inv_0))
    expect_error(validate_polylines(polyline_list_inv_1))
    expect_error(validate_polylines(polyline_list_inv_2))
    expect_error(validate_polylines(polyline_list_inv_3))

    polyline_list_val_0 <- list(list(name = "cool", c(1, 2)))
    polyline_list_val_1 <- list(list(name = "also cool", c(1, 2, 4, 5, 1)))

    expect_invisible(validate_polylines(polyline_list_val_0))
    expect_invisible(validate_polylines(polyline_list_val_1))

    #...(WIP)

})


test_that("validate_surfaces function checks if input is a list with the right contents", {

    surface_tibble <- tibble::tibble(name = "tibble", surface = c(c(0, 1, 2), c(1, 2, 3)))

    #Check class (should expect a list, not a tibble)
    expect_error(validate_surfaces(surface_tibble))

    #...(WIP)
})