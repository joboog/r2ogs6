test_that("validate_points works", {

    point_list <- list(x = c(0, 0), y = c(1, 1), z = c(0, 1))

    #Check class (should expect a tibble, not a list)
    expect_error(validate_points(point_list))

    point_tibble_inv_0 <- tibble::tibble(a = c(0, 0), b = c(1, 1), c = c(0, 1))
    point_tibble_inv_1 <- tibble::tibble(x = c(0, 0), y = c(1, 1), z = c(0))
    point_tibble_inv_2 <- tibble::tibble(x = c(0, 0), y = c(1, 1), z = c(0, 1),
                                         name = c("oh", "oh"))

    #Check column names
    expect_error(validate_points(point_tibble_inv_0))

    #Check if duplicate points and point names are detected
    expect_error(validate_points(point_tibble_inv_1))
    expect_warning(validate_points(point_tibble_inv_2))

    point_tibble_val <- tibble::tibble(x = c(0, 0), y = c(1, 1), z = c(0, 1),
                                       name = c("this", "works"))
    point_tibble_val_2 <- tibble::tibble(x = c(0, 0), y = c(1, 1), z = c(0, 1))

    expect_invisible(validate_points(point_tibble_val))
    expect_invisible(validate_points(point_tibble_val_2))
})


test_that("validate_polylines works", {

    polyline_list_inv_1 <- list(list(name = "wrong_length",
                                     c(0, 1),
                                     c(1, 2)))
    polyline_list_inv_2 <- list(list(name = "wrong_points",
                                     c("a", "b", "c")))

    expect_error(validate_polylines(polyline_list_inv_1))
    expect_error(validate_polylines(polyline_list_inv_2))

    polyline_list_val_0 <- list(list(name = "cool",
                                     c(1, 2)))
    polyline_list_val_1 <- list(list(name = "also cool",
                                     c(1, 2, 4, 5, 1)))

    expect_invisible(validate_polylines(polyline_list_val_0))
    expect_invisible(validate_polylines(polyline_list_val_1))

    polylines = list(
        list("front_left",
             c(0, 1))
        )

    polylines <- validate_polylines(polylines)

    expect_equal(names(polylines), "polyline")
    expect_equal(names(polylines[[1]])[[1]], "name")
})


test_that("validate_surfaces works", {

    surface_tibble <- tibble::tibble(name = "tibble",
                                     surface = c(c(0, 1, 2), c(1, 2, 3)))

    #Check class (should expect a list, not a tibble)
    expect_error(validate_surfaces(surface_tibble))

    #...(WIP)
})