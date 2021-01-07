

test_that("export_gml works", {

    # Get extdata directory and create folder for the test
    extdata_path <- system.file("extdata/", package = "r2ogs6")
    test_path <- paste0(extdata_path, "/export_gml_test")
    dir.create(test_path)

    # Define gml object
    test_gml <- r2ogs6_gml(
        name = "test_geometry",
        points = tibble::tibble(
            x = c(0, 0, 0, 0, 1, 1, 1, 1),
            y = c(0, 0, 1, 1, 0, 0, 1, 1),
            z = c(0, 1, 1, 0, 0, 1, 1, 0),
            name = c("origin", "", "", "", "", "", "", "")
        ),
        polylines = list(list(name = "test", c(1, 2, 3, 4, 1))),
        surfaces = list(list(name = "test", c(1, 2, 3), c(2, 3, 4)))
    )

    gml_path <- paste0(test_path, "/test_geometry.gml")

    # Now export it
    export_gml(test_gml, gml_path)

    expect_equal(file.exists(gml_path),
                 TRUE)

    # Tidy up by deleting the folder we created
    unlink(test_path, recursive = TRUE)
})


test_that("converting tibble to list works", {

    points = tibble::tibble(
            x = c(0, 0, 0, 0, 1, 1, 1, 1),
            y = c(0, 0, 1, 1, 0, 0, 1, 1),
            z = c(0, 1, 1, 0, 0, 1, 1, 0),
            name = c("origin", "", "", "", "", "", "", ""))

    points_list <- setNames(split(points,
                                  seq(nrow(points))),
                            rep("point", nrow(points)))

    points_list <- lapply(points_list, function(x){
        as.list(x)
    })

    expect_equal("tbl_df" %in% class(points_list[[1]]), FALSE)
})
