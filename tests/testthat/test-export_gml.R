

# test_that("export_gml works", {
#
#     test_gml <- r2ogs6_gml(
#         name = "test_geometry",
#         points = tibble::tibble(
#             x = c(0, 0, 0, 0, 1, 1, 1, 1),
#             y = c(0, 0, 1, 1, 0, 0, 1, 1),
#             z = c(0, 1, 1, 0, 0, 1, 1, 0),
#             name = c("origin", "", "", "", "", "", "", "")
#         ),
#         polylines = list(list(name = "test", c(1, 2, 3, 4, 1))),
#         surfaces = list(list(name = "test", c(1, 2, 3), c(2, 3, 4)))
#     )
#
#     path <- validate_is_dir_path(system.file("extdata/", package = "r2ogs6"))
#
#     filename <- paste0(path, "test_geometry.gml")
#
#     export_gml(test_gml, path)
#
#     expect_equal(file.exists(filename), TRUE)
#
#     #unlink(filename)
# })


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
