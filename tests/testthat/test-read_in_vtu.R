

test_that("read_in_vtu works", {

    vtu_path <- system.file("extdata/flow_free_expansion",
                            "cube_1x1x1.vtu",
                            package = "r2ogs6")

    vtu_obj <- read_in_vtu(vtu_path = vtu_path)

    expect_equal(vtu_obj$UnstructuredGrid$Piece$NumberOfPoints, "8")
})
