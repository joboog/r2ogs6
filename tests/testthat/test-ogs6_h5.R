helper_setup_h5 <- function(h5_file, group, time, geo,
                            varname = NULL,
                            var = NULL) {

    rhdf5::h5createFile(h5_file)
    rhdf5::h5createGroup(h5_file, group)
    if (!is.null(geo)) {
    rhdf5::h5write(geo, file = h5_file,
                   name = paste0(group, "/geometry"))
    }
    if (!is.null(time)) {
    rhdf5::h5write(time, file = h5_file,
                   name = "/times")
    }
    if (!is.null(var)) {
        rhdf5::h5write(var, file = h5_file,
                       name = varname)
    }
}

 test_that("transposed geometry is detected", {

    h5_dir <- paste0(tmp_dir, "/test-h5")
    dir.create(h5_dir)
    geo <- array(as.matrix(expand.grid(seq(0, 3, length.out = 10),
                                       seq(0, 2, length.out = 2),
                                       seq(0, 1, length.out = 1))),
                 c(20, 3, 1))
    # not transposed
    helper_setup_h5(paste0(h5_dir, "/test_1.h5"), group = "meshes",
                    time = c(0, 2),
                    geo)

    ogs6_h5 <- OGS6_h5$new(paste0(h5_dir, "/test_1.h5"))
    df <- ogs6_h5$get_df(group = "/meshes")
    expect_equal(dim(df), c(40, 4))

    # transposed
    geo_t <- array(t(geo[, , 1]), c(3, 20, 1))
    helper_setup_h5(paste0(h5_dir, "/test_2.h5"), group = "meshes",
                    time = c(0, 2),
                    geo_t)

    ogs6_h5 <- OGS6_h5$new(paste0(h5_dir, "/test_2.h5"))
    df <- ogs6_h5$get_df(group = "/meshes")
    expect_equal(dim(df), c(40, 4))

    unlink(h5_dir, recursive = TRUE)
})

test_that("Projects with no times field work", {
    h5_dir <- paste0(tmp_dir, "/test-h5")
    dir.create(h5_dir)
    geo <- array(as.matrix(expand.grid(seq(0, 3, length.out = 5),
                                       seq(0, 2, length.out = 1),
                                       seq(0, 1, length.out = 1))),
                 c(5, 3, 1))

    helper_setup_h5(paste0(h5_dir, "/test_1.h5"), group = "meshes",
                    time = NULL,
                    geo)
    ogs6_h5 <- OGS6_h5$new(paste0(h5_dir, "/test_1.h5"))
    expect_silent(df <- ogs6_h5$get_df(group = "/meshes"))
    expect_equal(dim(df), c(5, 4))

    unlink(h5_dir, recursive = TRUE)
})

test_that("Projects with no geometry field work", {
    h5_dir <- paste0(tmp_dir, "/test-h5")
    dir.create(h5_dir)

    helper_setup_h5(paste0(h5_dir, "/test_1.h5"), group = "meshes",
                    time = c(1, 2, 3, 4),
                    geo = NULL)
    ogs6_h5 <- OGS6_h5$new(paste0(h5_dir, "/test_1.h5"))
    expect_silent(df <- ogs6_h5$get_df(group = "/", names = "times"))
    expect_equal(dim(df), c(4, 4))

    unlink(h5_dir, recursive = TRUE)
})


test_that("Nonconformable arguments throw error", {
    h5_dir <- paste0(tmp_dir, "/test-h5")
    dir.create(h5_dir)
    geo <- array(as.matrix(expand.grid(seq(0, 3, length.out = 10),
                                       seq(0, 2, length.out = 2),
                                       seq(0, 1, length.out = 1))),
                 c(20, 3, 1))

    v <- matrix(seq(0, 10, length.out = 10),  ncol = 2)

    helper_setup_h5(paste0(h5_dir, "/test_1.h5"), group = "meshes",
                    time = c(0, 2),
                    geo,
                    varname = "/meshes/pressure",
                    var = v)

    ogs6_h5 <- OGS6_h5$new(paste0(h5_dir, "/test_1.h5"))

    expect_error({
        df <- ogs6_h5$get_df(group = "/meshes",
                             names = "pressure")
    })

    unlink(h5_dir, recursive = TRUE)
})


# tests with benchmark h5 files -------------------------------------------

test_that("benchmark h5 file cube_1e3.h5 passes", {
    h5_path <- system.file("/extdata/benchmarks/EllipticPETSc",
                           "cube_1e3.h5",
                           package = "r2ogs6")
    ogs6_h5 <- OGS6_h5$new(h5_path)
    expect_silent(df <- ogs6_h5$get_df("/t_0", "pressure"))
    expect_equal(dim(df), c(1895, 5))
    expect_equal(colnames(df), c("x", "y", "z", "time", "pressure"))
})

test_that("benchmark h5 file cube_1e3_np2.h5 passes", {
    h5_path <- system.file("/extdata/benchmarks/EllipticPETSc",
                           "cube_1e3_np2.h5",
                           package = "r2ogs6")
    ogs6_h5 <- OGS6_h5$new(h5_path)
    expect_silent(df <- ogs6_h5$get_df("/meshes/cube_1x1x1_hex_1e3",
                                       "pressure"))
    expect_equal(dim(df), c(3274, 5))
    expect_equal(colnames(df), c("x", "y", "z", "time", "pressure"))
})

test_that("benchmark h5 file cube_1e3_np2.h5 passes", {
    h5_path <- system.file("/extdata/benchmarks/EllipticPETSc",
                           "cube_1e3_np3.h5",
                           package = "r2ogs6")
    ogs6_h5 <- OGS6_h5$new(h5_path)
    expect_silent(df <- ogs6_h5$get_df("/t_0", "pressure"))
    expect_equal(dim(df), c(1895, 5))
    expect_equal(colnames(df), c("x", "y", "z", "time", "pressure"))
})

test_that("benchmark h5 file square_5x5_tris_32.h5 passes", {
    h5_path <- system.file("/extdata/benchmarks/EllipticPETSc",
                           "square_5x5_tris_32.h5",
                           package = "r2ogs6")
    ogs6_h5 <- OGS6_h5$new(h5_path)
    expect_silent(df <- ogs6_h5$get_df("/meshes/square_5x5_tris_32",
                                       c("pressure", "MaterialIDs")))
    expect_equal(dim(df), c(200, 6))
    expect_equal(colnames(df),
                 c("x", "y", "z", "time", "pressure", "MaterialIDs"))
})


test_that("helpful error msg for wrong h5 names", {

    h5_path <- system.file("/extdata/benchmarks/EllipticPETSc",
                          "cube_1e3.h5",
                          package = "r2ogs6")

    ogs6_h5 <- OGS6_h5$new(h5_path)
    expect_error(ogs6_h5$get_df("/t_0", "/tpolgy"),
                 regexp = "Name /tpolgy not found in file cube_1e3.h5")
})



