
#===== Implementation utility =====


test_that("get_class_from_xpath() works", {

    expect_equal(get_class_from_xpath("processes/process"),
                 "prj_process")

    xpath = "time_loop/processes/process/convergence_criterion"
    expect_equal(get_class_from_xpath(xpath),
                 "prj_convergence_criterion")
})


test_that("get_tag_from_class() works", {
    expect_equal(get_tag_from_class(class_name = "prj_tl_process"),
                 "process")
})


test_that("get_tag_from_xpath() works", {
    expect_equal(get_tag_from_xpath("this/is/a/test"), "test")
})


test_that("ogs6_prj_top_level_classes() works", {
    expect_equal(ogs6_prj_top_level_classes()[["processes"]], "prj_process")
})


#===== Coercion utility =====


test_that("coerce_string_to_numeric() works", {

    string <- "1          \n          0.5 4"
    expect_equal(coerce_string_to_numeric(string), c(1, 0.5, 4))
    expect_equal(coerce_string_to_numeric(list("1")), list("1"))
})


test_that("coerce_names() works", {

    test_list <- list(a = "1", b = "2")
    coerced_list <- coerce_names(test_list, c("b", "a"))
    expect_equal(identical(test_list, coerced_list), TRUE)
})


test_that("clean_imported_list works", {

    test_list <- list(list(1, 2), list("a", "b"), "\n    ")

    test_list <- clean_imported_list(test_list)

    expect_equal(length(test_list), 2)
    expect_equal(test_list[[1]], list(1, 2))
    expect_equal(test_list[[2]], list("a", "b"))
})


test_that("as_dir_path works", {

    path <- "test/path"
    path_2 <- "test\\path\\"


    path <- as_dir_path(path)
    path_2 <- as_dir_path(path_2)

    expect_equal(path, "test/path/")
    expect_equal(path_2, "test/path/")
})


#===== Validate parameters =====


#...

#--- File utility ------------------------------------------------------------

test_that("as_dir_path2() works", {

    path1 <- "./test/path"
    path2 <- "../test/path"
    path3 <- "test/path/"

    expect_equal(as_dir_path2(path1), "test/path")
    expect_equal(as_dir_path2(path2), "../test/path")
    expect_equal(as_dir_path2(path3), "test/path/")
})

test_that("make_abs_path() works", {

    ref_path <- system.file("extdata/benchmarks/Elliptic/circle_radius_1",
                             "circle_1e1_axi.prj", package = "r2ogs6") %>%
                dirname()

    path1 <- system.file("extdata/benchmarks/AxiSymTheis", "axisym_theis.gml",
                         package = "r2ogs6")
    path2 <- "circle_1_axi.gml"
    path3 <- "../other_dir/SteadyStateDiffusion.xml"
    path4 <- "test/path/"

    path2_ref <- system.file("extdata/benchmarks/Elliptic/circle_radius_1",
                             "circle_1_axi.gml", package = "r2ogs6")
    path3_ref <- system.file("extdata/benchmarks/Elliptic/other_dir/",
                             "SteadyStateDiffusion.xml", package = "r2ogs6")

    expect_equal(make_abs_path(path1, ref_path), path1)
    expect_equal(make_abs_path(path2, ref_path), path2_ref)
    expect_equal(make_abs_path(path3, ref_path), path3_ref)
    expect_error(make_abs_path(path4, ref_path))

})
