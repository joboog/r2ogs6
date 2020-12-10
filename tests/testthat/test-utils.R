

test_that("select_fitting_subclass works for medium objects", {

    subclass_names <- get_subclass_names("r2ogs6_medium")

    for(i in seq_len(length(subclass_names))){
        names(subclass_names)[[i]] <-
            get_class_tag_name(subclass_names[[i]])
    }

    subclass_name <-
        select_fitting_subclass("phase/properties/property",
                                subclass_names)

    expect_equal(subclass_name, "r2ogs6_ph_property")
})


test_that("select_fitting_subclass works for linear_solver objects", {

    subclass_names <- get_subclass_names("r2ogs6_linear_solver")

    for(i in seq_len(length(subclass_names))){
        names(subclass_names)[[i]] <-
            get_class_tag_name(subclass_names[[i]])
    }

    subclass_name <-
        select_fitting_subclass("linear_solvers/linear_solver/eigen",
                                subclass_names)

    expect_equal(subclass_name, "r2ogs6_eigen")
})


#===== General validation =====


test_that("validate_is_dir_path works", {

    path <- "test/path"
    path_2 <- "test\\path\\"


    path <- validate_is_dir_path(path)
    path_2 <- validate_is_dir_path(path_2)

    expect_equal(path, "test/path/")
    expect_equal(path_2, "test\\path\\")
})
