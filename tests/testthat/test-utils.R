

test_that("select_fitting_subclass works", {

    subclass_names <- get_subclass_names("r2ogs6_medium")

    for(i in seq_len(length(subclass_names))){
        names(subclass_names)[[i]] <-
            get_class_tag_name(subclass_names[[i]])
    }

    subclass_name <-
        select_fitting_subclass("phase/properties/property",
                                subclass_names)

    expect_equal(subclass_name, "r2ogs6_ph_property")


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


test_that("get_subclass_names works", {

    subclass_names <- get_subclass_names("r2ogs6_chemical_system")

    expect_equal(subclass_names,
                 c(solution = "r2ogs6_solution",
                   phase_component = "r2ogs6_phase_component",
                   kinetic_reactant = "r2ogs6_kinetic_reactant",
                   rate = "r2ogs6_rate"))
})


#===== General validation =====


test_that("validate_is_dir_path works", {

    path <- "test/path"
    path_2 <- "test\\path\\"


    path <- validate_is_dir_path(path)
    path_2 <- validate_is_dir_path(path_2)

    expect_equal(path, "test/path/")
    expect_equal(path_2, "test/path/")
})


test_that("clean_up_imported_list works", {

    test_list <- list(list(1, 2), list("a", "b"), "\n    ")

    test_list <- clean_up_imported_list(test_list)

    expect_equal(length(test_list), 2)
    expect_equal(test_list[[1]], list(1, 2))
    expect_equal(test_list[[2]], list("a", "b"))
})
