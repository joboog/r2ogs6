
test_that("add_attr adds an optional attribute to a node list", {

    test_node_1 <- list(test = structure(list()))

    test_node_1 <- add_attr(test_node_1, 42, "meaning_of_life")

    expect_equal(attributes(test_node_1[[1]])[["meaning_of_life"]], 42)

    test_node_1 <- add_attr(test_node_1, NULL, "null")
    expect_equal(length(attributes(test_node_1[[1]])), 1)

})


test_that("add_children adds children to a node list", {

    test_node <- list(test = structure(list(), attr_1 = "hi", attr_2 = "there"))

    expect_error(add_children(test_node, 42))
    expect_error(add_children(test_node, list(42)))

    test_node <- add_children(test_node, list(number = 42))
    test_node <- add_children(test_node, list(number = NULL))

    expect_equal(length(test_node[[1]]), 1)
})



