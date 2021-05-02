

test_that("construct_add_call works", {

  simple_num <- c(2, 3, 4)
  num_call <- construct_add_call(simple_num)
  expect_equal(num_call, "c(2, 3, 4)")

  simple_char <- c("a", "b", "c")
  char_call <- construct_add_call(simple_char)
  expect_equal(char_call, "c(\"a\", \"b\", \"c\")")

  my_tibble <- tibble::tibble(x = c(1, 2),
                              y = c(3, 4))
  tibble_call <- construct_add_call(my_tibble)
  expect_equal(tibble_call, "tibble::tibble(x = c(1, 2),\ny = c(3, 4))")

  my_list <- list(a = simple_num, b = simple_char)
  list_call <- construct_add_call(my_list)
  expect_equal(list_call, "list(a = c(2, 3, 4),\nb = c(\"a\", \"b\", \"c\"))")

  ogs6_param <- prj_parameter("a",
                                "t",
                                NULL,
                                c(0, 1))

  ogs6_param_call <- construct_add_call(ogs6_param)

  expect_equal(ogs6_param_call, paste0("ogs6_obj$add(",
                                      "prj_parameter(name = \"a\",\n",
                                      "type = \"t\",\n",
                                      "values = c(0, 1)))\n"))

  ogs6_meshes <- list("mesh_1",
                     "mesh_2")

  ogs6_mesh_call <- construct_add_call(ogs6_meshes)

  expect_equal(ogs6_mesh_call, paste0("list(\"mesh_1\",\n\"mesh_2\")"))
})


test_that("construct_add_call handles OGS6_gml correctly", {

  ogs6_gml <- OGS6_gml$new(name = "test",
                          points = tibble::tibble(x = c(0),
                                                  y = c(0),
                                                  z = c(0)))

  ogs6_gml_call <- construct_add_call(ogs6_gml)

  expect_equal(ogs6_gml_call,
               paste0("ogs6_obj$add(OGS6_gml$new(name = \"test\",\n",
               "points = tibble::tibble(x = 0,\ny = 0,\nz = 0)))\n"))
})


test_that("construct_add_call handles Ellipsis correctly", {

  ogs6_parameter <- prj_parameter(name = "test",
                                    type = "test",
                                    index_values = list("1", "1 2"),
                                    index_values = list("2", "2 3"))

  ogs6_param_call <- construct_add_call(ogs6_parameter)

  expect_equal(ogs6_param_call,
               paste0("ogs6_obj$add(prj_parameter(name = ",
                      "\"test\",\ntype = \"test\",\nindex_values = ",
                      "list(index = 1,\nvalues = c(1, 2)),\n",
                      "index_values = list(index = 2,\nvalues = c(2, 3))))\n"))
})
