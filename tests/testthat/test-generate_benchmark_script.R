

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

  ogs_mesh <- r2ogs6_mesh("my_mesh")
  ogs_mesh_call <- construct_add_call(ogs_mesh)
  expect_equal(ogs_mesh_call,
               "ogs6_obj$add_mesh(r2ogs6_mesh(mesh_ref = \"my_mesh\"))\n")

  ogs_param <- r2ogs6_parameter("a",
                                "t",
                                NULL,
                                c(0, 1))

  ogs_param_call <- construct_add_call(ogs_param)

  expect_equal(ogs_param_call, paste0("ogs6_obj$add_parameter(",
                                      "r2ogs6_parameter(name = \"a\",\n",
                                      "type = \"t\",\n",
                                      "values = c(0, 1)))\n"))
})


test_that("delete_nulls_from_str works", {

  #Single line strings
  test_str <- "r2ogs6_object(one = NULL, two = 3, three = NULL, f_o_u_r = NULL)"

  test_str <- delete_nulls_from_str(test_str)
  expect_equal(test_str, "r2ogs6_object(two = 3)")

  #Multiline strings
  test_str_2 <-
    "r2ogs6_object(one = NULL,\ntwo = 3,\nthree = NULL,\nf_o_u_r = NULL)"

  test_str_2 <- delete_nulls_from_str(test_str_2)
  expect_equal(test_str_2, "r2ogs6_object(two = 3)")

})