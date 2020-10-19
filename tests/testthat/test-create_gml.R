test_that("GMLPoint object is initialized correctly", {
  gml_point <- GMLPoint$new(1, c(2, 3, 4), TRUE)

  expect_equal(gml_point$id, 1)
  expect_equal(gml_point$coordinates, c(2, 3, 4))
  expect_equal(gml_point$dim, 3)
  expect_equal(gml_point$is_origin, TRUE)

  expect_equal(gml_point$as_list(), list(id = 1, x = 2, y = 3, z = 4, name = "origin"))
  expect_equal(gml_point$as_node(), list(point = structure(list(), id = 1,
                                                           x = 2, y = 3, z = 4, name = "origin")))
})

test_that("GMLPoints object is initialized correctly", {
  gml_point_1 <- GMLPoint$new(is_origin = TRUE)
  gml_point_2 <- GMLPoint$new(1, c(0, 0, 1))
  gml_points <- GMLPoints$new(c(gml_point_1, gml_point_2))

  expect_equal(gml_points$ids, c(gml_point_1$id, gml_point_2$id))
  expect_equal(gml_points$as_list(), list(point = gml_point_1$as_list(), point = gml_point_2$as_list()))
  expect_equal(gml_points$as_node(), list(points = c(gml_point_1$as_node(), gml_point_2$as_node())))
})


test_that("GMLPolyline object is initialized correctly", {
  gml_polyline <- GMLPolyline$new(0, "my_line", 1, 2)

  expect_equal(gml_polyline$id, 0)
  expect_equal(gml_polyline$name, "my_line")
  expect_equal(gml_polyline$point1, 1)
  expect_equal(gml_polyline$point2, 2)

  expect_equal(gml_polyline$as_node(), list(polyline = structure(list(pnt = list(1), pnt = list(2)),
                                                                 id = 0, name = "my_line")))
})

test_that("GMLPolylines object is initialized correctly", {
  gml_polyline_1 <- GMLPolyline$new(0, "line_1", 1, 2)
  gml_polyline_2 <- GMLPolyline$new(1, "line_2", 1, 3)
  gml_polyline_3 <- GMLPolyline$new(1, "line_3", 1, 3)
  gml_polylines <- GMLPolylines$new(c(gml_polyline_1, gml_polyline_2))

  expect_equal(gml_polylines$ids, c(gml_polyline_1$id, gml_polyline_2$id))
  expect_equal(gml_polylines$gml_polylines, c(gml_polyline_1, gml_polyline_2))

  expect_error(gml_polylines$validate_ids(c(gml_polyline_2, gml_polyline_3)))
  expect_error(gml_polylines$validate_polylines(c(gml_polyline_2, gml_polyline_3)))
  expect_equal(gml_polylines$as_node(), list(polylines = c(gml_polyline_1$as_node(), gml_polyline_2$as_node())))
})


test_that("GMLSurfaceElement object is initialized correctly", {
  gml_surface_element <- GMLSurfaceElement$new(0, 1, 3)

  expect_equal(gml_surface_element$p1, 0)
  expect_equal(gml_surface_element$p2, 1)
  expect_equal(gml_surface_element$p3, 3)

  expect_error(GMLSurfaceElement$new(0, 0, 3))
  expect_equal(gml_surface_element$as_list(), list(p1 = 0, p2 = 1, p3 = 3))
  expect_equal(gml_surface_element$as_node(), list(element = structure(list(), p1 = 0, p2 = 1, p3 = 3)))
})

test_that("GMLSurface object is initialized correctly", {
  gml_surface_element_1 <- GMLSurfaceElement$new(0, 1, 3)
  gml_surface_element_2 <- GMLSurfaceElement$new(1, 3, 2)
  gml_surface_element_3 <- GMLSurfaceElement$new(3, 0, 1)
  gml_surface <- GMLSurface$new(0, "my_surface", gml_surface_element_1, gml_surface_element_2)

  expect_equal(gml_surface$id, 0)
  expect_equal(gml_surface$name, "my_surface")
  expect_equal(gml_surface$element1, gml_surface_element_1)
  expect_equal(gml_surface$element2, gml_surface_element_2)

  expect_error(GMLSurface$new(0, "my_surface", gml_surface_element_1, gml_surface_element_3))
  expect_equal(gml_surface$as_node(), list(surface =
                                                     structure(c(gml_surface_element_1$as_node(),
                                                                 gml_surface_element_2$as_node()),
                                                               id = 0, name = "my_surface")))
})

test_that("GMLSurfaces object is initialized correctly", {
  gml_surface_element_1 <- GMLSurfaceElement$new(0, 1, 3)
  gml_surface_element_2 <- GMLSurfaceElement$new(1, 3, 2)
  gml_surface_element_3 <- GMLSurfaceElement$new(4, 5, 6)
  gml_surface_element_4 <- GMLSurfaceElement$new(5, 6, 7)
  gml_surface_1 <- GMLSurface$new(0, "surface_1", gml_surface_element_1, gml_surface_element_2)
  gml_surface_2 <- GMLSurface$new(1, "surface_2", gml_surface_element_3, gml_surface_element_4)
  gml_surface_3 <- GMLSurface$new(1, "surface_3", gml_surface_element_3, gml_surface_element_4)
  gml_surfaces <- GMLSurfaces$new(c(gml_surface_1, gml_surface_2))

  expect_equal(gml_surfaces$ids, c(gml_surface_1$id, gml_surface_2$id))
  expect_error(gml_surfaces$validate_ids(c(gml_surface_2, gml_surface_3)))
  expect_equal(gml_surfaces$as_node(), list(surfaces = c(gml_surface_1$as_node(), gml_surface_2$as_node())))
})
