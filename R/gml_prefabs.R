#This script contains some useful S3 classes for premade geometry

#'Constructor based on HydroMechanics/IdealGas/flow_free_expansion/cube_1x1x1.gml
new_r2ogs6_gml_cube_1x1x1 <- function() {
    structure(list(
        gml_geometry_name = "cube_1x1x1_geometry",

        #Cube points (will become second XML child under root)
        gml_points = tibble::tibble(x = c(0, 0, 0, 0, 1, 1, 1, 1),
                                    y = c(0, 0, 1, 1, 0, 0, 1, 1),
                                    z = c(0, 1, 1, 0, 0, 1, 1, 0),
                                    name = c("origin", rep("", 7))),

        #Cube polylines (will become third XML child under root)
        gml_polylines = list(list(name = "front_left", c(0, 1)),
                             list(name = "front_right", c(4, 5)),
                             list(name = "front_bottom", c(0, 4)),
                             list(name = "front_top", c(1, 5)),
                             list(name = "bottom_left", c(0, 3)),
                             list(name = "bottom_right", c(4, 7)),
                             list(name = "top_left", c(1, 2)),
                             list(name = "top_right", c(5, 6)),
                             list(name = "back_left", c(2, 3)),
                             list(name = "back_right", c(6, 7)),
                             list(name = "back_bottom", c(3, 7)),
                             list(name = "back_top", c(2, 6))),

        #Cube surfaces (will become fourth XML child under root)
        gml_surfaces = list(list(name = "left", c(0, 1, 2), c(0, 3, 2)),
                            list(name = "right", c(4, 6, 5), c(4, 6, 7)),
                            list(name = "top", c(1, 2, 5), c(5, 2, 6)),
                            list(name = "bottom", c(0, 3, 4), c(4, 3, 7)),
                            list(name = "front", c(0, 1, 4), c(4, 1, 5)),
                            list(name = "back", c(2, 3, 6), c(6, 3, 7)))

    ), class = "r2ogs6_gml_prefab")
}


#Another cube but without polylines


#'Constructor based on HydroMechanics/IdealGas/flow_no_strain/square_1x1.gml
new_r2ogs6_gml_square_1x1 <- function() {
    structure(list(
        gml_geometry_name = "square_1x1_geometry",
        gml_points = tibble::tibble(x = c(0, 0, 1, 1),
                                    y = c(0, 1, 0, 1),
                                    z = c(0, 0, 0, 0),
                                    name = c("origin", rep("", 3))),
        gml_polylines = list(list(name = "left", c(0, 1)),
                             list(name = "right", c(2, 3)),
                             list(name = "bottom", c(0, 2)),
                             list(name = "top", c(1, 3)))
    ), class = "r2ogs6_gml_prefab")
}


#'Constructor based on HydroMechanics/IdealGas/flow_pressure_boundary/quad_1x10.gml
new_r2ogs6_gml_quad_1x10 <- function() {
    structure(list(
        gml_geometry_name = "quad_1x10_geometry",
        gml_points = tibble::tibble(x = c(0, 0, 10, 10),
                                    y = c(0, 1, 0, 1),
                                    z = c(0, 0, 0, 0),
                                    name = c("origin", rep("", 3))),

        gml_polylines = list(list(name = "left", c(0, 1)),
                             list(name = "right", c(2, 3)),
                             list(name = "bottom", c(0, 2)),
                             list(name = "top", c(1, 3)))
    ), class = "r2ogs6_gml_prefab")
}


#'Constructor based on HydroMechanics/StaggeredScheme/InjectionProduction1D/bar.gml
new_r2ogs6_gml_bar <- function() {
    structure(list(
        gml_geometry_name = "bar",
        gml_points = tibble::tibble(x = c(0, 10, 10, 0),
                                    y = c(0, 0, 150, 150),
                                    z = c(0, 0, 0, 0),
                                    name = c(rep("", 4))),

        gml_polylines = list(list(name = "left", c(0, 3)),
                             list(name = "right", c(1, 2)),
                             list(name = "bottom", c(0, 1)),
                             list(name = "top", c(2, 3)))
    ), class = "r2ogs6_gml_prefab")
}


#'Constructor based on HydroMechanics/Verification/hm1_1Dbeam.gml
new_r2ogs6_gml_hm1_1Dbeam <- function() {
    structure(list(
        gml_geometry_name = "geometry",
        gml_points = tibble::tibble(x = c(1, 1, 1, 1, 0, 0, 0, 0),
                                    y = c(0.10000000000000001, 0, 0, 0.10000000000000001,
                                          0, 0, 0.10000000000000001, 0.10000000000000001),
                                    z = c(0, 0, 0.10000000000000001, 0.10000000000000001,
                                          0, 0.10000000000000001, 0, 0.10000000000000001),
                                    name = c("POINT0", "POINT1", "POINT2", "POINT3",
                                             "POINT4", "POINT7", "POINT8", "POINT22")),

        gml_polylines = list(list(name = "POLYLINE1", c(0, 1, 2, 3, 0)),
                             list(name = "POLYLINE2", c(4, 1, 2, 5, 4)),
                             list(name = "POLYLINE3", c(6, 0, 3, 7, 6)),
                             list(name = "POLYLINE4", c(4, 1, 0, 6, 4)),
                             list(name = "POLYLINE5", c(5, 2, 3, 7, 5)),
                             list(name = "POLYLINE6", c(4, 6, 7, 5, 4)),
                             list(name = "POLYLINE7", c(1, 0, 3, 2, 1))),

        gml_surfaces = list(list(name = "SURFACE1", c(0, 2, 1), c(2, 0, 3)),
                            list(name = "SURFACE2", c(4, 2, 1), c(2, 4, 5)),
                            list(name = "SURFACE3", c(6, 3, 0), c(3, 6, 7)),
                            list(name = "SURFACE4", c(4, 0, 1), c(0, 4, 6)),
                            list(name = "SURFACE5", c(5, 3, 2), c(3, 5, 7)),
                            list(name = "SURFACE6", c(4, 7, 6), c(7, 4, 5)),
                            list(name = "SURFACE7", c(1, 3, 0), c(3, 1, 2)))
    ), class = "r2ogs6_gml_prefab")
}


#'Constructor based on HydroMechanics/Verification/hm1_2Dsquare.gml
new_r2ogs6_gml_hm1_2Dsquare <- function() {
    structure(list(
        gml_geometry_name = "geometry",
        gml_points = tibble::tibble(x = c(0, 1, 1, 0, 0, 0, 1, 1),
                                    y = c(0, 0, 1, 1, 1, 0, 0, 1),
                                    z = c(0, 0, 0, 0, 0.10000000000000001, 0.10000000000000001,
                                          0.10000000000000001, 0.10000000000000001),
                                    name = c("POINT0", "POINT1", "POINT2", "POINT3",
                                             "POINT6", "POINT7", "POINT17", "POINT18")),

        gml_polylines = list(list(name = "POLYLINE1", c(0, 1, 2, 3, 0)),
                             list(name = "POLYLINE2", c(0, 3, 4, 5, 0)),
                             list(name = "POLYLINE3", c(0, 1, 6, 5, 0)),
                             list(name = "POLYLINE4", c(0, 1, 2, 3, 0)),
                             list(name = "POLYLINE5", c(5, 6, 7, 4, 5))),

        gml_surfaces = list(list(name = "SURFACE1", c(0, 2, 1), c(2, 0, 3)),
                            list(name = "SURFACE2", c(0, 4, 3), c(4, 0, 5)),
                            list(name = "SURFACE3", c(0, 6, 1), c(6, 0, 5)),
                            list(name = "SURFACE4", c(0, 2, 1), c(2, 0, 3)),
                            list(name = "SURFACE5", c(5, 7, 6), c(7, 5, 4)))
    ), class = "r2ogs6_gml_prefab")
}


#'Constructor based on HydroMechanics/Verification/hm1_3Dcube.gml
new_r2ogs6_gml_hm1_3Dcube <- function() {
    structure(list(
        gml_geometry_name = "geometry",
        gml_points = tibble::tibble(x = c(0, 1, 1, 0, 1, 0, 0, 1),
                                    y = c(0, 0, 1, 1, 0, 0, 1, 1),
                                    z = c(0, 0, 0, 0, 1, 1, 1, 1),
                                    name = c("POINT0", "POINT1", "POINT2", "POINT3",
                                             "POINT6", "POINT7", "POINT15", "POINT14")),

        gml_polylines = list(list(name = "POLYLINE1", c(0, 1, 2, 3, 0)),
                             list(name = "POLYLINE2", c(0, 1, 4, 5, 0)),
                             list(name = "POLYLINE3", c(0, 3, 6, 5, 0)),
                             list(name = "POLYLINE4", c(5, 4, 7, 6, 5))),

        gml_surfaces = list(list(name = "SURFACE1", c(0, 2, 1), c(2, 0, 3)),
                            list(name = "SURFACE2", c(0, 4, 1), c(4, 0, 5)),
                            list(name = "SURFACE3", c(0, 6, 3), c(6, 0, 5)),
                            list(name = "SURFACE4", c(5, 7, 4), c(7, 5, 6)))
    ), class = "r2ogs6_gml_prefab")
}

#'Constructor based on HydroMechanics/Verification/hm1_3Dgravity.gml
new_r2ogs6_gml_hm1_3Dgravity <- function() {
    structure(list(
        gml_geometry_name = "geometry",
        gml_points = tibble::tibble(x = c(0, 60, 60, 0, 0, 60, 60, 0),
                                    y = c(0, 0, 30, 30, 0, 0, 30, 30),
                                    z = c(30, 30, 30, 30, 0, 0, 0, 0),
                                    name = c("POINT0", "POINT1", "POINT2", "POINT3",
                                             "POINT4", "POINT5", "POINT6", "POINT7")),

        gml_polylines = list(list(name = "POLYLINE1", c(0, 1, 2, 3, 0)),
                             list(name = "POLYLINE2", c(4, 5, 6, 7, 4)),
                             list(name = "POLYLINE3", c(4, 5, 1, 0, 4)),
                             list(name = "POLYLINE4", c(7, 6, 2, 3, 7)),
                             list(name = "POLYLINE5", c(4, 7, 3, 0, 4)),
                             list(name = "POLYLINE6", c(5, 6, 2, 1, 5))),

        gml_surfaces = list(list(name = "SURFACE1", c(0, 2, 1), c(2, 0, 3)),
                            list(name = "SURFACE2", c(4, 6, 5), c(6, 4, 7)),
                            list(name = "SURFACE3", c(4, 1, 5), c(1, 4, 0)),
                            list(name = "SURFACE4", c(7, 2, 6), c(2, 7, 3)),
                            list(name = "SURFACE5", c(4, 3, 7), c(3, 4, 0)),
                            list(name = "SURFACE6", c(5, 2, 6), c(2, 5, 1)))
    ), class = "r2ogs6_gml_prefab")
}


#.../hm2_1d1bt.gml (add the hm2-gml prefabs)


#'Constructor based on ogs6_benchmarks/Elliptic/circle_radius_1/circle_1_axi.gml
#'
#'
new_r2ogs6_gml_circle_1_axi <- function() {
    structure(list(
        gml_geometry_name = "geometry",
        gml_points = tibble::tibble(x = c(0, 1),
                                    y = c(0, 0),
                                    z = c(0, 0),
                                    name = c("inner", "outer"))
    ), class = "r2ogs6_gml_prefab")
}

