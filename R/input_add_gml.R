# Functions for adding .gml data
# input: ogs6 class object
# output: updated ogs6 class object

#'Class describing the gml file and any parameters defined in one
new_gml <- function(geometry_name = NULL,
                    points = NULL,
                    polylines = NULL,
                    surfaces = NULL) {

    structure(
        list(geometry_name = geometry_name,
             points = points,
             polylines = polylines,
             surfaces = surfaces),

        class = "gml")
}


#'Adds an empty gml class object to a ogs6 class object input list
#'@param ogs6_obj The ogs6 object the gml class object should be added to
#'@param geometry_name The name of the geometry specified by the gml class object
input_add_gml_obj <- function(ogs6_obj, geometry_name) {

    if(!is.character(geometry_name)){
        stop("'geometry_name' is not of type character()", call. = FALSE)
    }

    if("gml_obj" %in% names(ogs6_obj$sim_input)){
        stop("ogs6_obj already has a gml object attached to it.", call. = FALSE)
    }else{
        ogs6_obj$add_sim_io_input("gml_obj", new_gml(geometry_name = geometry_name))
    }
}


#'Adds gml points to a ogs6 class object
#'@param ogs6_obj A ogs6 class object
#'@param gml_points A tibble of gml points
input_add_gml_points <- function(ogs6_obj, gml_points) {

    validate_points(gml_points)

    check_for_obj_of_name(ogs6_obj, "gml_obj")

    if(!is.null(ogs6_obj$sim_input[["gml_obj"]]$gml_points)){
        stop("There are already points defined for the gml object", call. = FALSE)
    }else{
        ogs6_obj$set_sim_input_obj_param("gml_obj", "gml_points", gml_points)
    }
}


#'Adds gml polylines to a ogs6 class object
#'@param ogs6_obj A ogs6 class object
#'@param gml_polylines A list of gml polylines
input_add_gml_polylines <- function(ogs6_obj, gml_polylines) {

    validate_polylines(gml_polylines)

    check_for_obj_of_name(ogs6_obj, "gml_obj")

    if(!is.null(ogs6_obj$sim_input[["gml_obj"]]$gml_polylines)){
        stop("There are already polylines defined for the gml object", call. = FALSE)
    }else{
        ogs6_obj$set_sim_input_obj_param("gml_obj", "gml_polylines", gml_polylines)
    }
}


#'Adds gml surfaces to a ogs6 class object
#'@param ogs6_obj A ogs6 class object
#'@param gml_surfaces A list of gml surfaces
input_add_gml_surfaces <- function(ogs6_obj, gml_surfaces) {

    validate_surfaces(gml_surfaces)

    check_for_obj_of_name(ogs6_obj, "gml_obj")

    if(!is.null(ogs6_obj$sim_input[["gml_obj"]]$gml_surfaces)){
        stop("There are already surfaces defined for the gml object", call. = FALSE)
    }else{
        ogs6_obj$set_sim_io_input_obj_param("gml_obj", "gml_surfaces", gml_surfaces)
    }
}