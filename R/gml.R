#============================== GML CLASSES AND METHODS ================================

#============================== GML ================================


#'r2ogs6_gml
#'@description S3 class describing the .gml file
#'@param name The name of the geometry
#'@param points A tibble of points
#'@param polylines Optional: A list of polylines
#'@param surfaces Optional: A list of surfaces
#'@export
r2ogs6_gml <- function(name, points, polylines = NULL, surfaces = NULL){

    #Make this more user friendly
    #...

    validate_r2ogs6_gml(new_r2ogs6_gml(name, points, polylines, surfaces))
}



#'new_r2ogs6_gml
#'@description Constructor for S3 class new_r2ogs6_gml
#'@param name The name of the geometry
#'@param points A tibble of points
#'@param polylines Optional: A list of polylines
#'@param surfaces Optional: A list of surfaces
new_r2ogs6_gml <- function(name, points, polylines = NULL, surfaces = NULL) {

    assertthat::assert_that(assertthat::is.string(name))

    validate_points(points)

    if(!is.null(polylines)){
        validate_polylines(polylines)
    }

    if(!is.null(surfaces)){
        validate_surfaces(surfaces)
    }

    structure(
        list(name = name,
             points = points,
             polylines = polylines,
             surfaces = surfaces),

        class = "r2ogs6_gml")
}


#'input_add.r2ogs6_gml
#'@description Implementation of generic function input_add for S3 class r2ogs6_gml
#'@param obj A r2ogs6_gml class object
#'@param ogs6_obj A OGS6 class object
#'@export
input_add.r2ogs6_gml <- function(obj, ogs6_obj) {
    ogs6_obj$add_gml(obj)
}