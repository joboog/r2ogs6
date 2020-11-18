#============================== CURVES CLASSES AND METHODS ================================

#============================== CURVE ================================


#'r2ogs6_curve
#'@description S3 class describing a .prj curve
#'@param name The name of the curve
#'@param coords Coordinates at which the curve's values are given
#'@param values Values of the curve at the given coordinates
#'@export
r2ogs6_curve <- function(name, coords, values){

    #Coerce input
    if(assertthat::is.string(coords)){
        coords <- as.double(unlist(strsplit(coords, " ")))
    }

    if(assertthat::is.string(values)){
        values <- as.double(unlist(strsplit(values, " ")))
    }

    new_r2ogs6_curve(name, coords, values)
}


#'new_r2ogs6_curve
#'@description Constructor for S3 class r2ogs6_curve
#'@param name The name of the curve
#'@param coords Coordinates at which the curve's values are given
#'@param values Values of the curve at the given coordinates
new_r2ogs6_curve <- function(name, coords, values){

    assertthat::assert_that(assertthat::is.string(name))
    assertthat::assert_that(is.numeric(coords))
    assertthat::assert_that(is.numeric(values))

    structure(list(name = name,
                   coords = coords,
                   values = values),
              class = "r2ogs6_curve"
    )
}


#'as_node.r2ogs6_curve
#'@description Implementation of generic function as_node for S3 class r2ogs6_curve
#'@param x A r2ogs6_curve class object
as_node.r2ogs6_curve <- function(x) {

    node <- list(curve = structure(list()))

    coords_str <- paste(x$coords, collapse = " ")
    values_str <- paste(x$values, collapse = " ")

    node <- add_children(node, list(name = x$name,
                                    coords = coords_str,
                                    values = values_str))

    return(node)
}


#'input_add.r2ogs6_curve
#'@description Implementation of generic function input_add for S3 class r2ogs6_curve
#'@param x A r2ogs6_curve class object
#'@param ogs6_obj A OGS6 class object
#'@export
input_add.r2ogs6_curve <- function(x, ogs6_obj) {
    ogs6_obj$add_curve(x)
}