#============================== TEST_DEFINITION CLASSES AND METHODS ================================

#============================== VTKDIFF ================================

#'r2ogs6_vtkdiff
#'@description S3 class describing a .prj test_definition vtkdiff
#'@param regex ...
#'@param field ...
#'@param absolute_tolerance ...
#'@param relative_tolerance ...
#'@export
r2ogs6_vtkdiff <- function(regex, field, absolute_tolerance, relative_tolerance) {

    #Coerce input
    if(assertthat::is.string(absolute_tolerance)){
        absolute_tolerance <- as.double(absolute_tolerance)
    }

    if(assertthat::is.string(relative_tolerance)){
        relative_tolerance <- as.double(relative_tolerance)
    }

    new_r2ogs6_vtkdiff(regex, field, absolute_tolerance, relative_tolerance)
}


new_r2ogs6_vtkdiff <- function(regex, field, absolute_tolerance, relative_tolerance) {

    assertthat::assert_that(assertthat::is.string(regex))
    assertthat::assert_that(assertthat::is.string(field))
    assertthat::assert_that(assertthat::is.number(absolute_tolerance))
    assertthat::assert_that(assertthat::is.number(relative_tolerance))

    structure(
        list(regex = regex,
             field = field,
             absolute_tolerance = absolute_tolerance,
             relative_tolerance = relative_tolerance
        ),
        class = "r2ogs6_vtkdiff"
    )
}


#'as_node.r2ogs6_vtkdiff
#'@description Implementation of generic function as_node for S3 class r2ogs6_vtkdiff
#'@param x A r2ogs6_vtkdiff class object
as_node.r2ogs6_vtkdiff <- function(x) {

    node <- list(vtkdiff = structure(list()))

    node <- add_children(node, list(regex = x$regex,
                                    field = x$field,
                                    absolute_tolerance = x$absolute_tolerance,
                                    relative_tolerance = x$relative_tolerance))

    return(node)
}


#'input_add.r2ogs6_vtkdiff
#'@description Implementation of generic function input_add for S3 class r2ogs6_vtkdiff
#'@param x A r2ogs6_vtkdiff class object
#'@param ogs6_obj A OGS6 class object
#'@export
input_add.r2ogs6_vtkdiff <- function(x, ogs6_obj) {
    ogs6_obj$add_vtkdiff(x)
}