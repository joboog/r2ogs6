

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
             relative_tolerance = relative_tolerance,
             tag_name = "vtkdiff",
             is_subclass = FALSE,
             attr_names = character(),
             flatten_on_exp = character()
        ),
        class = "r2ogs6_vtkdiff"
    )
}

