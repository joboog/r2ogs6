
#===== r2ogs6_vtkdiff =====


#'r2ogs6_vtkdiff
#'@description tag: vtkdiff
#'@param regex string: A regular expression
#'@param field string: ...
#'@param absolute_tolerance string | double: Absolute tolerance
#'@param relative_tolerance string | double: Relative tolerance
#'@export
r2ogs6_vtkdiff <- function(regex,
                           field,
                           absolute_tolerance,
                           relative_tolerance) {

    #Coerce input
    absolute_tolerance <- coerce_string_to_numeric(absolute_tolerance)
    relative_tolerance <- coerce_string_to_numeric(relative_tolerance)

    new_r2ogs6_vtkdiff(regex,
                       field,
                       absolute_tolerance,
                       relative_tolerance)
}


new_r2ogs6_vtkdiff <- function(regex,
                               field,
                               absolute_tolerance,
                               relative_tolerance) {

    assertthat::assert_that(assertthat::is.string(regex))
    assertthat::assert_that(assertthat::is.string(field))
    assertthat::assert_that(assertthat::is.number(absolute_tolerance))
    assertthat::assert_that(assertthat::is.number(relative_tolerance))

    structure(
        list(regex = regex,
             field = field,
             absolute_tolerance = absolute_tolerance,
             relative_tolerance = relative_tolerance,
             is_subclass = FALSE,
             attr_names = character(),
             flatten_on_exp = character()
        ),
        class = "r2ogs6_vtkdiff"
    )
}
