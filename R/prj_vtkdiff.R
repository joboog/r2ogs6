
#===== r2ogs6_vtkdiff =====


#'r2ogs6_vtkdiff
#'@description tag: vtkdiff
#'
#'@param field string: ...
#'@param absolute_tolerance string | double: Absolute tolerance
#'@param relative_tolerance string | double: Relative tolerance
#'@param file string: Optional: File
#'@param regex string: Optional: A regular expression
#'@export
r2ogs6_vtkdiff <- function(field,
                           absolute_tolerance,
                           relative_tolerance,
                           file = NULL,
                           regex = NULL) {

    #Coerce input
    absolute_tolerance <- coerce_string_to_numeric(absolute_tolerance)
    relative_tolerance <- coerce_string_to_numeric(relative_tolerance)

    new_r2ogs6_vtkdiff(field,
                       absolute_tolerance,
                       relative_tolerance,
                       file,
                       regex)
}


new_r2ogs6_vtkdiff <- function(field,
                               absolute_tolerance,
                               relative_tolerance,
                               file = NULL,
                               regex = NULL) {

    assertthat::assert_that(assertthat::is.string(field))

    validate_is_number(absolute_tolerance,
                       relative_tolerance)

    validate_is_null_or_string(file,
                               regex)

    structure(
        list(field = field,
             absolute_tolerance = absolute_tolerance,
             relative_tolerance = relative_tolerance,
             file = file,
             regex = regex,
             is_subclass = FALSE,
             attr_names = character(),
             flatten_on_exp = character()
        ),
        class = "r2ogs6_vtkdiff"
    )
}
