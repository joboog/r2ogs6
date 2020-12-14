
#===== r2ogs6_curve =====


#'r2ogs6_curve
#'@description tag: curve, a curve
#'@param name string: Name of the curve
#'@param coords string | numeric: Coordinates at which the curve's values
#' are given
#'@param values string | numeric: Values of the curve at the given coordinates
#'@export
r2ogs6_curve <- function(name, coords, values){

    #Coerce input
    coords <- coerce_string_to_numeric(coords, TRUE)
    values <- coerce_string_to_numeric(values, TRUE)

    new_r2ogs6_curve(name, coords, values)
}


new_r2ogs6_curve <- function(name, coords, values){

    assertthat::assert_that(assertthat::is.string(name))
    assertthat::assert_that(is.numeric(coords))
    assertthat::assert_that(is.numeric(values))

    structure(list(name = name,
                   coords = coords,
                   values = values,
                   is_subclass = FALSE,
                   attr_names = character(),
                   flatten_on_exp = c("coords", "values")
                   ),
              class = "r2ogs6_curve"
    )
}