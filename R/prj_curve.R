
#===== prj_curve =====


#' prj_curve
#' @description tag: curve, a curve
#' @param name string: Name of the curve
#' @param coords string | numeric: Coordinates at which the curve's values
#'   are given
#' @param values string | numeric: Values of the curve at the given coordinates
#' @example man/examples/ex_prj_curve.R
#' @export
prj_curve <- function(name, coords, values){

    if(missing(name)){ name <- NULL }
    coords <- coerce_string_to_numeric(coords)
    values <- coerce_string_to_numeric(values)

    new_prj_curve(name, coords, values)
}


new_prj_curve <- function(name, coords, values){

    if(!is.null(name)){
        assertthat::assert_that(assertthat::is.string(name))
    }
    assertthat::assert_that(is.numeric(coords))
    assertthat::assert_that(is.numeric(values))

    structure(list(name = name,
                   coords = coords,
                   values = values,
                   xpath = c("curves/curve",
                             paste0("processes/process/porous_medium/",
                                    "porous_medium/capillary_pressure/curve"),
                             paste0("processes/process/material_property/",
                             "porous_medium/porous_medium/",
                             "capillary_pressure/curve")),
                   attr_names = character(),
                   flatten_on_exp = c("coords", "values")
                   ),
              class = "prj_curve"
    )
}
