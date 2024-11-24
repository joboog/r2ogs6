
#'prj_raster
#'@description tag: raster
#'@param file string:
#'@param variable string:
#'@param dimension Optional:
#'@export
prj_raster <- function(
  file,
  variable,
  dimension = NULL
  ) {

  # Add coercing utility here
  dimension <- coerce_string_to_numeric(dimension)

  new_prj_raster(file,
    variable,
    dimension
  )

}


new_prj_raster <- function(
  file,
  variable,
  dimension = NULL
  ) {
  
  assertthat::assert_that(assertthat::is.string(file))
  assertthat::assert_that(assertthat::is.string(variable))
  are_null_or_numeric(dimension)

  structure(
    list(
      file = file,
      variable = variable,
      dimension = dimension,
      xpath = "rasters/raster",
      attr_names = character(),
      flatten_on_exp = character()
    ),
  class = "prj_raster"
  )
}