

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
                   values = values,
                   tag_name = "curve",
                   is_subclass = FALSE,
                   attr_names = character(),
                   flatten_on_exp = c("coords", "values")
                   ),
              class = "r2ogs6_curve"
    )
}