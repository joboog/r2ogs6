
#===== OGS6_gml =====


#' OGS6_gml
#' @description Constructor for the OGS6_gml base class
#' @examples
#' OGS6_gml$new(
#'     name = "cube_1x1x1_geometry",
#'     points = tibble::tibble(
#'         x = c(0, 0, 0, 0),
#'         y = c(0, 0, 1, 1),
#'         z = c(0, 1, 1, 0),
#'         name = c("origin", "", "", "")
#'     ),
#'     polylines = list(polyline = list("front_left",
#'                                      c(
#'                                          pnt = 0, pnt = 1
#'                                      ))),
#'     surfaces = list(surface = list(
#'         name = "left",
#'         element = c(p1 = 0, p2 = 1, p3 = 2),
#'         element = c(p1 = 0, p2 = 3, p3 = 2)
#'     ))
#' )
#' @export
OGS6_gml <- R6::R6Class(
    "OGS6_gml",
    public = list(

        #' @description
        #'   Creates new OGS6_gml object
        #' @param gml_path string: Optional: Path to .gml file
        #' @param name string: Geometry name
        #' @param points tibble: Must have 3 vectors named 'x', 'y' and 'z', may
        #'   have optional 'name' vector
        #' @param polylines list(list("foo", c(1, 2))):
        #' @param surfaces list(list("foo", c(1, 2, 3), c(2, 3, 4)))
        initialize = function(gml_path = NULL,
                              name = NULL,
                              points = NULL,
                              polylines = NULL,
                              surfaces = NULL) {

            if(is.null(gml_path)){
                self$name <- name
                self$points <- points
                self$polylines <- polylines
                self$surfaces <- surfaces
            }else{
                if(!is.null(name) ||
                   !is.null(points) ||
                   !is.null(polylines) ||
                   !is.null(surfaces)){
                    warning(paste("`gml_path` was specified for OGS6_gml",
                                  "initialization, so all other parameters",
                                  "will be ignored!"), call. = FALSE)
                }

                xml_doc <- validate_read_in_xml(gml_path)

                self$name <- xml2::xml_text(xml2::xml_find_first(xml_doc,
                                                                 "//name"))
                self$points <- read_in_points(xml_doc)
                self$polylines <- read_in_polylines(xml_doc)
                self$surfaces <- read_in_surfaces(xml_doc)
            }

            private$.gml_path <- gml_path
            private$validate()
        },

        #'@description
        #'Overrides default printing behaviour
        print = function(){
            cat("OGS6_gml\n")
            cat("path:  ", self$gml_path, "\n", sep = "")
            cat("name:  ", self$name, "\n", sep = "")

            cat("\npoints\n")
            print(self$points)

            cat("\npolylines\n")
            print(self$polylines)

            cat("\nsurfaces\n")
            print(self$surfaces)

            return(invisible(self))
        }
    ),

    active = list(

        #'@field gml_path
        #'Getter for private parameter '.gml_path'
        gml_path = function(value) {
            private$gml_path
        },

        #'@field name
        #'Access to private parameter '.name'
        name = function(value) {
            if(missing(value)) {
                private$.name
            }else{
                assertthat::assert_that(assertthat::is.string(value))
                private$.name <- value
            }
        },

        #'@field points
        #'Access to private parameter '.points'
        points = function(value) {
            if(missing(value)) {
                private$.points
            }else{
                private$.points <- validate_points(value)
            }
        },

        #'@field polylines
        #'Access to private parameter '.polylines'
        polylines = function(value) {
            if(missing(value)) {
                private$.polylines
            }else{
                if(!is.null(value)){
                    value <- validate_polylines(value)
                }

                private$.polylines <- value
            }
        },

        #'@field surfaces
        #'Access to private parameter '.surfaces'
        surfaces = function(value) {
            if(missing(value)) {
                private$.surfaces
            }else{
                if(!is.null(value)){
                    value <- validate_surfaces(value)
                }
                private$.surfaces <- value
            }
        },

        #'@field attr_names
        #'Getter for private parameter '.attr_names'
        attr_names = function(value) {
            private$.attr_names
        },

        #'@field flatten_on_exp
        #'Getter for private parameter '.flatten_on_exp'
        flatten_on_exp = function(value) {
            private$.flatten_on_exp
        }
    ),

    private = list(

        validate = function(){
            maximal_point_id <- length(self$points[[1]]) - 1

            check_pnt <- function(pnt){
                if(pnt > maximal_point_id ||
                   pnt < 0){
                    stop(paste("Point with ID", pnt, "does not exist"),
                         call. = FALSE)
                }
            }

            #Check if polylines reference existing points
            lapply(self$polylines, function(x){
                lapply(x[[2]], check_pnt)
            })

            #Check if surfaces reference existing points
            lapply(self$surfaces, function(x){
                lapply(x[[2]], check_pnt)
                if(length(x) == 3){
                    lapply(x[[3]], check_pnt)
                }
            })
        },

        .gml_path = NULL,
        .name = NULL,
        .points = NULL,
        .polylines = NULL,
        .surfaces = NULL,
        .attr_names = c("point", "name", "id", "element"),
        .flatten_on_exp = character()
    )
)


#===== Validation utility =====


#' validate_points
#' @description
#' Checks if the input is a tibble, if this tibble has the right number of
#' elements, if those elements are named correctly and if there are
#' any overlapping points or duplicate point names
#' @param points tibble: Must have 3 vectors named 'x', 'y' and 'z', may have
#'   optional 'name' vector
#' @noRd
validate_points <- function(points) {

    assertthat::assert_that(inherits(points, "tbl_df"))

    names <- names(points)

    if (!((length(points) == 4 && names[[1]] == "x" && names[[2]] == "y" &&
           names[[3]] == "z" && names[[4]] == "name") ||
          (length(points) == 3 && names[[1]] == "x" && names[[2]] == "y" &&
           names[[3]] == "z"))){
        stop(paste(points, " column names do not fit to 'x, y, z, (name)' "),
             call. = FALSE)
    }

    assertthat::assert_that(is.numeric(points$x))
    assertthat::assert_that(is.numeric(points$y))
    assertthat::assert_that(is.numeric(points$z))

    has_names <- (length(points) == 4)

    # #Find overlapping points and duplicate names
    # for(i in 1:(length(points[[1]])-1)){
    #     for(j in (i+1):length(points[[1]])){
    #         if(points[[1]][[i]] == points[[1]][[j]] &&
    #            points[[2]][[i]] == points[[2]][[j]] &&
    #            points[[3]][[i]] == points[[3]][[j]]){
    #             stop("Overlapping .gml points detected", call. = FALSE)
    #         }
    #
    #         if(has_names){
    #             if(points[[4]][[i]] == points[[4]][[j]] &&
    #                points[[4]][[i]] != ""){
    #                 warning("Duplicate .gml point names detected",
    #                         call. = FALSE)
    #             }
    #         }
    #     }
    # }

    return(invisible(points))
}


#' validate_polylines
#' @description
#' Checks if the input is a list, if this list consists of other
#' lists and if those lists have the correct structure (length of 2, first
#' element is a string named 'name', second element is a numeric vector)
#' @param polylines list(list("foo", c(1, 2))):
#' @noRd
validate_polylines <- function(polylines) {

    assertthat::assert_that(is.list(polylines))

    polylines <- lapply(polylines, function(x){
        assertthat::assert_that(is.list(x))
        assertthat::assert_that(length(x) == 2)
        assertthat::assert_that(assertthat::is.string(x[[1]]))
        assertthat::assert_that(is.numeric(x[[2]]))
        names(x)[[1]] <- c("name")
        names(x[[2]]) <- rep("pnt", length(names(x[[2]])))
        return(x)
    })

    names(polylines) <- rep("polyline", length(polylines))
    return(invisible(polylines))
}


#' validate_surfaces
#' @description
#' Checks if the input is a list, if this list consists of other
#' lists and if those lists have the correct structure (length of 2 or 3, first
#' element is a string named 'name', second and third element are numeric
#' vectors)
#' @param surfaces list(list("foo", c(1, 2, 3), c(2, 3, 4))):
#' @noRd
validate_surfaces <- function(surfaces) {

    assertthat::assert_that(is.list(surfaces))

    validate_element <- function(element){
        assertthat::assert_that(is.numeric(element))
        assertthat::assert_that(length(element) == 3)
        names(element) <- c("p1", "p2", "p3")
        return(invisible(element))
    }

    surfaces <- lapply(surfaces, function(x){

        assertthat::assert_that(is.list(x))
        assertthat::assert_that(length(x) == 2 ||
                                    length(x) == 3)

        names(x) <- c("name", rep("element", (length(x)-1)))
        x[[2]] <- validate_element(x[[2]])

        if(length(x) == 3){
            x[[3]] <- validate_element(x[[3]])
            # validate_pnt_values(x[[2]], x[[3]])
        }

        return(x)
    })

    names(surfaces) <- rep("surface", length(surfaces))

    return(invisible(surfaces))
}


#' validate_pnt_values
#' @description
#' Checks if two numerical vectors of length 3
#' (two surface elements) each consist of 3 different elements and have
#' exactly 2 matching elements between them. Think of the two vectors as
#' triangles, and the triangles together form a square which is our surface.
#' @param element_1 numeric, length = 3
#' @param element_2 numeric, length = 3
#' @noRd
validate_pnt_values = function (element_1, element_2) {

    if(element_1[[1]] == element_1[[2]] ||
       element_1[[1]] == element_1[[3]] ||
       element_1[[2]] == element_1[[3]] ||
       element_2[[1]] == element_2[[2]] ||
       element_2[[1]] == element_2[[3]] ||
       element_2[[2]] == element_2[[3]]) {
        stop("A surface element must consist of 3 different points",
             call. = FALSE)
    }

    equal_count <- 0

    for(i in 1:length(element_1)) {
        for(j in 1:length(element_2)) {
            if(element_1[[i]] == element_2[[j]]) {
                equal_count <- equal_count + 1
                break
            }
        }
    }

    if(equal_count != 2) {
        stop("Invalid surface detected", call. = FALSE)
    }
}
