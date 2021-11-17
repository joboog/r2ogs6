

#' OGS6_h5
#' @description Small class to wrap \code{h5} data into the \cde{r2ogs6} workflow.
#' @export
OGS6_h5 <- R6::R6Class("OGS6_h5",
    public = list(
        #' @describtion This function will be used after a simulation is run to
        #' give an overview of the \code{h5} output data.
        initialize = function(h5_path) {

            assertthat::is.string(h5_path)
            assertthat::assert_that(file.exists(h5_path))
            private$h5_info <- rhdf5::h5ls(h5_path)
            private$h5_path <- h5_path

        },
        #' @description Overrides the default print method
        print = function(){
            cat("OGS6_h5\n")
            cat("h5 path:\n")
            cat(private$h5_path, "\n\n")
            cat("# h5 file structure",
                paste0(rep("-", cli::console_width() - 20), collapse = ""),
                "\n")
            print(private$h5_info)
        },
        #' @desctiption return a h5 object for further processing with the
        #' \code{rhdf5} package.
        #' @param name Optional: *character* that indicates the element of the h5
        #' file to access. Default *"/"* will return the entire \code{rhdf5} object.
        #' @param ... Optional: Further arguments to be passed to the function
        #' \code{h5read}.
        #' @value A list of data elements or the element acessed with \code{name}
        get_h5 = function(name = "/", ...) {
            valid_names <- c("/",
                apply(private$h5_info[, 1:2], 1, function(x) {
                    gsub("//", "/", paste0(x, collapse = "/"))
            })
            )
            assertthat::assert_that(name %in% valid_names)
            return(rhdf5::h5read(file = private$h5_path, name = name))
        }

    ),
    private = list(
        h5_info = NULL,
        h5_path = NULL
    ))