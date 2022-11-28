

#' OGS6_h5
#' @description Small class to wrap \code{h5} data into the \code{r2ogs6} workflow.
#' @export
#' @importFrom R6 R6Class
OGS6_h5 <- R6::R6Class("OGS6_h5",
    public = list(
        #' @description This function will be called automatically after a
        #' simulation is run to give an overview of the \code{h5} output data.
        #' @param h5_path path to \code{*.h5} file.
        #' @examples
        #' h5_path <- system.file("/extdata/benchmarks/EllipticPETSc",
        #'                        "cube_1e3_np3.h5",
        #'                         package = "r2ogs6")
        #' ogs6_h5 <- OGS6_h5$new(h5_path)
        initialize = function(h5_path) {

            assertthat::is.string(h5_path)
            assertthat::assert_that(file.exists(h5_path))
            private$h5_info <- rhdf5::h5ls(h5_path)
            private$.h5_path <- h5_path
            private$valid_names <-
                c("/",
                  apply(private$h5_info[, 1:2], 1,
                        function(x) {
                            gsub("//", "/", paste0(x, collapse = "/"))
                        }
                        )
                  )
        },
        #' @description Overrides the default print method
        #' @examples
        #' h5_path <- system.file("/extdata/benchmarks/EllipticPETSc",
        #'                        "cube_1e3_np3.h5",
        #'                         package = "r2ogs6")
        #' ogs6_h5 <- OGS6_h5$new(h5_path)
        #' ogs6_h5
        #' \dontrun{ogs6_obj$h5s}
        print = function(){
            cat("OGS6_h5\n")
            cat("h5 path:\n")
            cat(private$.h5_path, "\n\n")
            cat("# h5 file structure",
                paste0(rep("-", cli::console_width() - 20), collapse = ""),
                "\n")
            print(private$h5_info)
        },
        #' @title wrapper for rhdf5::h5read()
        #' @description return a h5 object for further processing with the
        #' \code{rhdf5} package.
        #' @param name Optional: *character* that indicates the element of the h5
        #' file to access. Default *"/"* will return the entire file as a list.
        #' @param ... Optional: Further arguments to be passed to the function
        #' \code{h5read}.
        #' @return A list of data elements or the element accessed with
        #' \code{name}
        #' @examples
        #' \dontrun{h5_list <- ogs6_obj$h5s[[1]]$get_h5("/times")}
        get_h5 = function(name = "/", ...) {
            assertthat::assert_that(name %in% private$valid_names)
            return(rhdf5::h5read(file = private$.h5_path, name, ...))
        },

        #' @description Method to retrieve \code{HDF5} output as a \code{tibble}
        #' assuming a standardized structure of \code{OGS6 HDF5} output.
        #' times and geometry are added by default.
        #' @details NOTE: This is a beta-version and is only guaranteed to work
        #'  with the current three existing [benchmarks](https://doxygen.opengeosys.org/d9/d28/ogs_file_param__prj__time_loop__output__hdf.html)
        #' that contain *.h5 files. More complicated files can always be handled
        #' with the method \code{$get_h5} and may refer to the package \code{rhdf5}.
        #' @param group *character*
        #' @param names *character* names for a hdf5 element such as displayed
        #' when printing the OGS5_h5 object, without the leading "/".
        #' @examples
        #' h5_path <- system.file("/extdata/benchmarks/EllipticPETSc",
        #'                        "cube_1e3_np3.h5",
        #'                        package = "r2ogs6")
        #' ogs6_h5 <- OGS6_h5$new(h5_path)
        #' df <- ogs6_h5$get_df("/t_0", "pressure")
        #' \dontrun{df <- ogs6_obj$h5s[[1]]$get_df("/t_0", "pressure")}
        #' @importFrom dplyr bind_cols
        get_df = function(group, names = "geometry") {

            assertthat::is.string(group)
            assertthat::assert_that(length(group) == 1)
            assertthat::assert_that(is.character(names))

            name_paths <- apply(cbind(group, names), 1,
                  function(x) { # escape double //
                      gsub("//", "/", paste0(x, collapse = "/"))
                  }
            )
            assertthat::assert_that(all(name_paths %in%
                                            private$valid_names),
               msg = paste0("Name ",
                            names[which(!name_paths %in%
                                            private$valid_names)],
                            " not found in file ",
                            basename(private$.h5_path)
                                           ))

            lst <- private$add_geo_time(group) # adds geometry and time

            for (i in seq_along(names)) {

                lst <- switch(names[i],
                       # more methods for special variables can be called here
                       "times" = next, # added by default
                       "geometry" = next, # added by default
                       "v" = stop("v not yet implemented"),
                       "MaterialIDs" = private$add_materialIDs(name_paths[i],
                                                               lst),
                       private$add_default(name_paths[i], lst)
                       )
            }
            assertthat::assert_that(all(sapply(lst, length) == length(lst[[1]])),
                msg = "Whoops O_o some unmatching dimensions. Please consider using $get_h5()")

            return(dplyr::bind_cols(lst))
        }
    ),
    # === active fields

    active = list(
        #' @field h5_path
        #' Getter/setter for private parameter `.h5_path`
        h5_path = function(value) {
            if (missing(value)) {
                private$.h5_path
            } else {
                assertthat::assert_that(assertthat::is.string(value))
                private$.h5_path <- value
            }
        }
    ),
    private = list(
        h5_info = NULL,
        .h5_path = NULL,
        valid_names = NULL,

        add_geo_time = function(group) {

            # Method is called when the method get_df() is
            # used. Attempts to format geometry and time of the simulation into
            # a tibble in "long" format.

            if (paste0(group, "/geometry") %in% private$valid_names) {
                geo <- self$get_h5(paste0(group, "/geometry"))
                if (length(dim(geo)) == 3) { # is array
                    assertthat::assert_that(dim(geo)[3] == 1,
                       msg = "Can't handle 3d arrays. Please use $get_h5() :)")
                    if (dim(geo)[2] != 3) { # transpose or not
                        geo <- as.matrix(t(geo[, , 1]))
                    } else {
                        geo <- as.matrix(geo[, , 1])
                    }
                } else if (length(dim(geo)) == 2) { # is matrix
                    if (dim(geo)[1] == 3) { # transpose or not
                        geo <- as.matrix(t(geo))
                    } else {
                        geo <- as.matrix(geo)
                    }
                    }
                ngeo <- nrow(geo)
            } else { # hypothetical case of no geometry
                ngeo <- 1L # dont expand time vector
                geo <- matrix(c(0, 0, 0), nrow = 1)
            }
            if ("/times" %in% private$valid_names) {
                tim <- self$get_h5("/times")
                ntim <- length(tim)
            } else {
                tim <- 0
                ntim <- 1L # dont expand geo matrix
            }
            # expand geometry and time into long format
            # stack geo ntim times
            geo <- as.matrix(geo) %x% rep(1, ntim) # kroneker product
            # same time ngeo times
            tim <- rep(tim, each = ngeo)

            return(list(
                x = geo[, 1],
                y = geo[, 2],
                z = geo[, 3],
                time = tim
            ))
        },
        add_materialIDs = function(name_path, lst) {

            # Method is called when the method get_df() is
            # used for the h5 name "MaterialIDs".

            df <- self$get_h5(name_path)
            name <- basename(name_path)
            rep_n <- length(lst[[1]]) / length(c(df))

            if (!(rep_n%%1 == 0L)) {
                stop(paste0("The data is not a multiple of ", name))
            }
            # stack ids
            lst[[name]] <- rep(c(df), rep_n)
            return(lst)
        },
        add_default = function(name_path, lst) {

            # Method that gets called when a variable is attempted
            # to be added for which no method is yet defined.

            df <- self$get_h5(name_path)
            name <- basename(name_path)

            if (length(df) == length(lst[[1]]) |
                length(c(df)) == length(lst[[1]])) {

                lst[[name]] <- c(df)

            } else if (nrow(df) == length(lst[[1]])) {

                for (i in 1:nrow(df)) {
                    lst[[paste0(name, i)]] <- df[, i]
                }
            } else {
                lst[[name]] <- c(df[, -1])
            }
            return(lst)
        }
    ))