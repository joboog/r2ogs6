

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
        print = function(){
            cat("OGS6_h5\n")
            cat("h5 path:\n")
            cat(private$h5_path, "\n\n")
            cat("# h5 file structure",
                paste0(rep("-", cli::console_width() - 20), collapse = ""),
                "\n")
            print(private$h5_info)
        },
        #' wrapper for rhdf5::h5read()
        #' @desctiption return a h5 object for further processing with the
        #' \code{rhdf5} package.
        #' @param name Optional: *character* that indicates the element of the h5
        #' file to access. Default *"/"* will return the entire \code{rhdf5} object.
        #' @param ... Optional: Further arguments to be passed to the function
        #' \code{h5read}.
        #' @value A list of data elements or the element acessed with \code{name}
        get_h5 = function(name = "/", ...) {
            assertthat::assert_that(name %in% private$valid_names)
            return(rhdf5::h5read(file = private$h5_path, name, ...))
        },

        # times and geometry is added as a default
        #' @description Method to retrieve \code{HDF5} output as a \code{tibble}
        #' assuming a standardized structure of \code{OGS6 HDF5} output.
        #' @group *character*
        #' @name *character* name for a hdf5 element such as displayed
        #' when printing the OGS5_h5 object.
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
                            basename(private$h5_path)
                                           ))

            lst <- private$init_lst(group) # adds geometry and time

            for (i in seq_along(names)) {

                lst <- switch(names[i],
                       "times" = next, # added by default
                       "geometry" = next, # added by default
                       "v" = stop("v not yet implemented"),
                       # more methods for special variables can be added here
                       private$add_default(name = names[i],
                                   name_path = name_paths[i],
                                   lst = lst)
                       )
            }
            assertthat::assert_that(all(sapply(lst, length) == length(lst[[1]])),
                msg = "Whoops O_o some unmatching dimensions. Please consider using $get_h5()")

            return(dplyr::bind_cols(lst))
        }

    ),
    private = list(
        h5_info = NULL,
        h5_path = NULL,
        valid_names = NULL,
        init_lst = function(group) {

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
        add_default = function(name, name_path, lst) {
            df <- self$get_h5(name_path)



            return(lst)
        }
    ))