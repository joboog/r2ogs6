
#===== OGS6_pvd =====


#' OGS6_pvd
#' @description Constructor for the OGS6_pvd base class
#' @export
#' @importFrom R6 R6Class
OGS6_pvd <- R6::R6Class(
    "OGS6_pvd",
    public = list(

        #' @description
        #' Creates new OGS6_pvd object
        #' @param pvd_path string: Path to .pvd file
        initialize = function(pvd_path) {

            xml_doc <- validate_read_in_xml(pvd_path)
            dataset_nodes <- xml2::xml_find_all(xml_doc,
                                                "/VTKFile/Collection/DataSet")

            private$.pvd_path <- pvd_path
            private$.datasets <- lapply(dataset_nodes,
                                        node_to_object)
            private$.OGS6_vtus <- lapply(self$abs_vtu_paths,
                                         OGS6_vtu$new)

        },

        #' @description
        #' Overrides default printing behaviour
        print = function(){
            cat("OGS6_pvd\n")
            cat("number of referenced .vtu paths (= number of timesteps):  ",
                length(self$abs_vtu_paths),
                "\n", sep = "")
            cat("\n.vtu paths (absolute):\n",
                paste(self$abs_vtu_paths, collapse = "\n"),
                "\n", sep = "")

            cat("\ntimesteps:\n",
                paste(self$timesteps, collapse = "\n"),
                "\n", sep = "")

            cat("\nfirst OGS6_vtu in OGS6_vtus:\n")
            print(self$OGS6_vtus[[1]])

            invisible(self)
        },

        #' @description
        #' Returns .vtu path for specified timestep
        #' @param timestep string: Timestep
        vtu_by_timestep = function(timestep){

            assertthat::assert_that(assertthat::is.number(timestep))

            for(i in seq_len(length(self$timesteps))){
                if(self$timesteps[[i]] == timestep){
                    return(self$vtu_paths[[i]])
                }
            }

            warning(paste("No .vtu path found for timestep", timestep),
                    call. = FALSE)
        },

        #' @description
        #' Returns timestep for specified .vtu path
        #' @param vtu_path string: .vtu path
        timestep_by_vtu = function(vtu_path){

            assertthat::assert_that(assertthat::is.string(vtu_path))

            for(i in seq_len(length(self$vtu_paths))){
                if(self$vtu_paths[[i]] == vtu_path){
                    return(self$timesteps[[i]])
                }
            }

            warning(paste("No timestep found for .vtu path", vtu_path),
                    call. = FALSE)
        },

        #' @description
        #' Returns a tibble containing point data
        #' @param coordinates list(numeric): List of coordinates (a coordinate
        #'   is a numeric vector of length 3)
        #' @param keys character: Optional: `Name` attributes of `DataArray`
        #'   elements. Defaults to all.
        #' @param start_at_timestep number: Optional: Timestep to start at.
        #'   Defaults to first timestep.
        #' @param end_at_timestep number: Optional: Timestep to end at. Defaults
        #'   to last timestep.
        get_point_data_at = function(coordinates,
                                     keys,
                                     start_at_timestep,
                                     end_at_timestep){

            coordinates <- validate_coordinates(coordinates)

            if(missing(keys)){
                keys <- as.character(self$point_data$keys())
            }

            assertthat::assert_that(is.character(keys))

            # Use point locator to get data
            point_ids <- lapply(coordinates, function(x){
                self$OGS6_vtus[[1]]$vtkPointLocator$FindClosestPoint(x)
            })

            return(self$get_point_data(point_ids = as.numeric(point_ids),
                                       keys = keys,
                                       start_at_timestep = start_at_timestep,
                                       end_at_timestep = end_at_timestep))
        },

        #' @description
        #' Returns a tibble containing point data
        #' @param point_ids numeric: Optional: Point IDs. Defaults to all.
        #' @param keys character: Optional: `Name` attributes of `DataArray`
        #'   elements. Defaults to all.
        #' @param start_at_timestep number: Optional: Timestep to start at.
        #'   Defaults to first timestep.
        #' @param end_at_timestep number: Optional: Timestep to end at. Defaults
        #'   to last timestep.
        get_point_data = function(point_ids,
                                  keys,
                                  start_at_timestep,
                                  end_at_timestep){

            if(missing(point_ids)){
                max_id <- self$OGS6_vtus[[1]]$number_of_points - 1
                point_ids <- seq(0, max_id)
            }

            if(missing(keys)){
                keys <- as.character(self$OGS6_vtus[[1]]$point_data$keys())
            }

            private$get_data(
                data_type = "points",
                ids = point_ids,
                keys = keys,
                start_at_timestep = start_at_timestep,
                end_at_timestep = end_at_timestep
            )
        },

        #' @description
        #' Returns a tibble containing cell data
        #' @param cell_ids numeric: Optional: Cell IDs. Defaults to all.
        #' @param keys character: Optional: `Name` attributes of `DataArray`
        #'   elements. Defaults to all.
        #' @param start_at_timestep number: Optional: Timestep to start at.
        #'   Defaults to first timestep.
        #' @param end_at_timestep number: Optional: Timestep to end at. Defaults
        #'   to last timestep.
        get_cell_data = function(cell_ids,
                                 keys,
                                 start_at_timestep,
                                 end_at_timestep){

            if(missing(cell_ids)){
                max_id <- self$OGS6_vtus[[1]]$number_of_cells - 1
                cell_ids <- seq(0, max_id)
            }

            if(missing(keys)){
                keys <- as.character(self$OGS6_vtus[[1]]$cell_data$keys())
            }

            private$get_data(
                data_type = "cells",
                ids = cell_ids,
                keys = keys,
                start_at_timestep = start_at_timestep,
                end_at_timestep = end_at_timestep
            )
        }
    ),

    active = list(

        #' @field pvd_path
        #' Getter for private parameter '.pvd_path'
        pvd_path = function() {
            private$.pvd_path
        },

        #' @field datasets
        #' Getter for private parameter '.datasets'
        datasets = function() {
            private$.datasets
        },

        #' @field vtu_paths
        #' Getter for `datasets` `file`
        vtu_paths = function() {

            vtu_paths <- lapply(private$.datasets, function(x){
                x[["file"]]
            })
        },

        #' @field abs_vtu_paths
        #' Gets absolute .vtu paths, e.g. `dirname(pvd_path)` + `datasets`
        #' `file`
        abs_vtu_paths = function() {

            abs_vtu_paths <- lapply(self$vtu_paths, function(x){
                abs_vtu_path <- paste0(dirname(self$pvd_path), "/", x)
                return(invisible(abs_vtu_path))
            })
        },

        #' @field last_timestep
        #' Gets last timestep
        last_timestep = function() {
            self$timesteps[[length(self$timesteps)]]
        },

        #' @field timesteps
        #' Gets timesteps from private parameter `datasets`
        timesteps = function() {

            timesteps <- lapply(private$.datasets, function(x){
                as.double(x[["timestep"]])
            })
        },

        #' @field OGS6_vtus
        #' Getter for private parameter `.OGS6_vtus`
        OGS6_vtus = function() {
            private$.OGS6_vtus
        }
    ),

    private = list(

        # Gets sublist from referenced .vtus by timesteps
        get_relevant_vtus = function(start_at_timestep,
                                     end_at_timestep){

            assertthat::assert_that(assertthat::is.number(start_at_timestep))
            assertthat::assert_that(assertthat::is.number(end_at_timestep))

            relevant_vtus <- list()

            for(i in seq_len(length(self$OGS6_vtus))){
                timestep <- self$timestep_by_vtu(self$vtu_paths[[i]])

                if(timestep >= start_at_timestep &&
                   timestep <= end_at_timestep){
                    relevant_vtus <- c(relevant_vtus, list(self$OGS6_vtus[[i]]))
                }
            }

            return(relevant_vtus)
        },

        # Returns a dataframe with all of the CellData
        #' @importFrom dplyr bind_rows
        get_data = function(data_type,
                            ids,
                            keys,
                            start_at_timestep,
                            end_at_timestep){

            assertthat::assert_that(is.numeric(ids))
            assertthat::assert_that(is.character(keys))

            if(missing(start_at_timestep)){
                start_at_timestep <- self$timesteps[[1]]
            }

            if(missing(end_at_timestep)){
                end_at_timestep <- self$timesteps[[length(self$timesteps)]]
            }

            vtus <- private$get_relevant_vtus(start_at_timestep,
                                              end_at_timestep)

            tbl_rows <- list()

            for(i in seq_len(length(ids))){

                timestep_rows <- lapply(vtus, function(vtu){

                    if (data_type == "points") {
                        data <- vtu$point_data
                        coords <- vtu$get_point_coords(ids[[i]])
                        extra_columns <- list(x = coords[[1]],
                                              y = coords[[2]],
                                              z = coords[[3]])
                    } else{
                        data <- vtu$cell_data
                        extra_columns <- list()
                    }

                    new_row <- list(id = ids[[i]])
                    new_row <- c(new_row, extra_columns)

                    values <- list()

                    for(j in seq_len(length(keys))){

                        rid <- ids[[i]] + 1

                        if(length(
                            dim(data[[keys[[j]]]])) == 1){
                            value <- data[[keys[[j]]]][[rid]]

                        }else{
                            value <- list(as.numeric(
                                data[[keys[[j]]]][rid,]))
                        }

                        values <- c(values, list(value))
                        names(values)[[length(values)]] <- keys[[j]]
                    }

                    new_row <- c(new_row, values)

                    new_row <-
                        c(new_row,
                          list(timestep =
                                   self$timestep_by_vtu(
                                       basename(vtu$vtu_path))))

                    new_row <- tibble::as_tibble_row(new_row)
                    return(new_row)
                })

                tbl_rows <- c(tbl_rows, timestep_rows)
            }

            tbl <- dplyr::bind_rows(tbl_rows)
            return(tbl)
        },

        .pvd_path = NULL,
        .datasets = NULL,
        .OGS6_vtus = NULL
    )
)
