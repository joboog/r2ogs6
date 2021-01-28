
#===== OGS6_pvd =====


#'OGS6_pvd
#'@description Constructor for the OGS6_pvd base class
#'@export
OGS6_pvd <- R6::R6Class(
    "OGS6_pvd",
    public = list(

        #'@description
        #'Creates new OGS6_pvd object
        #'@param pvd_path string: Path to .pvd file
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

        #'@description
        #'Returns .vtu path for specified timestep
        #'@param timestep string: Timestep
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

        #'@description
        #'Returns timestep for specified .vtu path
        #'@param vtu_path string: .vtu path
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

        #'@description
        #'Returns a tibble with all of the PointData
        #'@param point_ids numeric: Optional: Point IDs. Defaults to all.
        #'@param Names character: Optional: `Name` attributes of `DataArray`
        #' elements. Defaults to all.
        #'@param start_at_timestep number: Optional: Timestep to start at.
        #' Defaults to first timestep.
        #'@param end_at_timestep number: Optional: Timestep to end at. Defaults
        #' to last timestep.
        get_point_data_tbl = function(point_ids,
                                      Names,
                                      start_at_timestep,
                                      end_at_timestep){

            if(missing(point_ids)){
                max_id <- self$OGS6_vtus[[1]]$number_of_points - 1
                point_ids <- seq(0, max_id)
            }

            if(missing(Names)){
                Names <- as.character(self$OGS6_vtus[[1]]$point_data$keys())
            }

            private$get_data_tbl(data_type = "points",
                                 ids = point_ids,
                                 Names = Names,
                                 start_at_timestep = start_at_timestep,
                                 end_at_timestep = end_at_timestep)
        },

        #'@description
        #'Returns a tibble with all of the CellData
        #'@param cell_ids numeric: Optional: Cell IDs. Defaults to all.
        #'@param Names character: Optional: `Name` attributes of `DataArray`
        #' elements. Defaults to all.
        #'@param start_at_timestep number: Optional: Timestep to start at.
        #' Defaults to first timestep.
        #'@param end_at_timestep number: Optional: Timestep to end at. Defaults
        #' to last timestep.
        get_cell_data_tbl = function(cell_ids,
                                     Names,
                                     start_at_timestep,
                                     end_at_timestep){

            if(missing(cell_ids)){
                max_id <- self$OGS6_vtus[[1]]$number_of_cells - 1
                cell_ids <- seq(0, max_id)
            }

            if(missing(Names)){
                Names <- as.character(self$OGS6_vtus[[1]]$cell_data$keys())
            }

            private$get_data_tbl(data_type = "cells",
                                 ids = cell_ids,
                                 Names = Names,
                                 start_at_timestep = start_at_timestep,
                                 end_at_timestep = end_at_timestep)
        }
    ),

    active = list(

        #'@field pvd_path
        #'Getter for private parameter '.pvd_path'
        pvd_path = function() {
            private$.pvd_path
        },

        #'@field datasets
        #'Getter for private parameter '.datasets'
        datasets = function() {
            private$.datasets
        },

        #'@field vtu_paths
        #'Getter for `datasets` `file`
        vtu_paths = function() {

            vtu_paths <- lapply(private$.datasets, function(x){
                x[["file"]]
            })
        },

        #'@field abs_vtu_paths
        #'Gets absolute .vtu paths, e.g. `dirname(pvd_path)` + `datasets` `file`
        abs_vtu_paths = function() {

            abs_vtu_paths <- lapply(self$vtu_paths, function(x){
                abs_vtu_path <- paste0(dirname(self$pvd_path), "/", x)
                return(invisible(abs_vtu_path))
            })
        },

        #'@field timesteps
        #'Gets timesteps from private parameter '.datasets'
        timesteps = function() {

            timesteps <- lapply(private$.datasets, function(x){
                as.double(x[["timestep"]])
            })
        },

        #'@field OGS6_vtus
        #'Getter for private parameter '.OGS6_vtus'
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

        #Returns a dataframe with all of the CellData
        get_data_tbl = function(data_type,
                                ids,
                                Names,
                                start_at_timestep,
                                end_at_timestep){

            assertthat::assert_that(is.numeric(ids))
            assertthat::assert_that(is.character(Names))

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

                    for(j in seq_len(length(Names))){

                        rid <- ids[[i]] + 1

                        if(length(
                            dim(data[[Names[[j]]]])) == 1){
                            value <- data[[Names[[j]]]][[rid]]

                        }else{
                            value <- list(as.numeric(
                                data[[Names[[j]]]][rid,]))
                        }

                        values <- c(values, list(value))
                        names(values)[[length(values)]] <- Names[[j]]
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


#===== OGS6_vtu =====


#'OGS6_vtu
#'@description Constructor for the OGS6_vtu base class
#'@export
OGS6_vtu <- R6::R6Class(
    "OGS6_vtu",
    public = list(

        #'@description
        #'Creates new OGS6_vtu object
        #'@param vtu_path string: Path to .vtu file
        initialize = function(vtu_path) {

            vtk_xml_ugr <- vtk$vtkXMLUnstructuredGridReader()
            vtk_xml_ugr$SetFileName(vtu_path)
            vtk_xml_ugr$Update()
            self$vtkUnstructuredGrid <- vtk_xml_ugr$GetOutput()

            point_locator <- vtk$vtkPointLocator()
            point_locator$SetDataSet(self$vtkUnstructuredGrid)
            point_locator$BuildLocator()
            private$.vtkPointLocator <- point_locator

            private$.vtu_path <- vtu_path
        },

        #'@description
        #'Gets FieldData.
        #'@param Names character: Optional: `Name` attributes of `DataArray`
        #' elements, defaults to all in `FieldData`
        #'@return list: List of format list(value_a = 1, value_b = 2), where the
        #' names reference the `Name` attributes of the `DataArray` elements
        get_data_for_field = function(Names){

            if(missing(Names)){
                Names <- as.character(self$field_data$keys())
            }

            field_data <- lapply(Names, function(x){
                self$field_data[[x]]
            })

            return(field_data)
        },

        #'@description
        #'Gets PointData for points with IDs in `point_ids`.
        #'@param point_ids numeric: Optional: Point IDs, defaults to all
        #'@param Names character: Optional: `Name` attributes of `DataArray`
        #' elements, defaults to all in `PointData`
        #'@return tibble: Tibble where each row represents a point.
        get_data_for_points = function(point_ids,
                                       Names){

            if(missing(point_ids)){
                max_point_id <- self$number_of_points() - 1
                point_ids <- seq(0, max_point_id)
            }

            if(missing(Names)){
                Names <- as.character(self$point_data$keys())
            }

            private$get_data(data_type = "points",
                             ids = point_ids,
                             Names = Names)

        },

        #'@description
        #'Gets CellData for cells with IDs in `cell_ids`.
        #'@param cell_ids numeric: Optional: Cell IDs, defaults to all
        #'@param Names character: Optional: `Name` attributes of `DataArray`
        #' elements, defaults to all in `CellData`
        #'@return tibble: Tibble where each row represents a cell.
        get_data_for_cells = function(cell_ids,
                                      Names){

            if(missing(cell_ids)){
                max_cell_id <- self$number_of_cells() - 1
                cell_ids <- seq(0, max_cell_id)
            }

            if(missing(Names)){
                Names <- as.character(self$cell_data$keys())
            }

            private$get_data(data_type = "cells",
                             ids = cell_ids,
                             Names = Names)

        },

        #'@description
        #'Gets coordinates of specific points by their IDs.
        #'@param point_ids numeric: Point IDs
        #'@return If `point_ids` is a number, a coordinate array. If `point_ids`
        #' is a numeric vector with length > 1, a list of coordinate arrays.
        get_point_coords = function(point_ids){
            assertthat::assert_that(is.numeric(point_ids))

            if(assertthat::is.number(point_ids)){
                return(self$points[(point_ids + 1),])
            }

            point_coords <- lapply(point_ids, function(x){
                return(self$points[(x + 1),])
            })

            return(point_coords)
        },

        #'@description
        #'Gets PointData at specified coordinates.
        #'@param coordinates list(numeric): List of coordinates (a coordinate
        #' is a numeric vector of length 3)
        #'@param Names character: Optional: `Name` attributes of `DataArray`
        #' elements, defaults to all in `PointData`
        get_point_data_at = function(coordinates,
                                     Names){

            # Wrap if its a single coordinate
            if(!is.list(coordinates)){
                coordinates <- list(coordinates)
            }

            # Check if contents of list are coordinates
            lapply(coordinates, function(x){
                assertthat::assert_that(is.numeric(x))
                assertthat::assert_that(length(x) == 3)
            })

            if(missing(Names)){
                Names <- as.character(self$point_data$keys())
            }

            assertthat::assert_that(is.character(Names))

            # Use point locator to get data
            point_ids <- lapply(coordinates, function(x){
                self$vtkPointLocator$FindClosestPoint(x)
            })

            return(self$get_data_for_points(point_ids = as.numeric(point_ids),
                                            Names = Names))
        }
    ),

    active = list(

        #'@field vtu_path
        #'Getter for private parameter '.vtu_path'
        vtu_path = function() {
            private$.vtu_path
        },

        #'@field number_of_points
        #'Getter for NumberOfPoints parameter of '.vtkUnstructuredGrid'
        number_of_points = function() {
            self$vtkUnstructuredGrid$GetNumberOfPoints()
        },

        #'@field number_of_cells
        #'Getter for NumberOfCells parameter of '.vtkUnstructuredGrid'
        number_of_cells = function() {
            self$vtkUnstructuredGrid$GetNumberOfCells()
        },

        #'@field points
        #'Getter for Points parameter of '.dsa_vtkUnstructuredGrid'
        points = function() {
            self$dsa_vtkUnstructuredGrid$Points
        },

        #'@field cells
        #'Getter for Cells parameter of '.dsa_vtkUnstructuredGrid'
        cells = function() {
            self$dsa_vtkUnstructuredGrid$Cells
        },

        #'@field field_data
        #'Getter for FieldData parameter of '.dsa_vtkUnstructuredGrid'
        field_data = function() {
            self$dsa_vtkUnstructuredGrid$FieldData
        },

        #'@field point_data
        #'Getter for PointData parameter of '.dsa_vtkUnstructuredGrid'
        point_data = function() {
            self$dsa_vtkUnstructuredGrid$PointData
        },

        #'@field cell_data
        #'Getter for CellData parameter of '.dsa_vtkUnstructuredGrid'
        cell_data = function() {
            self$dsa_vtkUnstructuredGrid$CellData
        },

        #'@field vtkPointLocator
        #'Getter for private parameter '.vtkPointLocator'
        vtkPointLocator = function() {
            private$.vtkPointLocator
        },

        #'@field vtkUnstructuredGrid
        #'Access to private parameter '.vtkUnstructuredGrid'
        vtkUnstructuredGrid = function(value) {
            if(missing(value)) {
                private$.vtkUnstructuredGrid
            }else{
                # Check class
                private$.vtkUnstructuredGrid <- value
                private$.dsa_vtkUnstructuredGrid <-
                    vtk$numpy_interface$dataset_adapter$WrapDataObject(value)
            }
        },

        #'@field dsa_vtkUnstructuredGrid
        #'Getter for private parameter '.dsa_vtkUnstructuredGrid'
        dsa_vtkUnstructuredGrid = function() {
            private$.dsa_vtkUnstructuredGrid
        }
    ),

    private = list(

        get_data = function(data_type,
                            ids,
                            Names){

            assertthat::assert_that(is.numeric(ids))
            assertthat::assert_that(is.character(Names))

            tbl_rows <- list()

            for (i in seq_len(length(ids))) {
                if (data_type == "points") {
                    data <- self$point_data
                    coords <- self$get_point_coords(ids[[i]])
                    extra_columns <- list(x = coords[[1]],
                                          y = coords[[2]],
                                          z = coords[[3]])
                } else{
                    data <- self$cell_data
                    extra_columns <- list()
                }

                new_row <- list(id = ids[[i]])
                new_row <- c(new_row, extra_columns)


                values <- list()

                for (j in seq_len(length(Names))) {
                    rid <- ids[[i]] + 1

                    if (length(dim(data[[Names[[j]]]])) == 1) {
                        value <- data[[Names[[j]]]][[rid]]

                    } else{
                        value <- list(as.numeric(data[[Names[[j]]]][rid,]))
                    }

                    values <- c(values, list(value))
                    names(values)[[length(values)]] <-
                        Names[[j]]
                }

                new_row <- c(new_row,
                             values)

                new_row <- tibble::as_tibble_row(new_row)
                tbl_rows <- c(tbl_rows, list(new_row))
            }

            tbl <- dplyr::bind_rows(tbl_rows)
            return(tbl)
        },

        .vtu_path = NULL,
        .vtkPointLocator = NULL,
        .vtkUnstructuredGrid = NULL,
        .dsa_vtkUnstructuredGrid = NULL
    )
)


#===== generate_structured_mesh =====


#'generate_structured_mesh
#'@description Wrapper function to call generateStructuredMesh.exe
#' (VTK mesh generator). For full documentation see
#'https://www.opengeosys.org/docs/tools/meshing/structured-mesh-generation/
#'@param args_str string: The arguments the script will be called with
#'@param ogs_bin_path string: Optional: Path to OpenGeoSys6 bin folder.
#' Defaults to options("r2ogs6.default_ogs_bin_path").
#'@return string: .vtu file path
#'@export
generate_structured_mesh = function(args_str,
                                    ogs_bin_path) {

    if(missing(ogs_bin_path)){
        ogs_bin_path <- unlist(options("r2ogs6.default_ogs_bin_path"))
    }

    assertthat::assert_that(assertthat::is.string(ogs_bin_path))
    assertthat::assert_that(assertthat::is.string(args_str))


    # Get .vtu path from args_str
    vtu_path <- stringr::str_extract(args_str, "-o [^ ]*")
    vtu_path <- stringr::str_remove(vtu_path, "-o ")

    system(command = paste0(ogs_bin_path,
                            "generateStructuredMesh.exe ",
                            args_str))

    return(invisible(vtu_path))
}
