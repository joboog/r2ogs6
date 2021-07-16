
#===== OGS6_vtu =====


#' OGS6_vtu
#' @description Constructor for the `OGS6_vtu` base class
#' @export
OGS6_vtu <- R6::R6Class(
    "OGS6_vtu",
    public = list(

        #' @description
        #' Creates new OGS6_vtu object.
        #' @param vtu_path string: Path to .vtu file
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

        #' @description
        #' Overrides default printing behaviour.
        print = function(){
            cat("OGS6_vtu\n")
            cat("path:  ", self$vtu_path, "\n", sep = "")

            cat("\nfield data keys:\n",
                paste(self$field_data$keys(), collapse = ("\n")),
                "\n", sep = "")

            cat("\nnumber of points:  ", self$number_of_points, "\n", sep = "")
            cat("\npoint data keys:\n",
                paste(self$point_data$keys(), collapse = ("\n")),
                "\n", sep = "")

            cat("\nnumber of cells:  ", self$number_of_cells, "\n", sep = "")
            cat("\ncell data keys:\n",
                paste(self$cell_data$keys(), collapse = ("\n")),
                "\n", sep = "")

            invisible(self)
        },


        #' @description
        #' Gets FieldData.
        #' @param keys character: Optional: `Name` attributes of `DataArray`
        #'   elements, defaults to all in `FieldData`.
        #' @return
        #' A list of format list(value_a = 1, value_b = 2), where
        #' the names reference the `Name` attributes of the `DataArray`
        #' elements.
        get_field_data = function(keys){

            if(missing(keys)){
                keys <- as.character(self$field_data$keys())
            }

            field_data <- lapply(keys, function(x){
                self$field_data[[x]]
            })

            return(field_data)
        },

        #' @description
        #' Gets coordinates of specific points by their IDs.
        #' @param point_ids numeric: Point IDs
        #' @return
        #' If `point_ids` is a number, a coordinate array. If
        #' point_ids` is a numeric vector with length > 1, a list of coordinate
        #' arrays.
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

        #' @description
        #' Gets PointData at specified coordinates.
        #' @param coordinates list(numeric): List of coordinates (a coordinate
        #'   is a numeric vector of length 3)
        #' @param keys character: Optional: `Name` attributes of `DataArray`
        #'   elements, defaults to all in `PointData`
        get_point_data_at = function(coordinates,
                                     keys){

            coordinates <- validate_coordinates(coordinates)

            if(missing(keys)){
                keys <- as.character(self$point_data$keys())
            }

            assertthat::assert_that(is.character(keys))

            # Use point locator to get data
            point_ids <- lapply(coordinates, function(x){
                self$vtkPointLocator$FindClosestPoint(x)
            })

            return(self$get_point_data(point_ids = as.numeric(point_ids),
                                       keys = keys))
        },

        #' @description
        #' Gets PointData for points with IDs in `point_ids`.
        #' @param point_ids numeric: Optional: Point IDs, defaults to all
        #' @param keys character: Optional: `Name` attributes of `DataArray`
        #'   elements, defaults to all in `PointData`
        #' @return A tibble::tibble where each row represents a point.
        get_point_data = function(point_ids,
                                  keys){

            if(missing(point_ids)){
                max_point_id <- self$number_of_points() - 1
                point_ids <- seq(0, max_point_id)
            }

            if(missing(keys)){
                keys <- as.character(self$point_data$keys())
            }

            private$get_data(data_type = "points",
                             ids = point_ids,
                             keys = keys)

        },

        #' @description
        #' Gets CellData for cells with IDs in `cell_ids`.
        #' @param cell_ids numeric: Optional: Cell IDs, defaults to all
        #' @param keys character: Optional: `Name` attributes of `DataArray`
        #'   elements, defaults to all in `CellData`
        #' @return A tibble::tibble where each row represents a cell.
        get_cell_data = function(cell_ids,
                                 keys){

            if(missing(cell_ids)){
                max_cell_id <- self$number_of_cells() - 1
                cell_ids <- seq(0, max_cell_id)
            }

            if(missing(keys)){
                keys <- as.character(self$cell_data$keys())
            }

            private$get_data(data_type = "cells",
                             ids = cell_ids,
                             keys = keys)

        }
    ),

    active = list(

        #' @field vtu_path
        #' Getter for private parameter `.vtu_path`
        vtu_path = function() {
            private$.vtu_path
        },

        #' @field number_of_points
        #' Getter for NumberOfPoints parameter of `.vtkUnstructuredGrid`
        number_of_points = function() {
            self$vtkUnstructuredGrid$GetNumberOfPoints()
        },

        #' @field number_of_cells
        #' Getter for NumberOfCells parameter of `.vtkUnstructuredGrid`
        number_of_cells = function() {
            self$vtkUnstructuredGrid$GetNumberOfCells()
        },

        #' @field points
        #' Getter for Points parameter of `.dsa_vtkUnstructuredGrid`
        points = function() {
            self$dsa_vtkUnstructuredGrid$Points
        },

        #' @field cells
        #' Getter for Cells parameter of `.dsa_vtkUnstructuredGrid`
        cells = function() {
            self$dsa_vtkUnstructuredGrid$Cells
        },

        #' @field field_data
        #' Getter for FieldData parameter of `.dsa_vtkUnstructuredGrid`
        field_data = function() {
            self$dsa_vtkUnstructuredGrid$FieldData
        },

        #' @field point_data
        #' Getter for PointData parameter of `.dsa_vtkUnstructuredGrid`
        point_data = function() {
            self$dsa_vtkUnstructuredGrid$PointData
        },

        #' @field cell_data
        #' Getter for CellData parameter of `.dsa_vtkUnstructuredGrid`
        cell_data = function() {
            self$dsa_vtkUnstructuredGrid$CellData
        },

        #' @field vtkPointLocator
        #' Getter for private parameter `.vtkPointLocator`
        vtkPointLocator = function() {
            private$.vtkPointLocator
        },

        #' @field vtkUnstructuredGrid
        #' Access to private parameter `.vtkUnstructuredGrid`
        vtkUnstructuredGrid = function(value) {
            if(missing(value)) {
                private$.vtkUnstructuredGrid
            }else{
                # Check class
                private$.vtkUnstructuredGrid <- value
                private$.dsa_vtkUnstructuredGrid <- dsa$WrapDataObject(value)
            }
        },

        #' @field dsa_vtkUnstructuredGrid
        #' Getter for private parameter `.dsa_vtkUnstructuredGrid`
        dsa_vtkUnstructuredGrid = function() {
            private$.dsa_vtkUnstructuredGrid
        }
    ),

    private = list(

        get_data = function(data_type,
                            ids,
                            keys){

            assertthat::assert_that(is.numeric(ids))
            assertthat::assert_that(is.character(keys))

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

                for (j in seq_len(length(keys))) {
                    rid <- ids[[i]] + 1

                    if (length(dim(data[[keys[[j]]]])) == 1) {
                        value <- data[[keys[[j]]]][[rid]]

                    } else{
                        value <- list(as.numeric(data[[keys[[j]]]][rid,]))
                    }

                    values <- c(values, list(value))
                    names(values)[[length(values)]] <-
                        keys[[j]]
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


#===== validate_coordinates =====


validate_coordinates <- function(coordinates){

    if(!is.list(coordinates)){
        coordinates <- list(coordinates)
    }

    # Check if contents of list are coordinates
    lapply(coordinates, function(x){
        assertthat::assert_that(is.numeric(x))
        assertthat::assert_that(length(x) == 3)
    })

    return(coordinates)
}


#===== ogs6_generate_structured_mesh =====


#' ogs6_generate_structured_mesh
#' @description Wrapper function to call generateStructuredMesh.exe
#'   (VTK mesh generator). For full documentation see
#'   https://www.opengeosys.org/docs/tools/meshing/structured-mesh-generation/
#' @param args_str string: The arguments the script will be called with
#' @param ogs6_utils_path string: Optional: Path to OpenGeoSys6 utils folder.
#'   Defaults to options("r2ogs6.default_ogs6_utils_path")
#' @return string: .vtu file path
#' @export
ogs6_generate_structured_mesh = function(args_str,
                                         ogs6_utils_path) {

    if(missing(ogs6_utils_path)){
        ogs6_utils_path <- unlist(options("r2ogs6.default_ogs6_utils_path"))
    }

    assertthat::assert_that(assertthat::is.string(ogs6_utils_path))
    assertthat::assert_that(assertthat::is.string(args_str))


    # Get .vtu path from args_str
    vtu_path <- stringr::str_extract(args_str, "-o [^ ]*")
    vtu_path <- stringr::str_remove(vtu_path, "-o ")

    system(command = paste0(ogs6_utils_path,
                            "generateStructuredMesh.exe ",
                            args_str))

    return(invisible(vtu_path))
}
