
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
        get_vtu_path_by_timestep = function(timestep){

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
        get_timestep_by_vtu_path = function(vtu_path){

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
        #'Creates a tibble object from PointData
        #'@param point_ids numeric: Optional: IDs of the points of interest.
        #' Will default to all point IDs if not defined.
        #'@param Names character: `Name` attributes of `DataArray` elements
        #'@param start_at_timestep number: Timestep to start at
        #'@param end_at_timestep number: Timestep to end at
        get_PointData_time_tibble = function(point_ids,
                                             Names,
                                             start_at_timestep,
                                             end_at_timestep){

            if(missing(point_ids)){
                max_point_id <- self$OGS6_vtus[[1]]$get_number_of_points() - 1
                point_ids <- seq(0, max_point_id)
            }

            assertthat::assert_that(is.numeric(point_ids))

            if(missing(start_at_timestep)){
                start_at_timestep <- self$timesteps[[1]]
            }

            if(missing(end_at_timestep)){
                end_at_timestep <- self$timesteps[[length(self$timesteps)]]
            }

            assertthat::assert_that(assertthat::is.number(start_at_timestep))
            assertthat::assert_that(assertthat::is.number(end_at_timestep))

            relevant_vtus <- list()

            for(i in seq_len(length(self$OGS6_vtus))){
                timestep <- self$get_timestep_by_vtu_path(self$vtu_paths[[i]])

                if(timestep >= start_at_timestep &&
                   timestep <= end_at_timestep){
                    relevant_vtus <- c(relevant_vtus, list(self$OGS6_vtus[[i]]))
                }
            }

            time_list <- list()

            # For each .vtu file referenced in pvd_path...
            for(i in seq_len(length(relevant_vtus))){

                new_row <- list()
                timestep_name <- paste0("t", i)

                # ... get all rows of PointData or get rows by Name
                if(missing(Names)){
                    Names <- names(relevant_vtus[[i]]$get_PointData())
                }

                assertthat::assert_that(is.character(Names))

                for (j in seq_len(length(point_ids))) {
                    point_data <-
                        relevant_vtus[[i]]$get_PointData_for_point(
                            point_ids[[j]],
                            Names)

                    new_row <- c(new_row,
                                 list(list(point_data)))
                    names(new_row[[length(new_row)]]) <- timestep_name
                    names(new_row)[[length(new_row)]] <- paste0("p", (j - 1))
                }

                time_list <- c(time_list,
                               list(tibble::as_tibble_row(new_row)))
                names(time_list)[[length(time_list)]] <- timestep_name
            }

            # Combine into tibble
            time_tibble <- dplyr::bind_rows(time_list)

            return(time_tibble)
        },

        #'@description
        #'Gets PointData at specified timestep. Calls
        #' `get_PointData_time_tibble` internally with `start_at_timestep` and
        #' `end_at_timestep` both being `timestep`
        #'@param point_ids number: Point IDs
        #'@param Names character: `Name` attributes of `DataArray` elements
        #'@param timestep string: Timestep
        get_PointData_at_timestep = function(point_ids,
                                             Names,
                                             timestep){

            self$get_PointData_time_tibble(point_ids = point_ids,
                                           Names = Names,
                                           start_at_timestep = timestep,
                                           end_at_timestep = timestep)
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

            private$.vtu_path <- vtu_path
            self$vtkUnstructuredGrid <- vtk_xml_ugr$GetOutput()
        },

        #'@description
        #'Gets PointData for point with ID `point_id`
        #'@param point_id number: Point ID
        #'@param Names character: Optional: `Name` attributes of `DataArray`
        #' elements, defaults to all in `PointData`
        get_PointData_for_point = function(point_id,
                                           Names){

            if(missing(Names)){
                Names <- names(self$get_PointData())
            }

            assertthat::assert_that(assertthat::is.number(point_id))
            assertthat::assert_that(is.character(Names))

            point_data <- list()

            for(i in seq_len(length(Names))){
                point_data <-
                    c(point_data,
                      list(self$get_PointData(Names[[i]])[[(point_id + 1)]]))
                names(point_data)[[length(point_data)]] <- Names[[i]]
            }

            return(point_data)
        },

        #'@description
        #'Gets PointData of `DataArray` element with `Name` attribute
        #'@param Name string: Optional: `Name` attribute of `DataArray`
        #' elements, defaults to all in `PointData`
        get_PointData = function(Name){

            if(missing(Name)){
                return(self$dsa_wrapped_vtkUnstructuredGrid$PointData)
            }

            assertthat::assert_that(assertthat::is.string(Name))
            return(self$dsa_wrapped_vtkUnstructuredGrid$PointData[[Name]])
        },

        #'@description
        #'Gets number of points
        #'@return number: The number of points
        get_number_of_points = function(){
            return(self$vtkUnstructuredGrid$GetNumberOfPoints())
        }
    ),

    active = list(

        #'@field vtu_path
        #'Getter for private parameter '.vtu_path'
        vtu_path = function() {
            private$.vtu_path
        },

        #'@field vtkUnstructuredGrid
        #'Access to private parameter '.vtkUnstructuredGrid'
        vtkUnstructuredGrid = function(value) {
            if(missing(value)) {
                private$.vtkUnstructuredGrid
            }else{
                # Check class
                private$.vtkUnstructuredGrid <- value
                private$.dsa_wrapped_vtkUnstructuredGrid <-
                    vtk_dsa$WrapDataObject(value)
            }
        },

        #'@field dsa_wrapped_vtkUnstructuredGrid
        #'Getter for private parameter '.dsa_wrapped_vtkUnstructuredGrid'
        dsa_wrapped_vtkUnstructuredGrid = function() {
            private$.dsa_wrapped_vtkUnstructuredGrid
        }
    ),

    private = list(
        .vtu_path = NULL,
        .vtkUnstructuredGrid = NULL,
        .dsa_wrapped_vtkUnstructuredGrid = NULL
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
