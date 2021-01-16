
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
                                         read_in_vtu)

        },

        #'@description
        #'Returns .vtu path for specified timestep
        #'@param timestep string: Timestep
        get_vtu_path_by_timestep = function(timestep){

            assertthat::assert_that(assertthat::is.string(timestep))

            for(i in seq_len(length(private$.datasets))){
                if(private$.datasets[[i]][["timestep"]] == timestep){
                    return(private$.datasets[[i]][["file"]])
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

            for(i in seq_len(length(private$.datasets))){
                if(private$.datasets[[i]][["file"]] == vtu_path){
                    return(private$.datasets[[i]][["timestep"]])
                }
            }

            warning(paste("No timestep found for .vtu path", vtu_path),
                    call. = FALSE)
        },

        #'@description
        #'Creates a tibble object from PointData
        #'@param Names character: `Name` attributes of `DataArray` elements
        get_PointData_time_tibble = function(Names){

            assertthat::assert_that(is.character(Names))

            time_list <- list()

            # For each .vtu file referenced in pvd_path...
            for(i in seq_len(length(self$OGS6_vtus))){

                new_row <- list()

                # ... get row of PointData by Name
                for(j in seq_len(length(Names))){
                    point_data <-
                        self$OGS6_vtus[[i]]$get_PointData_DataArray(Names[[j]])
                    new_row <- c(new_row, list(list(point_data)))
                    names(new_row)[[length(new_row)]] <- Names[[j]]
                }

                time_list <- c(time_list,
                               list(tibble::as_tibble_row(new_row)))
            }

            # Combine into tibble
            time_tibble <- dplyr::bind_rows(time_list)

            return(time_tibble)
        },

        get_PointData_timeline = function(point_id,
                                          Name,
                                          starting_from_timestep,
                                          ending_on_timestep){

            # ...

        },

        #'@description
        #'Gets PointData at specified timestep. Calls `get_PointData_timeline`
        #' internally with `starting_from_timestep` and `ending_on_timestep`
        #' both being `timestep`
        #'@param point_id number: Point ID
        #'@param Name string: `Name` attribute of `DataArray` element
        #'@param timestep string: Timestep
        get_PointData_at_timestep = function(point_id,
                                             Name,
                                             timestep){

            self$get_PointData_timeline(point_id = point_id,
                                        Name = Name,
                                        starting_from_timestep = timestep,
                                        ending_on_timestep = timestep)
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
        #'@return character: .vtu paths as referenced in `pvd_path`, for
        #' absolute paths use `abs_vtu_paths`
        vtu_paths = function() {

            vtu_paths <- lapply(private$.datasets, function(x){
                x[["file"]]
            })
        },

        #'@field abs_vtu_paths
        #'Gets absolute .vtu paths, e.g. `dirname(pvd_path)` + `datasets` `file`
        #'@return character: Absolute .vtu paths
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
                x[["timestep"]]
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
        #'@param vtkUnstructuredGrid
        initialize = function(vtu_path,
                              vtkUnstructuredGrid) {
            self$vtkUnstructuredGrid <- vtkUnstructuredGrid
        },

        get_PointData_DataArray = function(Name){

            if(missing(Name)){
                return(self$dsa_wrapped_vtkUnstructuredGrid$PointData)
            }

            assertthat::assert_that(assertthat::is.string(Name))
            return(self$dsa_wrapped_vtkUnstructuredGrid$PointData[[Name]])
        }

    ),

    active = list(

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
        .vtkUnstructuredGrid = NULL,
        .dsa_wrapped_vtkUnstructuredGrid = NULL
    )
)


#'read_in_vtu
#'@description Reads in .vtu file via `vtkXMLUnstructuredGridReader` from the
#' python `vtk` library
#'@param vtu_path string: Path to .vtu file
#'@return vtkUnstructuredGrid*: Unstructured Grid
#'@export
read_in_vtu <- function(vtu_path) {

    vtk_xml_ugr <- vtk$vtkXMLUnstructuredGridReader()
    vtk_xml_ugr$SetFileName(vtu_path)
    vtk_xml_ugr$Update()

    return(invisible(OGS6_vtu$new(vtu_path,
                                  vtk_xml_ugr$GetOutput())))
}


#===== generate_structured_mesh =====


#'generate_structured_mesh
#'@description Wrapper function to call generateStructuredMesh.exe
#' (VTK mesh generator). For full documentation see
#'https://www.opengeosys.org/docs/tools/meshing/structured-mesh-generation/
#'@param args_str string: The arguments the script will be called with
#'@return string: .vtu file path
#'@export
generate_structured_mesh = function(ogs_bin_path,
                                    args_str) {

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
