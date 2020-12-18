
#===== OGS6_mesh =====


#'OGS6_mesh
#'@description Constructor for the OGS6_mesh base class
#'@export
OGS6_mesh <- R6::R6Class(
    "OGS6_mesh",
    public = list(

        #'@description
        #'Creates new OGS6_mesh object
        #'@param mesh_path string:
        initialize = function(mesh_path) {
            self$mesh_path <- mesh_path
        }
    ),

    active = list(
        #'@field mesh_path
        #'Access to private parameter '.mesh_path'
        mesh_path = function(value) {
            if (missing(value)) {
                private$.mesh_path
            } else{
                assertthat::assert_that(assertthat::is.string(value))
                private$.mesh_path <- value
                private$.mesh_filename <- basename(value)
            }
        },

        #'@field mesh_filename
        #'Access to private parameter '.mesh_filename'
        mesh_filename = function() {
            private$.mesh_filename
        },

        #'@field is_subclass
        #'Access to private parameter '.is_subclass'
        is_subclass = function() {
            private$.is_subclass
        },

        #'@field subclasses_names
        #'Access to private parameter '.subclasses_names'
        subclasses_names = function() {
            private$.subclasses_names
        },

        #'@field attr_names
        #'Access to private parameter '.attr_names'
        attr_names = function() {
            private$.attr_names
        }
    ),

    private = list(
        .mesh_path = NULL,
        .mesh_filename = NULL,
        .is_subclass = FALSE,
        .subclasses_names = character(),
        .attr_names = character()
    )
)


#===== OGS6_UnstructuredGrid =====


#'OGS6_UnstructuredGrid
#'@description Constructor for the OGS6_UnstructuredGrid base class
#'@export
OGS6_UnstructuredGrid <- R6::R6Class(
    "OGS6_UnstructuredGrid",
    public = list(

        #'@description
        #'Creates new OGS6_UnstructuredGrid object
        #'@param Piece OGS6_Piece:
        #'@param FieldData character, length == 2:
        initialize = function(Piece,
                              FieldData = NULL) {
            self$Piece <- Piece
            self$FieldData <- FieldData
        }
    ),

    active = list(
        #'@field Piece
        #'Access to private parameter '.Piece'
        Piece = function(value) {
            if (missing(value)) {
                private$.Piece
            } else{
                private$.Piece <- value
            }
        },

        #'@field FieldData
        #'Access to private parameter '.FieldData'
        FieldData = function(value) {
            if (missing(value)) {
                private$.FieldData
            } else{
                private$.FieldData <- value
            }
        },

        #'@field is_subclass
        #'Access to private parameter '.is_subclass'
        is_subclass = function() {
            private$.is_subclass
        },

        #'@field subclasses_names
        #'Access to private parameter '.subclasses_names'
        subclasses_names = function() {
            private$.subclasses_names
        },

        #'@field attr_names
        #'Access to private parameter '.attr_names'
        attr_names = function() {
            private$.attr_names
        }
    ),

    private = list(
        .Piece = NULL,
        .FieldData = NULL,
        .is_subclass = TRUE,
        .subclasses_names = character(),
        .attr_names = character()
    )
)


#===== OGS6_Piece =====


OGS6_Piece <- R6::R6Class(
    "OGS6_Piece",
    public = list(
        initialize = function(NumberOfPoints,
                              NumberOfCells,
                              PointData,
                              Points,
                              Cells,
                              CellData = NULL) {

            self$NumberOfPoints <- NumberOfPoints
            self$NumberOfCells <- NumberOfCells
            self$PointData <- PointData
            self$Points <- Points
            self$Cells <- Cells
            self$CellData <- CellData
        }
    ),

    active = list(

        #'@field NumberOfPoints
        #'Access to private parameter '.NumberOfPoints'
        NumberOfPoints = function(value) {
            if (missing(value)) {
                private$.NumberOfPoints
            } else{
                private$.NumberOfPoints <- value
            }
        },

        #'@field NumberOfCells
        #'Access to private parameter '.NumberOfCells'
        NumberOfCells = function(value) {
            if (missing(value)) {
                private$.NumberOfCells
            } else{
                private$.NumberOfCells <- value
            }
        },

        #'@field PointData
        #'Access to private parameter '.PointData'
        PointData = function(value) {
            if (missing(value)) {
                private$.PointData
            } else{
                private$.PointData <- value
            }
        },

        #'@field Points
        #'Access to private parameter '.Points'
        Points = function(value) {
            if (missing(value)) {
                private$.Points
            } else{
                private$.Points <- value
            }
        },

        #'@field Cells
        #'Access to private parameter '.Cells'
        Cells = function(value) {
            if (missing(value)) {
                private$.Cells
            } else{
                private$.Cells <- value
            }
        },

        #'@field CellData
        #'Access to private parameter '.CellData'
        CellData = function(value) {
            if (missing(value)) {
                private$.CellData
            } else{
                private$.CellData <- value
            }
        },

        #'@field is_subclass
        #'Access to private parameter '.is_subclass'
        is_subclass = function() {
            private$.is_subclass
        },

        #'@field subclasses_names
        #'Access to private parameter '.subclasses_names'
        subclasses_names = function() {
            private$.subclasses_names
        },

        #'@field attr_names
        #'Access to private parameter '.attr_names'
        attr_names = function() {
            private$.attr_names
        }
    ),

    private = list(
        .NumberOfPoints = NULL,
        .NumberOfCells = NULL,
        .PointData = NULL,
        .Points = NULL,
        .Cells = NULL,
        .CellData = NULL,
        .is_subclass = TRUE,
        .subclasses_names = character(),
        .attr_names = c("NumberOfPoints",
                        "NumberOfCells")
    )
)


#===== generate_structured_mesh =====


#'generate_structured_mesh
#'@description Wrapper function to call generateStructuredMesh.exe
#' (VTK mesh generator). For full documentation see
#'https://www.opengeosys.org/docs/tools/meshing/structured-mesh-generation/
#'@param ogs6_obj OGS6: Simulation object
#'@param call_str The arguments the script will be called with
#' (EXCEPT -o output_file_name, this will be generated automatically!)
#'@return The newly generated .vtu file path
#'@export
generate_structured_mesh = function(ogs6_obj, call_str) {

    assertthat::assert_that(inherits(ogs6_obj, "OGS6"))
    assertthat::assert_that(assertthat::is.string(call_str))

    mesh_number <- 1

    is_first <- (length(ogs6_obj$meshes) == 0)

    if(!is_first){
        mesh_number <- length(ogs6_obj$meshes) + 1
    }

    mesh_dir_path <- tempdir()
    mesh_output_file_name <- paste0(ogs6_obj$sim_name, "_", mesh_number, ".vtu")
    mesh_path <- paste0(mesh_dir_path, mesh_output_file_name)

    system(command = paste0(ogs6_obj$ogs_bin_path, "generateStructuredMesh.exe",
                            " -o ", mesh_path, " ", call_str))

    ogs6_obj$add_mesh(OGS6_mesh$new(mesh_path))

    return(invisible(mesh_path))
}
