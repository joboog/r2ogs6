
#===== OGS6_vtu =====


#'OGS6_vtu
#'@description Constructor for the OGS6_vtu base class
#'@export
OGS6_vtu <- R6::R6Class(
    "OGS6_vtu",
    public = list(

        #'@description
        #'Creates new OGS6_vtu object
        #'@param type string:
        #'@param version string:
        #'@param byte_order string:
        #'@param UnstructuredGrid OGS6_UnstructuredGrid:
        #'@param header_type string: Optional:
        #'@param compressor string: Optional:
        #'@param AppendedData string: Optional:
        initialize = function(type,
                              version,
                              byte_order,
                              UnstructuredGrid,
                              header_type = NULL,
                              compressor = NULL,
                              AppendedData = NULL) {

            self$type <- type
            self$version <- version
            self$byte_order <- byte_order
            self$UnstructuredGrid <- UnstructuredGrid

            if(!is.null(header_type)){
                self$header_type <- header_type
            }

            if(!is.null(compressor)){
                self$compressor <- compressor
            }

            if(!is.null(AppendedData)){
                self$AppendedData <- AppendedData
            }
        }
    ),

    active = list(

        #'@field type
        #'Access to private parameter '.type'
        type = function(value) {
            if(missing(value)) {
                private$.type
            }else{
                assertthat::assert_that(assertthat::is.string(value))
                private$.type <- value
            }
        },

        #'@field version
        #'Access to private parameter '.version'
        version = function(value) {
            if(missing(value)) {
                private$.version
            }else{
                assertthat::assert_that(assertthat::is.string(value))
                private$.version <- value
            }
        },

        #'@field byte_order
        #'Access to private parameter '.byte_order'
        byte_order = function(value) {
            if(missing(value)) {
                private$.byte_order
            }else{
                assertthat::assert_that(assertthat::is.string(value))
                private$.byte_order <- value
            }
        },

        #'@field UnstructuredGrid
        #'Access to private parameter '.UnstructuredGrid'
        UnstructuredGrid = function(value) {
            if(missing(value)) {
                private$.UnstructuredGrid
            }else{
                assertthat::assert_that("OGS6_UnstructuredGrid" %in%
                                            class(value))
                private$.UnstructuredGrid <- value
            }
        },

        #'@field header_type
        #'Access to private parameter '.header_type'
        header_type = function(value) {
            if(missing(value)) {
                private$.header_type
            }else{
                assertthat::assert_that(assertthat::is.string(value))
                private$.header_type <- value
            }
        },

        #'@field compressor
        #'Access to private parameter '.compressor'
        compressor = function(value) {
            if(missing(value)) {
                private$.compressor
            }else{
                assertthat::assert_that(assertthat::is.string(value))
                private$.compressor <- value
            }
        },

        #'@field AppendedData
        #'Access to private parameter '.AppendedData '
        AppendedData  = function(value) {
            if(missing(value)) {
                private$.AppendedData
            }else{
                assertthat::assert_that(assertthat::is.string(value))
                private$.AppendedData  <- value
            }
        },

        #'@field is_subclass
        #'Access to private parameter '.is_subclass'
        is_subclass = function() {
            private$.is_subclass
        },

        #'@field attr_names
        #'Access to private parameter '.attr_names'
        attr_names = function() {
            private$.attr_names
        },

        #'@field flatten_on_exp
        #'Access to private parameter '.flatten_on_exp'
        flatten_on_exp = function() {
            private$.flatten_on_exp
        }
    ),

    private = list(
        .type = NULL,
        .version = NULL,
        .byte_order = NULL,
        .UnstructuredGrid = NULL,
        .header_type = NULL,
        .compressor = NULL,
        .AppendedData = NULL,
        .is_subclass = FALSE,
        .attr_names = c("type",
                        "version",
                        "byte_order",
                        "header_type",
                        "compressor"),
        .flatten_on_exp = character()
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
        #'@param FieldData character, length == 2: Optional:
        initialize = function(Piece,
                              FieldData = NULL) {
            self$Piece <- Piece

            if(!is.null(FieldData)){
                self$FieldData <- FieldData
            }
        }
    ),

    active = list(
        #'@field Piece
        #'Access to private parameter '.Piece'
        Piece = function(value) {
            if (missing(value)) {
                private$.Piece
            }else{
                assertthat::assert_that("OGS6_Piece" %in% class(value))
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

        #'@field attr_names
        #'Access to private parameter '.attr_names'
        attr_names = function() {
            private$.attr_names
        },

        #'@field flatten_on_exp
        #'Access to private parameter '.flatten_on_exp'
        flatten_on_exp = function() {
            private$.flatten_on_exp
        }
    ),

    private = list(
        .Piece = NULL,
        .FieldData = NULL,
        .is_subclass = TRUE,
        .attr_names = character(),
        .flatten_on_exp = character()
    )
)


#===== OGS6_Piece =====


#'OGS6_Piece
#'@description Constructor for the OGS6_Piece base class
#'@export
OGS6_Piece <- R6::R6Class(
    "OGS6_Piece",
    public = list(

        #'@description
        #'Creates new OGS6_Piece object
        #'@param NumberOfPoints string | number:
        #'@param NumberOfCells string | number:
        #'@param PointData list, :
        #'@param Points list, :
        #'@param Cells list, :
        #'@param CellData list, : Optional:
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

            if(!is.null(CellData)){
                self$CellData <- CellData
            }
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
        .attr_names = c("NumberOfPoints",
                        "NumberOfCells"),
        .flatten_on_exp = character()
    )
)


#===== generate_structured_mesh =====


#'generate_structured_mesh
#'@description Wrapper function to call generateStructuredMesh.exe
#' (VTK mesh generator). For full documentation see
#'https://www.opengeosys.org/docs/tools/meshing/structured-mesh-generation/
#'@param ogs6_obj OGS6: Simulation object
#'@param call_str string: The arguments the script will be called with
#' (EXCEPT -o output_file_name, this will be generated automatically!)
#'@param read_in_vtu flag: Should .vtu file just be copied or read in too?
#'@return string: .vtu file path
#'@export
generate_structured_mesh = function(ogs6_obj,
                                    call_str,
                                    read_in_vtu) {

    assertthat::assert_that(inherits(ogs6_obj, "OGS6"))
    assertthat::assert_that(assertthat::is.string(call_str))

    mesh_number <- 1

    is_first <- (length(ogs6_obj$meshes) == 0)

    if(!is_first){
        mesh_number <- length(ogs6_obj$meshes) + 1
    }

    vtu_dir_path <- tempdir()
    vtu_output_filename <- paste0(ogs6_obj$sim_name, "_", mesh_number, ".vtu")
    vtu_path <- paste0(vtu_dir_path, vtu_output_filename)

    system(command = paste0(ogs6_obj$ogs_bin_path, "generateStructuredMesh.exe",
                            " -o ", vtu_path, " ", call_str))

    if(read_in_vtu){
        read_in_vtu(ogs6_obj, vtu_path)
    }else{
        ogs6_obj$add_mesh(vtu_path)
    }

    return(invisible(vtu_path))
}
