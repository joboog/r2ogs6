# Functions for adding .vtu data
# input: ogs6 class object
# output: updated ogs6 class object

#'new_vtu
#'@description S3 class constructor, returns S3 class describing the .vtu file and any parameters defined in it
#'@param type The type of the vtu file (e. g. "UnstructuredGrid")
#'@param version The version of the .vtu file (e. g. 1.0)
#'@param byte_order Either "LittleEndian" or "BigEndian"
#'@param header_type The header type (e. g. "UInt32")
#'@param compressor Optional: The compressor to be used
new_vtu <- function(type,
                    version,
                    byte_order,
                    header_type,
                    compressor = NULL) {

    assertthat::assert_that(assertthat::is.string(type))
    assertthat::assert_that(is.double(version))
    assertthat::assert_that(assertthat::is.string(byte_order))
    assertthat::assert_that(assertthat::is.string(header_type))

    if(!is.null(compressor)){
        assertthat::assert_that(assertthat::is.string(compressor))
    }

    structure(
        list(unstructured_grid = NULL,
             appended_data = NULL),

        type = type,
        version = version,
        byte_order = byte_order,
        header_type = header_type,
        compressor = compressor,
        class = "vtu")
}

#'new_data_array
#'@description S3 class constructor, returns S3 class describing a .vtu DataArray element
#'@param type
#'@param name
#'@param format
#'@param range_min
#'@param range_max
#'@param number_of_components
#'@param number_of_tuples
#'@param offset
#'@param data
new_data_array <- function(type, name, format, range_min, range_max,
                           number_of_components = NULL, number_of_tuples = NULL,
                           offset = NULL, data = NULL) {
    structure(
        list(type,
             name,
             format,
             range_min,
             range_max,
             offset,
             number_of_components,
             number_of_tuples,
             data),
        class = "data_array"
    )
}

#'as_node.data_array
#'@description Implementation of the generic function as_node for data_array class objects
as_node.data_array <- function(obj) {

    data_array_node <- list(DataArray = structure(list(), type = obj$type, Name = obj$name))

    data_array_node <- add_opt_child(data_array_node, obj$data)

    data_array_node <- add_opt_attr(data_array_node, obj$number_of_components, "NumberOfComponents")
    data_array_node <- add_opt_attr(data_array_node, obj$number_of_tuples, "NumberOfTuples")

    attributes(data_array_node)[["format"]] <- obj$format
    attributes(data_array_node)[["RangeMin"]] <- obj$range_min
    attributes(data_array_node)[["RangeMax"]] <- obj$range_max

    data_array_node <- add_opt_attr(data_array_node, obj$offset, "offset")

    return(data_array_node)
}


#'input_add_vtu_obj
#'@description Adds an empty vtu class object to a ogs6 class object input.
#'@param ogs6_obj The ogs6 object the vtu class object should be added to
#'@param type The type of the .vtu file (e. g. "UnstructuredGrid")
#'@param version The version of the .vtu file (e. g. 1.0)
#'@param byte_order Either "LittleEndian" or "BigEndian"
#'@param header_type The header type (e. g. "UInt32")
#'@param compressor Optional: The compressor to be used
#'@export
input_add_vtu_obj <- function(ogs6_obj, type, version, byte_order, header_type, compressor = NULL) {

    if("vtu_obj" %in% names(ogs6_obj$sim_input)){
        stop("ogs6_obj already has a vtu object attached to it.", call. = FALSE)
    }else{
        ogs6_obj$add_sim_input("vtu_obj", new_vtu(type = type,
                                                  version = version,
                                                  byte_order = byte_order,
                                                  header_type = header_type,
                                                  compressor = compressor))
    }
}


#'input_add_vtu_unstructured_grid
#'@description Adds .vtu UnstructuredGrid to a ogs6 class object
#'@param ogs6_obj The ogs6 object the .vtu UnstructuredGrid should be added to
#'@param piece A Piece (WIP)
#'@param field_data FieldData (WIP)
#'@export
input_add_vtu_unstructured_grid <- function(ogs6_obj, piece, field_data = NULL) {

    #validator missing...

    check_for_obj_of_name(ogs6_obj, "vtu_obj")

    #...
    set_sim_input_obj_param("vtu_obj", "unstructured_grid", list(field_data = field_data,
                                                                 piece = piece))
}


#'input_add_vtu_appended_data
#'@description Adds .vtu AppendedData to a ogs6 class object
#'@param ogs6_obj The ogs6 object the .vtu AppendedData should be added to
#'@param encoding How the AppendedData is encoded
#'@param data The actual data in the specified encoding
#'@export
input_add_vtu_appended_data <- function(ogs6_obj, encoding, data) {

    #validator missing...

    check_for_obj_of_name(ogs6_obj, "vtu_obj")

    assertthat::assert_that(assertthat::is.string(encoding))
    assertthat::assert_that(assertthat::is.string(data))

    if(!(encoding %in% vtu_appended_data_valid_encoding_types)) {
        stop("Invalid encoding for vtu appended data. For an overview of valid
             types refer to ?vtu_appended_data_valid_encoding_types", call. = FALSE)
    }

    set_sim_input_obj_param("vtu_obj", "appended_data", list(encoding = encoding,
                                                             data = data))
}