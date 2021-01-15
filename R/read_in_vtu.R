

#===== read_in_pvd =====


#'read_in_pvd
#'@description Function to read in a .pvd file
#'@param pvd_path string: Path to .pvd file that should be read in
#'@return character: Vector containing .vtu paths
#'@export
read_in_pvd <- function(pvd_path) {

    xml_doc <- validate_read_in_xml(pvd_path)
    xpath_expr <- "/VTKFile"

    root_node <- xml2::xml_find_first(xml_doc, xpath_expr)

    dataset_nodes <- xml2::xml_find_all(root_node, "//Collection/DataSet")
    vtu_refs <- character()

    for(i in seq_len(length(dataset_nodes))){
        vtu_refs <- c(vtu_refs, xml2::xml_attrs(dataset_nodes[[i]])[["file"]])
    }

    return(invisible(vtu_refs))
}


#===== read_in_vtu =====


#'read_in_vtu
#'@description Wrapper function to read in a whole .vtu file as a OGS6_vtu
#' class object
#'@param vtu_path string: Path to .vtu file that should be read in
#'@return OGS6_vtu: Mesh object
#'@export
read_in_vtu <- function(vtu_path) {

    xml_doc <- validate_read_in_xml(vtu_path)
    xpath_expr <- "/VTKFile"

    root_node <- xml2::xml_find_first(xml_doc, xpath_expr)

    vtu_obj <-
        node_to_r2ogs6_class_object(xml_node = root_node,
                                    xpath_expr = xpath_expr,
                                    subclasses_names =
                                        get_subclass_names("OGS6_vtu"))

    return(invisible(vtu_obj))
}


#===== read_in_PointData_DataArray =====


#'read_in_PointData_DataArray
#'@description Wrapper function to read in a `PointData` `DataArray` element
#' from a .vtu file
#'@param vtu_path string: Path to .vtu file that should be read in
#'@param Name string: `Name` attribute of `DataArray` element
#'@export
read_in_PointData_DataArray <- function(vtu_path,
                                     Name) {

    assertthat::assert_that(assertthat::is.string(vtu_path))
    assertthat::assert_that(assertthat::is.string(Name))

    # load vtu
    vtk_xml_ugr <- vtk$vtkXMLUnstructuredGridReader()
    vtk_xml_ugr$SetFileName(vtu_path)
    vtk_xml_ugr$Update()

    # extract data
    wrapped_data <- vtk_dsa$WrapDataObject(vtk_xml_ugr$GetOutput())

    wrapped_data_arr <- wrapped_data$PointData[[Name]]

    return(invisible(wrapped_data_arr))
}


#===== Decoding and decompressing functionality =====


#'decode_appended_data
#'@description Decodes AppendedData
#'@param appended_data character: `AppendedData` parameter of `OGS6_vtu`
#'@param data_arrays list: Content of lists specified in
#' `get_valid_vtu_categories()`
#'@param compressor string: Optional: How the data was compressed, this is the
#' `compressor` parameter of `OGS6_vtu`
#'@return list: DataArrays with `data` element which is the decoded data
decode_appended_data <- function(appended_data,
                                 data_arrays,
                                 compressor = "") {

    assertthat::assert_that(is.character(appended_data))
    assertthat::assert_that(length(appended_data) == 2)

    assertthat::assert_that(assertthat::is.string(compressor))

    encoding <- appended_data[["encoding"]]
    appended_data <- substring(appended_data[["xml_text"]], 2)

    for(i in seq_len(length(data_arrays))){
        offset <- data_arrays[[i]][["offset"]]

        next_offset <- ifelse(i < length(data_arrays),
                              data_arrays[[i+1]][["offset"]],
                              (nchar(appended_data) + 1))

        encoded_data <- substring(appended_data, offset, (next_offset - 1))
        data <- decode_data_array_data(encoded_data,
                                       encoding = encoding,
                                       compressor = compressor)

        data_arrays[[i]] <- c(data_arrays[[i]], data = data)
    }

    return(invisible(data_arrays))
}



decode_data_array_data <- function(data_array_data,
                                   encoding = "",
                                   compressor = ""){

    assertthat::assert_that(assertthat::is.string(data_array_data))
    assertthat::assert_that(assertthat::is.string(encoding))
    assertthat::assert_that(assertthat::is.string(compressor))

    decoded_data <- data_array_data

    # Decode
    if(encoding == "base64"){
        decoded_data <- base64enc::base64decode(decoded_data)
    }else{
        stop(paste("Encoding of AppendedData is not `base64`."), call. = FALSE)
    }

    switch(
        compressor,

        vtkZLibDataCompressor = {

            py_env <- reticulate::py_run_string(
                paste(
                    "import zlib",
                    "def decompress(x):",
                    "\t",
                    "\treturn zlib.decompress(x)",
                    sep = "\n"
                ),
                convert = TRUE
            )

            decoded_data <- py_env$decompress(decoded_data)
        }
    )

    return(invisible(decoded_data))
}


#===== get_vtu_data_arrays_from_file =====


#'get_vtu_data_arrays_from_file
#'@description Reads DataArray elements from a .vtu file
#'@param vtu_path string: .vtu file path
#'@param Names character: Optional: Select `DataArray` elements by `Name`
#' attribute
#'@param categories character: Optional: One or more of `FieldData`, `PointData`,
#' `CellData`, `Points` or `Cells`. If left empty, will get `DataArray`
#' elements from whole XML document.
get_vtu_data_arrays_from_file <- function(vtu_path,
                                          Names = character(),
                                          categories = character()){

    assertthat::assert_that(assertthat::is.string(vtu_path))
    assertthat::assert_that(is.character(Names))
    assertthat::assert_that(is.character(categories))

    valid_categories <- get_valid_vtu_categories()

    lapply(categories, function(x){
        assertthat::assert_that(x %in% valid_categories)
    })

    xml_doc <- validate_read_in_xml(vtu_path)
    xpath_expr <- "/VTKFile/UnstructuredGrid"


    data_array_nodes <- list()

    if(length(categories) != 0){

        for(i in seq_len(length(categories))){

            categories_expr <- ifelse(categories[[i]] != "FieldData",
                                    "/Piece",
                                    "")

            categories_expr <- paste0(xpath_expr,
                                    categories_expr,
                                    "/", categories[[i]], "/DataArray")

            data_array_nodes <- c(data_array_nodes,
                                  list(xml2::xml_find_all(xml_doc,
                                                          categories_expr)))
        }

    }else{
        data_array_nodes <- xml2::xml_find_all(xml_doc, "//DataArray")
    }

    if(length(Names) != 0){
        data_array_nodes <-
            data_array_nodes[xml2::xml_attr(data_array_nodes,
                                            "Name") %in% Names]
    }

    data_arrays <- lapply(data_array_nodes, function(x){
        node_to_object(x)
    })

    return(invisible(data_arrays))
}


