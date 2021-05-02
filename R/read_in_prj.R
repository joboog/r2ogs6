
#===== read_in_prj =====


#' read_in_prj
#' @description Wrapper function to read in a whole \code{.prj} file
#' @param ogs6_obj OGS6: Simulation object
#' @param prj_path string: Path to the \code{.prj} file that should be read in
#' @param read_in_gml flag: Optional: Should \code{.gml} file just be copied or
#' read in too? If this parameter is missing and the \code{.gml} file contains
#' <= \code{options("r2ogs6.max_lines_gml")}, the \code{.gml} will be read in.
#' Else, only the geometry reference will be saved.
#' @param read_in_vtu flag: Should \code{.vtu} file just be copied or read in
#' too?
#' @export
read_in_prj <- function(ogs6_obj,
                        prj_path,
                        read_in_gml,
                        read_in_vtu = FALSE){

    assertthat::assert_that("OGS6" %in% class(ogs6_obj))
    xml_doc <- validate_read_in_xml(prj_path)

    assertthat::assert_that(assertthat::is.flag(read_in_vtu))

    # Geometry reference
    gml_ref_node <- xml2::xml_find_first(xml_doc, "/OpenGeoSysProject/geometry")

    # Meshes references
    vtu_ref_nodes <- NULL

    if(!any(grepl("xml_missing", class(gml_ref_node), fixed = TRUE))){
        gml_path <- paste0(dirname(prj_path), "/",
                           xml2::xml_text(gml_ref_node))

        # If read_in_gml isn't supplied, check number of lines in .gml file
        # since string concatenation is slow
        if(missing(read_in_gml)){
            read_in_gml <- (length(readLines(gml_path)) <=
                                unlist(options("r2ogs6.max_lines_gml")))
        }

        assertthat::assert_that(assertthat::is.flag(read_in_gml))

        if(read_in_gml){
            ogs6_obj$add_gml(OGS6_gml$new(gml_path))
        }else{
            ogs6_obj$add_gml(gml_path)
        }

        vtu_ref_nodes <- xml2::xml_find_all(xml_doc, "/OpenGeoSysProject/mesh")
    }else{
        vtu_ref_nodes <- xml2::xml_find_all(xml_doc,
                                            "/OpenGeoSysProject/meshes/*")
    }

    for(i in seq_along(vtu_ref_nodes)){
        vtu_ref <- xml2::xml_text(vtu_ref_nodes[[i]])
        vtu_path <- paste0(dirname(prj_path), "/", vtu_ref)

        axisym_val <- xml2::xml_attr(vtu_ref_nodes[[i]], "axially_symmetric")

        if(!is.na(axisym_val) && axisym_val == "true"){
            axisym_val <- TRUE
        }else{
            axisym_val <- FALSE
        }

        # Read in .vtu file(s) or just save their path
        ogs6_obj$add_vtu(path = vtu_path,
                         axisym = axisym_val,
                         read_in_vtu = read_in_vtu)
    }

    prj_components <- ogs6_prj_top_level_classes()

    # Include file reference
    processes_include_node <-
        xml2::xml_find_first(xml_doc,
                             "/OpenGeoSysProject/processes/include")

    if(!any(grepl("xml_missing", class(processes_include_node), fixed = TRUE))){
        file_reference <- xml2::xml_attrs(processes_include_node)[["file"]]

        if(grepl("^\\.\\.", file_reference)){
            file_reference <- gsub("^\\.\\.", "", file_reference)
            file_reference <- paste0(dirname(dirname(prj_path)), file_reference)
        }else{
            file_reference <- paste0(dirname(prj_path), "/", file_reference)
        }

        ogs6_obj$processes <- file_reference
        prj_components <- prj_components[names(prj_components) != "processes"]
    }

    # Check for python script
    python_script_node <-
        xml2::xml_find_first(xml_doc,
                             "/OpenGeoSysProject/python_script")

    if(!any(grepl("xml_missing", class(python_script_node), fixed = TRUE))){
        ogs6_obj$python_script <- xml2::xml_text(python_script_node)
    }

    prj_components <-
        prj_components[names(prj_components) != "python_script"]


    for(i in seq_len(length(prj_components))){

        class_tag_name <- get_tag_from_class(prj_components[[i]])

        # Differentiate between wrapper lists and singular objects
        if(class_tag_name != names(prj_components)[[i]]){
            read_in(ogs6_obj, prj_path, paste0("/OpenGeoSysProject/",
                                               names(prj_components)[[i]],
                                               "/",
                                               class_tag_name))
        }else{
            read_in(ogs6_obj, prj_path, paste0("/OpenGeoSysProject/",
                                               class_tag_name))
        }
    }
}
