
#===== read_in_prj =====

#'read_in_prj
#'@description Wrapper function to read in a whole .prj file
#'@param ogs6_obj A OGS6 class object
#'@param prj_path The path to the project file that should be read in
#'@param read_in_vtu flag: Should .vtu file be copied or read in?
#'@export
read_in_prj <- function(ogs6_obj,
                        prj_path,
                        read_in_vtu = FALSE){

    assertthat::assert_that("OGS6" %in% class(ogs6_obj))
    xml_doc <- validate_read_in_xml(prj_path)

    # Geometry reference
    gml_ref_node <- xml2::xml_find_first(xml_doc, "/OpenGeoSysProject/geometry")

    # Meshes references
    vtu_ref_nodes <- NULL

    if(!any(grepl("xml_missing", class(gml_ref_node), fixed = TRUE))){
        gml_path <- paste0(dirname(prj_path), "/",
                           xml2::xml_text(gml_ref_node))
        read_in_gml(ogs6_obj, gml_path)
        vtu_ref_nodes <- xml2::xml_find_all(xml_doc, "/OpenGeoSysProject/mesh")
    }else{
        vtu_ref_nodes <- xml2::xml_find_all(xml_doc,
                                            "/OpenGeoSysProject/meshes/*")
    }

    for(i in seq_along(vtu_ref_nodes)){
        vtu_ref <- xml2::xml_text(vtu_ref_nodes[[i]])
        vtu_path <- paste0(dirname(prj_path), "/", vtu_ref)


        # Read in .vtu file(s) or just save their path
        if(read_in_vtu){
            read_in_vtu(ogs6_obj, vtu_path)
        }else{
            ogs6_obj$add_mesh(vtu_path)
        }
    }

    impl_classes <- get_implemented_classes()

    for(i in seq_len(length(impl_classes))){

        class_tag_name <- get_class_tag_name(impl_classes[[i]])

        # Differentiate between wrapper lists and singular objects
        if(class_tag_name != names(impl_classes)[[i]]){
            read_in(ogs6_obj, prj_path, paste0("/OpenGeoSysProject/",
                                               names(impl_classes)[[i]],
                                               "/",
                                               class_tag_name))
        }else{
            read_in(ogs6_obj, prj_path, paste0("/OpenGeoSysProject/",
                                               class_tag_name))
        }
    }
}
