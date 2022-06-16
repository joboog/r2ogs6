
#===== export_prj =====


#' export_prj
#' @description
#' Wrapper function to create a \code{.prj} XML document based on the user
#' input data
#' @param ogs6_obj OGS6: Simulation object
#' @noRd
export_prj <- function(ogs6_obj, copy_ext_files = F) {

    prj_xml <- xml2::xml_new_root(
        .value = "OpenGeoSysProject",
        .version = "1.0",
        .encoding = "ISO-8859-1"
    )

    # handle geometry and meshes
    meshes_node <- NULL
    meshes <- ogs6_obj$meshes
    geometry <- ogs6_obj$geometry

    # copy meshes if required
    if(isTRUE(copy_ext_files)){
        for(i in seq(meshes)){
            vtu_path <- meshes[[i]][["path"]]
            file.copy(vtu_path, ogs6_obj$sim_path)
            meshes[[i]][["path"]] <- basename(vtu_path)
        }}

    # handle gml
    if(!is.null(ogs6_obj$gml)){
        gml_path <- paste0(ogs6_obj$sim_path, ogs6_obj$sim_name, ".gml")
        export_gml(ogs6_obj$gml, gml_path)
        geometry <- basename(gml_path)
        xml2::xml_add_child(prj_xml,
                            xml2::as_xml_document(to_node(geometry)))
    }
    else if(!is.null(geometry)){
        if(isTRUE(copy_ext_files)){
            file.copy(geometry, ogs6_obj$sim_path)
            geometry <- basename(geometry)
        }
        xml2::xml_add_child(prj_xml,
                            xml2::as_xml_document(to_node(geometry)))
    }

    # export vtu
    meshes_node <- meshes_to_xml(meshes)
    xml2::xml_add_child(prj_xml,
                        meshes_node)

    #Get implemented classes
    prj_components <- ogs6_prj_top_level_classes()

    # handle includes
    if(names(ogs6_obj$processes)[[1]] == "include"){

        processes <- ogs6_obj$processes
        if(isTRUE(copy_ext_files)){
            file.copy(processes[[1]][["file"]], ogs6_obj$sim_path)
            processes[[1]][["file"]] <- basename(processes[[1]][["file"]])
        }

        processes_node <- to_node(processes, attribute_names = "include")
        xml2::xml_add_child(prj_xml,
                            xml2::as_xml_document(processes_node))
        prj_components <- prj_components[names(prj_components) != "processes"]
    }

    #Add default cases
    for(i in seq_len(length(prj_components))){
        param_name <- names(prj_components)[[i]]

        # cat("\nHandling param", param_name, "\n")

        get_param_call <- paste0("ogs6_obj$", param_name)
        param <- eval(parse(text = get_param_call))

        #If parameter wasn't defined, skip
        if(length(param) == 0){
            next
        }

        # in case copy referenced files
        if(copy_ext_files){
            if(param_name=="chemical_system"){
                file.copy(param$database, ogs6_obj$sim_path)
                param$database <- basename(param$database)
            }
            if(param_name=="python_script"){
                file.copy(param, ogs6_obj$sim_path)
                param <- basename(param)
            }
        }

        # create xml node
        param_node <- to_node(param, param_name)
        xml2::xml_add_child(prj_xml,
                            xml2::as_xml_document(param_node))
    }

    file <- paste0(ogs6_obj$sim_path, ogs6_obj$sim_name, ".prj")

    xml2::write_xml(prj_xml,
                    file,
                    options = "format",
                    encoding="ISO-8859-1")

    return(invisible(TRUE))
}
