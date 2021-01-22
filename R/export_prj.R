#This script contains functions to export the .prj data

#'export_prj
#'@description Wrapper function to create a .prj XML document based on the user
#' input data
#'@param ogs6_obj OGS6: Simulation object
export_prj <- function(ogs6_obj) {

    prj_xml <- xml2::xml_new_root(
        .value = "OpenGeoSysProject",
        .version = "1.0",
        .encoding = "ISO-8859-1"
    )

    meshes_node <- NULL

    #If there is a .gml defined, add "mesh" node, else add "meshes" node
    if(is.null(ogs6_obj$geometry)) {
        basenames <- lapply(ogs6_obj$meshes, function(x){basename(x)})
        meshes_node <- to_node(basenames, "meshes")
    }else{
        xml2::xml_add_child(
            prj_xml,
            xml2::as_xml_document(to_node(ogs6_obj$geometry)))
        meshes_node <- to_node(basename(ogs6_obj$meshes[[1]]), "mesh")
    }

    xml2::xml_add_child(prj_xml,
                        xml2::as_xml_document(meshes_node))

    #Get implemented classes
    prj_components <- addable_prj_components()

    # Include file reference
    if(names(ogs6_obj$processes)[[1]] == "include"){
        processes_node <- to_node(ogs6_obj$processes,
                                  attribute_names = "include")

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

        to_node_call <- paste0("to_node(ogs6_obj$", param_name, ")")
        param_node <- eval(parse(text = to_node_call))

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
