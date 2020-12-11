#This script contains functions to export the .prj data

#'export_prj
#'@description Wrapper function to create a .prj XML document based on the user
#' input data
#'@param ogs6_obj OGS6: Simulation object
export_prj <- function(ogs6_obj) {

    meshes_node <- NULL

    #If there is a .gml defined, add "mesh" node, else add "meshes" node
    if(is.null(ogs6_obj$geometry)) {
        meshes_node <- to_node(ogs6_obj$meshes, "meshes")
    }else{
        meshes_node <- to_node(ogs6_obj$meshes[[1]]$mesh_ref, "mesh")
    }

    # First instantiate our big wrapper list
    prj_node <- list(OpenGeoSysProject = list())

    special_cases <- c("meshes",
                       "gml")

    #Handle special cases
    prj_node[[1]] <- c(prj_node[[1]], list(meshes_node))

    if(!is.null(ogs6_obj$geometry)){
        prj_node[[1]] <- c(prj_node[[1]], list(to_node(ogs6_obj$geometry)))
    }

    #Get implemented classes
    impl_classes <- get_implemented_classes()

    #Add default cases
    for(i in seq_len(length(impl_classes))){
        param_name <- names(impl_classes)[[i]]

        #If parameter was a special case we already handled, skip
        if(param_name %in% special_cases){
            next
        }

        get_param_call <- paste0("ogs6_obj$", param_name)
        param <- eval(parse(text = get_param_call))

        #If parameter wasn't defined, skip
        if(length(param) == 0){
            next
        }

        to_node_call <- paste0("to_node(ogs6_obj$", param_name, ")")
        param_node <- eval(parse(text = to_node_call))
        prj_node[[1]] <- c(prj_node[[1]], list(param_node))
    }

    file <- paste0(ogs6_obj$sim_path, ogs6_obj$sim_name, ".prj")

    prj_xml <- xml2::as_xml_document(prj_node)

    xml2::write_xml(prj_xml, file, options = "format", encoding="ISO-8859-1")

    return(invisible())
}
