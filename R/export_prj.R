#This script contains various functions to turn data for a .prj file into the correct XML format

#' Wrapper function to create a .prj XML document based on the user input data
#' @param prj_obj .
prj_data_to_xml <- function(prj_obj) {

    data_node <- xml2::xml_new_root(.value = "OpenGeoSysProject")

    xml2::xml_add_child(data_node, adopt_nodes("processes", prj_obj$processes))
    xml2::xml_add_child(data_node, adopt_nodes("media", prj_obj$media))
    xml2::xml_add_child(data_node, adopt_nodes("time_loop", prj_obj$time_loop))
    xml2::xml_add_child(data_node, adopt_nodes("parameters", prj_obj$parameters))
    xml2::xml_add_child(data_node, adopt_nodes("process_variables", prj_obj$process_variables))
    xml2::xml_add_child(data_node, adopt_nodes("nonlinear_solvers", prj_obj$nonlinear_solvers))
    xml2::xml_add_child(data_node, adopt_nodes("linear_solvers", prj_obj$linear_solvers))

    return(data_node)
}

#============================== processes ================================

#Implementation for class r2ogs6_process
as_node.r2ogs6_prj_process <- function(obj){

    process_node <- list(process = structure(list(name = obj$name,
                                                  type = obj$type,
                                                  integration_order = obj$integration_order,
                                                  dimension = obj$dimension,
                                                  constitutive_relation = list(type = obj$cr_type,
                                                                               youngs_modulus = obj$cr_youngs_modulus,
                                                                               poissons_ratio = obj$cr_poissons_ratio),
                                                  process_variables = list(displacement = obj$pv_displacement,
                                                                           pressure = obj$pv_pressure),
                                                  secondary_variables = list(),
                                                  specific_body_force = obj$specific_body_force)))
    return(process_node)
}


#'Method for coercing an r2ogs6_prj_property class object into the structure expected by xml2
as_node.r2ogs6_prj_property <- function(obj) {
    return(list(property = list(name = obj$name, type = obj$type)))
}


#============================== media ================================


#'Method for coercing an r2ogs_prj_medium_phase class object into the structure expected by xml2
as_node.r2ogs6_prj_medium_phase <- function(obj) {
    medium_phase_node <- list(phase = list(type = obj$type),
                              adopt_nodes("properties", obj$properties))
    return(medium_phase_node)
}


as_node.r2ogs6_prj_medium <- function(obj) {

    medium_node <- list(medium = list(phases = list(), properties = list()))

    for(i in 1:length(obj$phases)){
        medium_node[[1]] <- c(medium_node[[1]][[1]], as_node(obj$phases[[i]]))
    }

    for(i in 1:length(obj$properties)){
        medium_node[[1]] <- c(medium_node[[1]][[2]], as_node(obj$properties[[i]]))
    }

    return(medium_node)
}


#============================== time_loop ================================


#============================== parameters ================================


#============================== process_variables ================================


#============================== nonlinear_solvers ================================


#============================== linear_solvers ================================