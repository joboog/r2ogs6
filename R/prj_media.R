
#============================== MEDIA CLASSES AND METHODS ================================

#============================== MEDIUM ================================

#'r2ogs6_medium_property
#'@description S3 class describing a .prj medium property (a constitutive property) (WIP!)
#'@param name A string specifying the property name
#'@param type A string specifying the property type
#'@param value A string ...
#'@param ... ...
#'@export
r2ogs6_medium_property <- function(name, type, value = NULL, ...){

    #Coerce input
    if(!is.null(value)){
        if(assertthat::is.string(value)){
            value <- as.double(value)
        }
    }

    new_r2ogs6_medium_property(name, type, value, ...)
}


new_r2ogs6_medium_property <- function(name, type, value = NULL, ...){

    assertthat::assert_that(assertthat::is.string(name))
    assertthat::assert_that(assertthat::is.string(type))

    if(!is.null(value)){
        assertthat::assert_that(assertthat::is.number(value))
    }

    structure(
        list(
            name = name,
            type = type,
            value = value
        ),
        class = "r2ogs6_medium_property"
    )
}


#'as_node.r2ogs6_medium_property
#'@description Implementation of generic function as_node for S3 class r2ogs6_medium_property
#'@param x A r2ogs6_medium_property class object
as_node.r2ogs6_medium_property <- function(x) {
    node <- list(property = structure(list()))

    node <- add_children(node, list(name = x$name,
                                    type = x$type,
                                    value = x$value))

    return(invisible(node))
}


#'r2ogs6_medium_phase
#'@description S3 class describing a .prj medium phase (a coherent material with homogeneous properties)
#'@param type A string specifying the medium type (one of "Gas", "Solid", "AqueousLiquid" and "NonAqueousLiquid")
#'@param properties A list of properties (see ?r2ogs6_medium_property for more info)
#'@export
r2ogs6_medium_phase <- function(type, properties){

    #Make this more user friendly
    #...

    new_r2ogs6_medium_phase(type, properties)
}


new_r2ogs6_medium_phase <- function(type, properties) {

    assertthat::assert_that(assertthat::is.string(type))
    assertthat::assert_that(type %in% prj_medium_phase_types)
    validate_wrapper_list(properties, "r2ogs6_medium_property")

    structure(
        list(
            type = type,
            properties = properties
        ),
        class = "r2ogs6_medium_phase"
    )
}


#'as_node.r2ogs6_medium_phase
#'@description Implementation of generic function as_node for S3 class r2ogs6_medium_phase
#'@param x A r2ogs6_medium_phase class object
as_node.r2ogs6_medium_phase <- function(x) {
    node <- list(phase = structure(list()))

    properties_node <- adopt_nodes("properties", x$properties)

    node <- add_children(node, list(type = x$type,
                                    properties_node))

    return(invisible(node))
}


#'r2ogs6_medium
#'@description A specific medium with optional id corresponding to the MaterialIDs
#'@param phases A list of medium phases (r2ogs6_medium_phase objects)
#'@param properties A list of medium properties (r2ogs6_medium_property objects)
#'@param id Optional: ID corresponding to the MaterialIDs
#'@export
r2ogs6_medium <- function(phases, properties, id = NULL) {

    #Coerce input
    if(!is.null(id)){
        if(assertthat::is.string(id)){
            id <- as.double(id)
        }
    }

    new_r2ogs6_medium(phases, properties, id)
}


new_r2ogs6_medium <- function(phases, properties, id = NULL) {

    validate_wrapper_list(phases, "r2ogs6_medium_phase")
    validate_wrapper_list(properties, "r2ogs6_medium_property")

    if(!is.null(id)){
        assertthat::assert_that(assertthat::is.number(id))
    }

    structure(
        list(
            phases = phases,
            properties = properties,
            id = id
        ),
        class = "r2ogs6_medium"
    )
}


#'as_node.r2ogs6_medium
#'@description Implementation of generic function as_node for S3 class r2ogs6_medium
#'@param x A r2ogs6_medium class object
as_node.r2ogs6_medium <- function(x) {
    node <- list(medium = structure(list()))

    node <- add_attr(node, x$id, "id")

    phases_node <- adopt_nodes("phases", x$phases)
    properties_node <- adopt_nodes("properties", x$properties)

    node <- add_children(node, list(phases_node,
                                    properties_node))

    return(invisible(node))
}


#'input_add.r2ogs6_medium
#'@description Implementation of generic function input_add for S3 class r2ogs6_medium
#'@param x A r2ogs6_medium class object
#'@param ogs6_obj An OGS6 class object
#'@export
input_add.r2ogs6_medium <- function(x, ogs6_obj) {
    ogs6_obj$add_medium(x)
}