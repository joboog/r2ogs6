
#============================== MEDIA CLASSES AND METHODS ================================

#============================== MEDIUM ================================

#'r2ogs6_medium_property
#'@description S3 class describing a .prj medium property (some rare parameters are NOT implemented yet!)
#'@param name A string specifying the property name
#'@param type A string specifying the property type
#'@param value A string ...
#'@param ... ...
#'@export
r2ogs6_medium_property <- function(name, type, value = NULL, ...){

    #Make this more user friendly
    #...

    new_r2ogs6_medium_property(name, type, value, ...)
}


#'new_r2ogs6_medium_property
#'@description Constructor for S3 class r2ogs6_medium_property
new_r2ogs6_medium_property <- function(name, type, value = NULL, ...){

    assertthat::assert_that(is.string(name))
    assertthat::assert_that(is.string(type))

    if(!is.null(value)){
        assertthat::assert_that(is.numeric(value), length(value) == 1)
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
#'@param obj A r2ogs6_medium_property class object
as_node.r2ogs6_medium_property <- function(obj) {
    medium_property_node <- list(property = structure(list()))

    medium_property_node <- add_children(medium_property_node, list(name = obj$name,
                                                                    type = obj$type,
                                                                    value = obj$value))

    return(medium_property_node)
}


#'r2ogs6_medium_phase
#'@description S3 class describing a .prj medium phase
#'@param type A string specifying the medium type (valid types: ...)
#'@param properties A list of properties (see ?r2ogs6_medium_property for more info)
#'@export
r2ogs6_medium_phase <- function(type, properties){

    #Make this more user friendly
    #...

    new_r2ogs6_medium_phase(type, properties)
}


#'new_r2ogs6_medium_phase
#'@description Constructor for S3 class r2ogs6_medium_phase
new_r2ogs6_medium_phase <- function(type, properties) {

    assertthat::assert_that(is.string(type))

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
#'@param obj A r2ogs6_medium_phase class object
as_node.r2ogs6_medium_phase <- function(obj) {
    medium_phase_node <- list(property = structure(list()))

    medium_phase_node <- add_children(medium_property_node, list(type = obj$type,
                                                                 properties = obj$properties))

    return(medium_phase_node)
}


#'new_r2ogs6_medium
#'@description
#'@param phases A list of medium phases (r2ogs6_medium_phase objects)
#'@param properties A list of medium properties (r2ogs6_medium_property objects)
#'@param id Optional: The medium ID
#'@export
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
#'@param obj A r2ogs6_medium class object
as_node.r2ogs6_medium <- function(obj) {
    medium_node <- list(medium = structure(list()))

    medium_node <- add_attr(medium_node, obj$id, "id")

    medium_node <- add_children(medium_node, list(phases = obj$phases,
                                                  properties = obj$properties))

    return(medium_node)
}


#'input_add.r2ogs6_medium
#'@description Implementation of generic function input_add for S3 class r2ogs6_medium
#'@param obj A r2ogs6_medium class object
#'@param ogs6_obj An OGS6 class object
#'@export
input_add.r2ogs6_medium <- function(obj, ogs6_obj) {
    ogs6_obj$add_medium(obj)
}