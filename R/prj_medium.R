

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
            id = id,
            tag_name = "medium",
            is_subclass = FALSE,
            attr_names = c("id"),
            flatten_on_exp = character()
        ),
        class = "r2ogs6_medium"
    )
}


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
            value = value,
            tag_name = "property",
            is_subclass = TRUE,
            attr_names = character(),
            flatten_on_exp = character()
        ),
        class = "r2ogs6_medium_property"
    )
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
            properties = properties,
            tag_name = "phase",
            is_subclass = TRUE,
            attr_names = character(),
            flatten_on_exp = character()
        ),
        class = "r2ogs6_medium_phase"
    )
}
