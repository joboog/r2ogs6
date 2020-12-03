
#===== r2ogs6_medium =====


#'r2ogs6_medium
#'@description tag: medium, a specific medium with optional id corresponding
#' to the MaterialIDs
#'@param phases list, r2ogs6_medium_phase: Medium phases
#'@param properties list, r2ogs6_medium_property: Medium properties
#'@param id double: ID corresponding to the MaterialIDs
#'@export
r2ogs6_medium <- function(phases, properties, id = NULL) {

    #Coerce input
    id <- coerce_string_to_numeric(id)

    new_r2ogs6_medium(phases, properties, id)
}


new_r2ogs6_medium <- function(phases, properties, id = NULL) {

    validate_wrapper_list(phases, "r2ogs6_medium_phase")
    validate_wrapper_list(properties, "r2ogs6_medium_property")
    validate_is_null_or_number(id)

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


#===== r2ogs6_medium_property =====


#'r2ogs6_medium_property
#'@description tag: property, a constitutive property
#'@param name string: Property name
#'@param type string: Property type
#'@param value string | double: ...
#'@param ... ...
#'@export
r2ogs6_medium_property <- function(name, type, value = NULL, ...){

    #Coerce input
    value <- coerce_string_to_numeric(value)

    new_r2ogs6_medium_property(name, type, value, ...)
}


new_r2ogs6_medium_property <- function(name, type, value = NULL, ...){

    assertthat::assert_that(assertthat::is.string(name))
    assertthat::assert_that(assertthat::is.string(type))

    validate_is_null_or_number(value)

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


#===== r2ogs6_medium_phase =====


#'r2ogs6_medium_phase
#'@description tag: phase, a coherent material with homogeneous properties
#'@param type string: Medium type (one of "Gas", "Solid", "AqueousLiquid"
#' and "NonAqueousLiquid")
#'@param properties list, r2ogs6_medium_property: Properties
#'@param components list, components (WIP)
#'@export
r2ogs6_medium_phase <- function(type, properties, components = NULL){

    #Make this more user friendly
    #...

    new_r2ogs6_medium_phase(type, properties, components)
}


new_r2ogs6_medium_phase <- function(type, properties, components = NULL) {

    assertthat::assert_that(assertthat::is.string(type))
    assertthat::assert_that(type %in% prj_medium_phase_types)

    if(!is.null(components)){
        #...
    }

    validate_wrapper_list(properties, "r2ogs6_medium_property")

    structure(
        list(
            type = type,
            properties = properties,
            components = components,
            tag_name = "phase",
            is_subclass = TRUE,
            attr_names = character(),
            flatten_on_exp = character()
        ),
        class = "r2ogs6_medium_phase"
    )
}
