
#===== r2ogs6_porous_medium =====


#'r2ogs6_porous_medium
#'@description tag: porous_medium
#'@param id string | double:
#'@param permeability list:
#'@param porosity list:
#'@param storage list:
#'@param capillary_pressure r2ogs6_capillary_pressure:
#'@param relative_permeability list:
#'@export
r2ogs6_porous_medium <- function(id,
                                 permeability,
                                 porosity,
                                 storage,
                                 capillary_pressure,
                                 relative_permeability) {

    # COerce input
    id <- coerce_string_to_numeric(id)

    new_r2ogs6_porous_medium(id,
                             permeability,
                             porosity,
                             storage,
                             capillary_pressure,
                             relative_permeability)
}


new_r2ogs6_porous_medium <- function(id,
                                     permeability,
                                     porosity,
                                     storage,
                                     capillary_pressure,
                                     relative_permeability) {

    are_numbers(id)

    permeability <- coerce_names(permeability,
                                        c("permeability_tensor_entries",
                                          "type"))

    porosity <- coerce_names(porosity,
                                    c("porosity_parameter",
                                      "type"))

    storage <- coerce_names(storage,
                                   c("value",
                                     "type"))

    assertthat::assert_that(class(capillary_pressure) ==
                                "r2ogs6_capillary_pressure")

    if(!"r2ogs6_relative_permeability" %in%
       class(relative_permeability)){
        is_wrapper_list(relative_permeability,
                              "r2ogs6_relative_permeability")
    }

    structure(list(id = id,
                   permeability = permeability,
                   porosity = porosity,
                   storage = storage,
                   capillary_pressure = capillary_pressure,
                   relative_permeability = relative_permeability,
                   xpath = c(paste0("processes/process/material_property/",
                                  "porous_medium/porous_medium"),
                             "processes/process/porous_medium/porous_medium"),
                   attr_names = c("id"),
                   flatten_on_exp = character()
    ),
    class = "r2ogs6_porous_medium"
    )
}


#===== r2ogs6_capillary_pressure =====


#'r2ogs6_capillary_pressure
#'@description tag: capillary_pressure
#'@param type string:
#'@param pd Optional: string | double:
#'@param sr Optional: string | double:
#'@param smax Optional: string | double:
#'@param m Optional: string | double:
#'@param pc_max Optional: string | double:
#'@param has_regularized Optional: string, "true" | "false":
#'@param curve Optional: list:
#'@export
r2ogs6_capillary_pressure <- function(type,
                                      pd = NULL,
                                      sr = NULL,
                                      smax = NULL,
                                      m = NULL,
                                      pc_max = NULL,
                                      has_regularized = NULL,
                                      curve = NULL) {

    # Coerce input
    pd <- coerce_string_to_numeric(pd)
    sr <- coerce_string_to_numeric(sr)
    smax <- coerce_string_to_numeric(smax)
    m <- coerce_string_to_numeric(m)
    pc_max <- coerce_string_to_numeric(pc_max)

    new_r2ogs6_capillary_pressure(type,
                                  pd,
                                  sr,
                                  smax,
                                  m,
                                  pc_max,
                                  has_regularized,
                                  curve)
}


new_r2ogs6_capillary_pressure <- function(type,
                                          pd = NULL,
                                          sr = NULL,
                                          smax = NULL,
                                          m = NULL,
                                          pc_max = NULL,
                                          has_regularized = NULL,
                                          curve = NULL) {

    are_strings(type)

    are_null_or_numbers(pd,
                               sr,
                               smax,
                               m,
                               pc_max)

    if(!is.null(has_regularized)){
        has_regularized <- stringr::str_remove_all(has_regularized,
                                                   "[:space:]*")
    }

    are_null_or_string_flags(has_regularized)

    is_null_or_coerce_names(curve, c("coords", "values"))

    if(!is.null(curve)){
        curve[[1]] <- coerce_string_to_numeric(curve[[1]])
        curve[[2]] <- coerce_string_to_numeric(curve[[2]])

        assertthat::assert_that(is.numeric(curve[[1]]))
        assertthat::assert_that(is.numeric(curve[[2]]))
    }

    structure(list(type = type,
                   pd = pd,
                   sr = sr,
                   smax = smax,
                   m = m,
                   pc_max = pc_max,
                   has_regularized = has_regularized,
                   curve = curve,
                   xpath = c(paste0("processes/process/material_property/",
                                  "porous_medium/porous_medium/",
                                  "capillary_pressure"),
                             paste0("processes/process/",
                                    "porous_medium/porous_medium/",
                                    "capillary_pressure")),
                   attr_names = character(),
                   flatten_on_exp = character()
    ),
    class = "r2ogs6_capillary_pressure"
    )
}


#===== r2ogs6_relative_permeability =====


#'r2ogs6_relative_permeability
#'@description tag: relative_permeability
#'@param type string:
#'@param sr string | number:
#'@param smax string | number:
#'@param m string | number:
#'@param krel_min string | number:
#'@param id string: Optional:
r2ogs6_relative_permeability <- function(type,
                                         sr,
                                         smax,
                                         m,
                                         krel_min,
                                         id = NULL) {

    # Add coercing utility here

    sr <- coerce_string_to_numeric(sr)
    smax <- coerce_string_to_numeric(smax)
    m <- coerce_string_to_numeric(m)
    krel_min <- coerce_string_to_numeric(krel_min)

    new_r2ogs6_relative_permeability(type,
                                     sr,
                                     smax,
                                     m,
                                     krel_min,
                                     id)
}


new_r2ogs6_relative_permeability <- function(type,
                                             sr,
                                             smax,
                                             m,
                                             krel_min,
                                             id = NULL) {
    are_strings(type)
    are_null_or_strings(id)

    are_numbers(sr,
                       smax,
                       m,
                       krel_min)

    structure(list(type = type,
                   sr = sr,
                   smax = smax,
                   m = m,
                   krel_min = krel_min,
                   id = id,
                   xpath =
                       c(paste0("processes/process/material_property/",
                                "porous_medium/porous_medium/",
                                "relative_permeability/relative_permeability"),
                         paste0("processes/process/",
                                "porous_medium/porous_medium/",
                                "relative_permeability")),
                   attr_names = c("id"),
                   flatten_on_exp = character()
    ),
    class = "r2ogs6_relative_permeability"
    )
}
