
#===== r2ogs6_parameter =====


#'r2ogs6_parameter
#'@description tag: parameter
#'@param name string:
#'@param type string:
#'@param value Optional: string | double: Parameter value
#'@param values Optional: string | double: Parameter values
#'@param curve Optional: string:
#'@param parameter Optional: string:
#'@param group_id_property Optional: string:
#'@param index_values Optional: list:
#'@param field_name Optional: string:
#'@param mesh Optional: string:
#'@param expression Optional: string:
#'@param time_series Optional: list:
#'@param use_local_coordinate_system Optional: string, "true" | "false":
#'@export
r2ogs6_parameter <- function(name,
                             type,
                             value = NULL,
                             values = NULL,
                             curve = NULL,
                             parameter = NULL,
                             group_id_property = NULL,
                             index_values = NULL,
                             field_name = NULL,
                             mesh = NULL,
                             expression = NULL,
                             time_series = NULL,
                             use_local_coordinate_system = NULL) {

    #Coerce input
    value <- coerce_string_to_numeric(value)
    values <- coerce_string_to_numeric(values, TRUE)

    new_r2ogs6_parameter(name,
                         type,
                         value,
                         values,
                         curve,
                         parameter,
                         group_id_property,
                         index_values,
                         field_name,
                         mesh,
                         expression,
                         time_series,
                         use_local_coordinate_system)
}


new_r2ogs6_parameter <- function(name,
                                 type,
                                 value = NULL,
                                 values = NULL,
                                 curve = NULL,
                                 parameter = NULL,
                                 group_id_property = NULL,
                                 index_values = NULL,
                                 field_name = NULL,
                                 mesh = NULL,
                                 expression = NULL,
                                 time_series = NULL,
                                 use_local_coordinate_system = NULL) {

    assertthat::assert_that(assertthat::is.string(name))
    assertthat::assert_that(assertthat::is.string(type))

    validate_is_null_or_number(value)
    validate_is_null_or_numeric(values)

    validate_is_null_or_string(curve,
                               parameter,
                               group_id_property,
                               field_name,
                               mesh,
                               expression)

    validate_is_null_or_str_flag(use_local_coordinate_system)

    index_values <- validate_index_values(index_values)

    if(!is.null(time_series)){
        assertthat::assert_that(is.list(time_series))
        names(time_series) <- rep("pair", length(time_series))

        for(i in seq_len(length(time_series))){
            time_series[[i]] <- validate_param_list(time_series[[i]],
                                                    c("time",
                                                      "parameter_name"))
        }
    }

    structure(list(name = name,
                   type = type,
                   value = value,
                   values = values,
                   curve = curve,
                   parameter = parameter,
                   group_id_property = group_id_property,
                   index_values = index_values,
                   field_name = field_name,
                   mesh = mesh,
                   expression = expression,
                   time_series = time_series,
                   use_local_coordinate_system = use_local_coordinate_system,
                   is_subclass = TRUE,
                   attr_names = character(),
                   flatten_on_exp = c("values")
    ),
    class = "r2ogs6_parameter"
    )
}


#===== validation utility =====


validate_index_values <- function(index_values){

    if(!is.null(index_values)){
        assertthat::assert_that(is.list(index_values))
        assertthat::assert_that(length(index_values == 2))

        #Coerce index
        index_values[[1]] <- coerce_string_to_numeric(index_values[[1]])
        names(index_values)[[1]] <- "index"

        #Coerce value / values
        index_values[[2]] <- coerce_string_to_numeric(index_values[[2]],
                                                      TRUE)

        if(length(index_values[[2]]) > 1){
            names(index_values)[[2]] <- "values"
        }else{
            names(index_values)[[2]] <- "value"
        }
    }

    return(invisible(index_values))
}
