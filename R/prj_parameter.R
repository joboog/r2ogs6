
#===== prj_parameter =====


#' prj_parameter
#' @description tag: parameter
#' @param name string:
#' @param type string:
#' @param value Optional: string | double: Parameter value
#' @param values Optional: string | double: Parameter values
#' @param curve Optional: string:
#' @param parameter Optional: string:
#' @param group_id_property Optional: string:
#' @param field_name Optional: string:
#' @param mesh Optional: string:
#' @param time_series Optional: list:
#' @param use_local_coordinate_system Optional: string, "true" | "false":
#' @param range Optional: list
#' @param seed Optional: number
#' @param ... Optional: for index_values and expression tags (since there can be
#' multiple)
#' @example man/examples/ex_prj_parameter.R
#' @export
prj_parameter <- function(name,
                             type,
                             value = NULL,
                             values = NULL,
                             curve = NULL,
                             parameter = NULL,
                             group_id_property = NULL,
                             field_name = NULL,
                             mesh = NULL,
                             time_series = NULL,
                             use_local_coordinate_system = NULL,
                             range = NULL,
                             seed = NULL,
                             ...) {

    #Coerce input
    value <- coerce_string_to_numeric(value)
    values <- coerce_string_to_numeric(values)

    ellipsis_list <- list(...)

    index_values <- ellipsis_list[names(ellipsis_list) == "index_values"]
    expression <- ellipsis_list[names(ellipsis_list) == "expression"]

    range <- coerce_string_to_numeric(range)
    seed <- coerce_string_to_numeric(seed)

    new_prj_parameter(name,
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
                         use_local_coordinate_system,
                         range,
                         seed)
}


new_prj_parameter <- function(name,
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
                                 use_local_coordinate_system = NULL,
                                 range = NULL,
                                 seed = NULL) {

    assertthat::assert_that(assertthat::is.string(name))
    assertthat::assert_that(assertthat::is.string(type))

    are_null_or_numbers(value)
    are_null_or_numeric(values)

    are_null_or_strings(curve,
                               parameter,
                               group_id_property,
                               field_name,
                               mesh)

    lapply(expression, function(x){
        assertthat::assert_that(assertthat::is.string(x))
    })

    are_null_or_string_flags(use_local_coordinate_system)


    for(i in seq_len(length(index_values))){
        index_values[[i]] <- validate_index_values(index_values[[i]])
    }

    if(!is.null(time_series)){
        assertthat::assert_that(is.list(time_series))
        names(time_series) <- rep("pair", length(time_series))

        for(i in seq_len(length(time_series))){
            time_series[[i]] <- coerce_names(time_series[[i]],
                                                    c("time",
                                                      "parameter_name"))
        }
    }
    are_null_or_numeric(range)
    are_null_or_numbers(seed)

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
                   range = range,
                   seed = seed,
                   xpath = "parameters/parameter",
                   attr_names = character(),
                   flatten_on_exp = c("values"),
                   unwrap_on_exp = c("index_values", "expression")
    ),
    class = "prj_parameter"
    )
}


#===== validation utility =====


validate_index_values <- function(index_values){

    # cat("\nindex_values:", class(index_values),"\n")
    # print(index_values)

    if(!is.null(index_values)){
        assertthat::assert_that(is.list(index_values))
        assertthat::assert_that(length(index_values) == 2)

        #Coerce index
        index_values[[1]] <- coerce_string_to_numeric(index_values[[1]])
        names(index_values)[[1]] <- "index"

        #Coerce value / values
        index_values[[2]] <- coerce_string_to_numeric(index_values[[2]])

        if(length(index_values[[2]]) > 1){
            names(index_values)[[2]] <- "values"
        }else{
            names(index_values)[[2]] <- "value"
        }
    }

    return(invisible(index_values))
}
