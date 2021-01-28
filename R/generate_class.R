

#===== S3 class generation =====

#===== generate_constructor =====


#'generate_constructor
#'@description Helper function to generate a constructor out of a tag name
#' and a flag vector
#'@param params list: (Return value of analyse_xml())
#'@param prefix Optional: For subclasses whose represented elements have
#' the same tag name as an element for which a class was already specified,
#' a prefix must be appended to the class name
#'@param print_result flag: Should the result be printed to the console?
generate_constructor <- function(params,
                                 prefix = "",
                                 print_result = FALSE){

    assertthat::assert_that(is.list(params))
    assertthat::assert_that(length(params) == 4)
    assertthat::assert_that(assertthat::is.string(prefix))

    xpath <- stringr::str_remove(params[[1]], "\\/[A-Za-z_]*\\/")
    tag_name <- get_tag_from_xpath(xpath)

    attr_flags <- params[[3]]
    param_flags <- params[[4]]

    class_name <- paste0("r2ogs6_", prefix, tag_name)

    param_str <- flags_to_con_str(param_flags)
    assign_str <- flags_to_assign_str(param_flags)

    con_str <- paste0("new_", class_name, " <- function(", param_str, ") {\n")

    attr_names <- ""

    if(length(attr_flags) > 0){
        attr_names <- paste0("\"",
                             paste(names(attr_flags), collapse = "\", \""),
                             "\"")
    }

    #Add validation utility here

    con_str <- paste0(con_str,
                      "structure(list(",
                      assign_str, ",\n",
                      "xpath = \"", xpath, "\",\n",
                      "attr_names = c(", attr_names , "),\n",
                      "flatten_on_exp = character()\n",
                      "),\n",
                      "class = \"", class_name, "\"\n",
                      ")\n",
                      "}\n")

    if(print_result){
        cat(con_str, "\n")
    }

    return(invisible(con_str))
}


#===== generate_helper =====


#'generate_helper
#'@description Helper function to generate a helper out of a tag name
#' and a flag vector
#'@param params list: (Return value of analyse_xml())
#'@param prefix Optional: For subclasses whose represented elements have
#' the same tag name as an element for which a class was already specified,
#' a prefix must be appended to the class name
#'@param print_result flag: Should the result be printed to the console?
generate_helper <- function(params,
                            prefix = "",
                            print_result = FALSE){

    assertthat::assert_that(is.list(params))
    assertthat::assert_that(length(params) == 4)
    assertthat::assert_that(assertthat::is.string(prefix))

    xpath <- stringr::str_remove(params[[1]], "\\/[A-Za-z_]*\\/")
    tag_name <- get_tag_from_xpath(xpath)

    param_flags <- params[[4]]

    class_name <- paste0("r2ogs6_", prefix, tag_name)

    doc_str <- flags_to_doc_str(param_flags)
    con_str <- flags_to_con_str(param_flags)
    con_call_str <- flags_to_con_call_str(param_flags)

    helper_str <- paste0("#'", class_name, "\n",
                         "#'@description tag: ", tag_name, "\n",
                         doc_str,
                         "\n",
                         class_name, " <- function(", con_str, ") {\n",
                         "\n# Add coercing utility here\n\n",
                         "new_", class_name, "(", con_call_str, ")\n",
                         "}\n")

    if(print_result){
        cat(helper_str, "\n")
    }

    return(invisible(helper_str))
}


#===== get parameter lists (for use in class constructor / helper) =====


#'flags_to_con_str
#'@description Helper function to generate a string out of a flag vector
#'@param flags vector: Flags
#'@param print_result flag: Should the result be printed to the console?
flags_to_con_str <- function(flags, print_result = FALSE) {

    assertthat::assert_that(is.logical(flags))

    flag_strs <- character()

    for(i in seq_len(length(flags))){
        if(flags[[i]]){
            flag_strs <- c(flag_strs, names(flags)[[i]])
        }else{
            flag_strs <- c(flag_strs, paste(names(flags)[[i]], "= NULL"))
        }
    }

    flag_str <- paste(flag_strs, collapse = ",\n")

    if(print_result){
        cat(flag_str, "\n")
    }

    return(invisible(flag_str))
}


#'flags_to_con_call_str
#'@description Helper function to generate a string out of a flag vector
#'@param flags vector: Flags
#'@param print_result flag: Should the result be printed to the console?
flags_to_con_call_str <- function(flags, print_result = FALSE) {

    assertthat::assert_that(is.logical(flags))

    flag_str <- paste(names(flags), collapse = ",\n")

    if(print_result){
        cat(flag_str, "\n")
    }

    return(invisible(flag_str))
}


#'flags_to_name_str
#'@description Helper function to generate a name string out of a flag vector
#'@param flags vector: Flags
#'@param print_result flag: Should the result be printed to the console?
flags_to_name_str <- function(flags, print_result = FALSE) {

    assertthat::assert_that(is.logical(flags))

    flag_str <- paste0("\"", paste(names(flags), collapse = "\",\n\""), "\"")

    if(print_result){
        cat(flag_str, "\n")
    }

    return(invisible(flag_str))
}


#===== get class parameter list (for use in class constructor) =====


#'flags_to_assign_str
#'@description Helper function to generate a string out of a flag vector
#'@param flags vector: Flags
#'@param print_result flag: Should the result be printed to the console?
flags_to_assign_str <- function(flags, print_result = FALSE){

    assertthat::assert_that(is.logical(flags))

    flag_str <- paste(names(flags),
                      names(flags),
                      sep = " = ",
                      collapse = ",\n")

    if(print_result){
        cat(flag_str, "\n")
    }

    return(invisible(flag_str))
}


#===== get class parameter doc (for use in class helper) =====


#'flags_to_doc_str
#'@description Helper function to generate a string out of a flag vector
#'@param flags vector: Flags
#'@param print_result flag: Should the result be printed to the console?
flags_to_doc_str <- function(flags, print_result = FALSE){

    assertthat::assert_that(is.logical(flags))

    flag_strs <- character()

    for(i in seq_len(length(flags))){
        if(flags[[i]]){
            flag_strs <- c(flag_strs, paste("#'@param",
                                            names(flags)[[i]]))
        }else{
            flag_strs <- c(flag_strs, paste("#'@param",
                                            names(flags)[[i]],
                                            "Optional: "))
        }
    }

    flag_str <- paste(flag_strs, collapse = "\n")

    if(print_result){
        cat(flag_str, "\n")
    }

    return(invisible(flag_str))
}



#===== R6 class generation =====

#===== generate_R6 =====


#'generate_R6
#'@description Helper function to generate a R6 class out of a tag name
#' and a flag vector
#'@param params list: (Return value of analyse_xml())
#'@param prefix Optional: For subclasses whose represented elements have
#' the same tag name as an element for which a class was already specified,
#' a prefix must be appended to the class name
#'@param print_result flag: Should the result be printed to the console?
generate_R6 <- function(params,
                        prefix = "",
                        print_result = TRUE){

    assertthat::assert_that(is.list(params))
    assertthat::assert_that(length(params) == 4)

    xpath <- stringr::str_remove(params[[1]], "\\/[A-Za-z_]*\\/")
    tag_name <- get_tag_from_xpath(xpath)

    attr_flags <- params[[3]]
    param_flags <- params[[4]]

    default_af_str <- paste0("#'@field is_subclass\n",
                             "#'Access to private parameter ",
                             "'.is_subclass'\n",
                             "is_subclass = function() {\n",
                             "private$.is_subclass\n},\n\n",
                             "#'@field subclasses_names\n",
                             "#'Access to private parameter ",
                             "'.subclasses_names'\n",
                             "subclasses_names = function() {\n",
                             "private$.subclasses_names\n},\n\n",
                             "#'@field attr_names\n",
                             "#'Access to private parameter '.attr_names'\n",
                             "attr_names = function() {\n",
                             "private$.attr_names\n}")

    attr_names <- ""

    if(length(attr_flags) > 0){
        attr_names <- paste0("\"",
                             paste(names(attr_flags), collapse = "\", \""),
                             "\"")
    }

    r6_str <- paste0("OGS6_", tag_name, " <- R6::R6Class(\"OGS6_",
                     tag_name, "\",\n",
                     "public = list(\n",
                     "#'@description\n",
                     "#'Creates new OGS6_", tag_name, "object\n",
                     flags_to_doc_str(param_flags),
                     "initialize = function(",
                     flags_to_con_str(param_flags),
                     "){\n",
                     flags_to_r6_init_str(param_flags),
                     "\n}\n),\n\n",
                     "active = list(\n",
                     flags_to_r6_active_field_str(param_flags), ",\n\n",
                     default_af_str, "\n",
                     "),\n\n",
                     "private = list(\n",
                     flags_to_r6_private_str(param_flags), ",\n",
                     ".is_subclass = TRUE,\n",
                     ".subclasses_names = character(),\n",
                     ".attr_names = c(", attr_names , "),\n",
                     ")\n)")


    if(print_result){
        cat(r6_str, "\n")
    }

    return(invisible(r6_str))
}


#'flags_to_r6_init_str
#'@description Helper function to generate a string out of a flag vector
#'@param flags vector: Flags
#'@param print_result flag: Should the result be printed to the console?
flags_to_r6_init_str <- function(flags, print_result = FALSE){

    assertthat::assert_that(is.logical(flags))
    assertthat::assert_that(assertthat::is.flag(print_result))

    init_strs <- c()

    for(i in seq_len(length(flags))){

        param_name <- names(flags)[[i]]

        init_str <- paste0("self$", param_name, " <- ", param_name)

        init_strs <- c(init_strs, c(init_str))
    }

    init_str <- paste(init_strs, collapse = "\n")

    if(print_result){
        cat(init_str, "\n")
    }

    return(invisible(init_str))
}


#'flags_to_r6_active_field_str
#'@description Helper function to generate a string out of a flag vector
#'@param flags vector: Flags
#'@param mutable flag: On per default, turn off if parameters are static
#'@param print_result flag: Should the result be printed to the console?
flags_to_r6_active_field_str <- function(flags,
                                         mutable = TRUE,
                                         print_result = FALSE){

    assertthat::assert_that(is.logical(flags))
    assertthat::assert_that(assertthat::is.flag(print_result))
    assertthat::assert_that(assertthat::is.flag(mutable))

    af_strs <- c()

    for(i in seq_len(length(flags))){

        af_str <- paste0("#'@field ", names(flags)[[i]], "\n",
                         "#'Access to private parameter '.",
                         names(flags)[[i]], "'\n")

        if(mutable){
            af_str <- paste0(af_str,
                             names(flags)[[i]], " = function(value) {\n",
                             "if(missing(value)) {\n",
                             "private$.", names(flags)[[i]], "\n",
                             "}else{\n",
                             "private$.", names(flags)[[i]], " <- value\n",
                             "}\n")

        }else{
            af_str <- paste0(af_str,
                             names(flags)[[i]], " = function() {\n",
                             "private$.", names(flags)[[i]], "\n")
        }

        af_str <- paste0(af_str, "}")

        af_strs <- c(af_strs, c(af_str))
    }

    af_str <- paste(af_strs, collapse = ",\n\n")

    if(print_result){
        cat(af_str, "\n")
    }

    return(invisible(af_str))
}


#'flags_to_r6_private_str
#'@description Helper function to generate a string out of a flag vector
#'@param flags vector: Flags
#'@param print_result flag: Should the result be printed to the console?
flags_to_r6_private_str <- function(flags, print_result = FALSE){

    assertthat::assert_that(is.logical(flags))
    assertthat::assert_that(assertthat::is.flag(print_result))

    for(i in seq_len(length(flags))){
        names(flags)[[i]] <- paste0(".", names(flags)[[i]])
    }

    flag_str <- paste(names(flags),
                      rep("NULL", length(names(flags))),
                      sep = " = ",
                      collapse = ",\n")

    if(print_result){
        cat(flag_str, "\n")
    }

    return(invisible(flag_str))
}


#===== code generation for OGS6 class =====


#'generate_add_method
#'@description Helper function to generate an R6 add_* method for a
#' r2ogs6 class object
#'@param tag_name The tag name of the XML element represented by the class
#' object
#'@param parent_tag_name The tag name of the parent of the XML element
#' represented by the class object
generate_add_method <- function(tag_name, parent_tag_name) {

    has_wrapper <- (tag_name != parent_tag_name)

    method_str <- paste0("add_", tag_name, " = function(", tag_name, ") {\n",
                         "assertthat::assert_that(class(", tag_name,
                         ") == \"r2ogs6_", tag_name, "\")\n")

    if(has_wrapper){
        method_str <- paste0(method_str, "private$.", parent_tag_name,
                             " <- c(private$.", parent_tag_name,
                             ", list(", tag_name, "))\n",
                             "}\n")
    }else{
        method_str <- paste0(method_str, "if(!is.null(private$.",
                             tag_name, ")){\n",
                             "warning(\"Overwriting ", tag_name,
                             " variable of OGS6 object\", call. = FALSE)\n",
                             "}\n",
                             "private$.", tag_name, " <- ", tag_name, "\n",
                             "}\n")
    }

    return(invisible(method_str))
}


#'generate_active_field
#'@description Helper function to generate an R6 active field for a OGS6 class
#' parameter
#'@param parameter_name The name of the OGS6 class parameter
generate_active_field <- function(parameter_name){

    af_str <- paste0(parameter_name, " = function(value) {\n",
                     "if (missing(value)) {\n",
                     "private$.", parameter_name, "\n",
                     "} else {\n",
                     "stop(\"To modify $", parameter_name,
                     ", use add_", parameter_name, ".\", call . = FALSE)\n",
                     "}\n",
                     "}\n")

    return(invisible(af_str))
}
