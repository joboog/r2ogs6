

#'build_from_prj_files
#'@description Builds class structure based on supplied .prj file(s).
#' Supplying a path to this function should not be necessary because it will
#' attempt to get all info from the official OGS6 benchmarks at (link).
#' However, if you aren't connected to the internet, you can supply a path to
#' a local benchmark directory / .prj file.
#'
#' STRICTLY WIP!!!
#'@param benchmark_path Optional: Path to benchmark files to base the structure
#' on
build_from_prj_files <- function(benchmark_path = "") {

    #WIP, not implemented yet

    #Delete old benchmarks
    # do.call(file.remove, list(list.files("extdata/benchmarks",
    #                                      full.names = TRUE)))

    #delete_old_classes()

    #Mine benchmark files (WIP)
    #scrape_benchmarks(benchmark_path)

    #Generate classes based on data from benchmarks (WIP)
    #generate_classes(benchmark_pkg_path)

    #Generate benchmark scripts with objects of the classes we just generated
    #generate_all_benchmarks()
}



#'get_xml_structure_info
#'@description Experimental function (WIP)
#'@param benchmark_pkg_path The directory within the package that the benchmark
#' files are downloaded to (and will be deleted from later)
#'@param xml_node_name Optional: The name of the XML node we're analysing. This
#' parameter is for recursion utility and should be left alone.
#'@param iteration_depth Optional: The iteration depth. This parameter is for
#' recursion utility and should be left alone.
get_xml_structure_info <- function(benchmark_pkg_path,
                             xml_node_name = "OpenGeoSysProject",
                             iteration_depth = 0){

    node_info <- analyse_xml(benchmark_pkg_path, "\\.prj$",
                             xml_node_name, FALSE)

    #Recursion stops here
    if(length(node_info[["children"]]) == 0){
        return(invisible(c(tag_name = node_info[["tag_name"]],
                           attrs = node_info[["attributes"]])))
    }

    is_wrapper_node <- ""

    #If the node has children wrap the info about them in this list
    node_family_info <- list(tag_name = xml2::xml_name(xml_node),
                             attrs = xml2::xml_attrs(xml_node),
                             children = list(),
                             is_wrapper_node = is_wrapper_node)

    for(i in seq_len(length(node_info[["children"]]))){
        child_name <- names(node_info[["children"]])[[i]]

        child_info <- generate_classes(node_info[["children"]][[i]],
                                       (iteration_depth + 1))

        node_family_info[[node_children]] <-
            c(node_family_info[[node_children]],
              list(child_repr))
    }

    #If we're directly under root, examine type of node
    if(iteration_depth == 0){

        if(is_wrapper_node){

        }


    }
}


#'code_from_xml_info
#'@description Experimental function to generate code from info from a list
#'@param info_list The list with info about an XML element
#'@param is_class_parameter Is the element a class parameter?
#'@param is_required Is the element required?
#' (i.e. will it be verified on its own or as part of another parameter?)
code_from_xml_info <- function(info_list, is_class_parameter, is_required){

    #Recursion stops here
    if(info_list[["type"]] == "leaf"){

        ret_snippets <- character()

        if(is_class_parameter){
            value_type = get_value_type(info_list[["tag_name"]])

            if (value_type == "string") {
                ret_snippets <-
                    c(
                        for_constructor = paste0(
                            "assertthat::assert_that(",
                            "assertthat::is.string(",
                            info_list[["tag_name"]],
                            "))"
                        )
                    )
            } else{
                ret_snippets <-
                    c(
                        for_constructor = paste0(
                            "assertthat::assert_that(",
                            "is.double(",
                            info_list[["tag_name"]],
                            "))"
                        ),
                        for_helper = paste0(
                            "if(assertthat::is.string(",
                            info_list[["tag_name"]],
                            ")) {\n",
                            info_list[["tag_name"]],
                            " <- as.double(",
                            info_list[["tag_name"]],
                            ")\n",
                            "}\n"
                        )
                    )
            }

            if (!is_required) {
                ret_snippets[["for_constructor"]] <-
                    paste0("if(!is.null(",
                           info_list[["tag_name"]],
                           ")){\n",
                           ret_snippets[["for_constructor"]],
                           "}\n")
            }
        }

        return(invisible(ret_snippets))
    }

    if(info_list[["is_class"]]){
        ret_snippets <- character()

        #Generate constructor
        ret_snippets[["for_constructor"]] <-
            generate_constructor(info_list[["tag_name"]],
                                 info_list[["child_flags"]])

        #Generate helper for export
        ret_snippets[["for_helper"]] <- generate_helper()

        return(invisible(ret_snippets))
    }

    if(info_list[["type"]] == "leaf_parent"){

        ret_snippets <- character()

        if(is_class_parameter){

        }

        return(invisible(ret_snippets))
    }
}


#'generate_constructor
#'@description Helper function to generate a constructor out of a tag name
#' and a flag vector
#'@param tag_name The name of the XML element the class will be based on
#'@param param_flags The parameters for the class and if they are required or
#' not (i.e. 'c(a = TRUE, b = FALSE)')
#'@param parent_tag Optional: For subclasses, the parent class tag name will be
#' appended to the class name
generate_constructor <- function(tag_name, param_flags, parent_tag = ""){

    assertthat::assert_that(assertthat::is.string(tag_name))
    assertthat::assert_that(is.logical(param_flags))
    assertthat::assert_that(assertthat::is.string(parent_tag))

    class_name <- paste0("new_r2ogs6_", parent_tag, tag_name)

    param_flags <- sort(param_flags, decreasing = TRUE)
    param_names <- names(param_flags)
    param_str <- flags_to_str(param_flags)

    con_str <- paste0(class_name, " <- function(", param_str, ") {\n")

    #Add validation utility here

    con_str <- paste0(con_str,
                      "structure(list(",
                      paste(param_names,
                            param_names, sep = " = ",
                            collapse = ",\n"),
                      "tag_name = \"", tag_name, "\",\n",
                      "is_subclass = ", ",\n",
                      "attr_names = ", ",\n",
                      "flatten_on_exp = ", "\n",
                      "),\n",
                      "class = \"", class_name, "\"\n",
                      ")\n",
                      "}\n")

    return(invisible(con_str))
}


#'generate_helper
#'@description Helper function to generate a helper out of a tag name
#' and a flag vector
#'@param tag_name The name of the XML element the helper will be based on
#'@param param_flags The parameters for the class and if they are required or
#' not (i.e. 'c(a = TRUE, b = FALSE)')
#'@param parent_tag Optional: For subclasses, the parent class tag name will be
#' appended to the class name
generate_helper <- function(tag_name, param_flags, parent_tag = ""){

    assertthat::assert_that(assertthat::is.string(tag_name))
    assertthat::assert_that(is.logical(param_flags))
    assertthat::assert_that(assertthat::is.string(parent_tag))

    class_name <- paste0("r2ogs6_", parent_tag, tag_name)

    param_flags <- sort(param_flags, decreasing = TRUE)
    param_names <- names(param_flags)
    param_str <- flags_to_str(param_flags)

    helper_str <- paste0(class_name, " <- function(", param_str, ") {\n")

    #Add coercing utility here

    helper_str <- paste0(helper_str, "new_", class_name, "(", param_str, ")\n",
                         "}\n")

    return(invisible(helper_str))
}


#'flags_to_str
#'@description Helper function to generate a string out of a flag vector
#'@param flags A flag vector
flags_to_str <- function(flags) {

    assertthat::assert_that(is.logical(flags))

    flag_strs <- character()

    for(i in seq_len(length(flags))){
        if(flags[[i]]){
            flag_strs <- c(flag_strs, names(flags)[[i]])
        }else{
            flag_strs <- c(flag_strs, paste(names(flags)[[i]], "= NULL"))
        }
    }

    return(invisible(paste(flag_strs, collapse = ", ")))
}


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
