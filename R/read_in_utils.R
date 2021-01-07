
#===== VALIDATION UTILITY =====


#'validate_read_in_xml
#'@description Utility function, tries parsing the provided file as an XML
#' document
#'@param path string: A file to be parsed as XML
#'@return The parsed XML file (as class object of type xml2::xml_document)
validate_read_in_xml <- function(path){

    assertthat::assert_that(assertthat::is.string(path))

    #Attempt to read in file
    xml_doc <- tryCatch(
        {
            return(invisible(xml2::read_xml(path, encoding="ISO-8859-1")))
        },
        error = function(e){
            print(e)
            stop(paste("Could not find file (see error message above),",
                       "aborting call."), call. = FALSE)
        }
    )
}


has_ambiguous_representation <- function(tag_name) {
    ambiguous_tags <- c("material_property",
                        "fluid",
                        "porous_medium",
                        "relative_permeability",
                        "capillary_pressure")

    return(invisible(tag_name %in% ambiguous_tags))
}


check_could_be_subclass <- function(tag_name, xpath_expr) {
    could_be_subclass <- TRUE

    if(has_ambiguous_representation(tag_name)){

        non_subclass_paths <-
            c("constitutive_relation/material_properties/material_property",
              "processes/process/fluid",
              "processes/process/porous_medium",
              "material_property/porous_medium",
              paste0("material_property/porous_medium/porous_medium/",
                     "relative_permeability"),
              "process/process_variables/capillary_pressure")

        for(i in seq_len(length(non_subclass_paths))){
            split_ncp <-
                unlist(strsplit(non_subclass_paths[[i]], "/", fixed = TRUE))

            regex_friendly_ncp <- paste(split_ncp, collapse = "  ")

            split_xpth <-
                unlist(strsplit(xpath_expr, "/", fixed = TRUE))
            regex_friendly_xpth <- paste(split_xpth, collapse = "  ")

            # cat("\n", regex_friendly_ncp, "\n")
            # cat("\n", regex_friendly_xpth, "\n")

            if(grepl(paste0(regex_friendly_ncp, "$"), regex_friendly_xpth)){
                could_be_subclass <- FALSE
                break
            }
        }
    }

    return(invisible(could_be_subclass))
}


#===== GENERAL READ IN UTILITY =====


#'read_in
#'@description Reads in elements from a file
#'@param ogs6_obj A OGS6 class object
#'@param path string: Path to file XML elements should be read from
#'@param xpath_expr string: An XPath expression (should be absolute!)
read_in <- function(ogs6_obj,
                    path,
                    xpath_expr){

    assertthat::assert_that("OGS6" %in% class(ogs6_obj))
    xml_doc <- validate_read_in_xml(path)

    assertthat::assert_that(assertthat::is.string(xpath_expr))

    split_path <- unlist(strsplit(xpath_expr, "/", fixed = TRUE))
    child_name <- split_path[[length(split_path)]]
    subclasses_names <- get_subclass_names(paste0("r2ogs6_", child_name))

    nodes <- xml2::xml_find_all(xml_doc, xpath_expr)

    if(length(nodes) == 0){
        return(invisible(NULL))
    }

    r2ogs6_obj <- NULL

    #Code to be parsed when r2ogs6_obj has been defined
    add_call <- paste0("ogs6_obj$add_", child_name, "(r2ogs6_obj)")

    #Parse all children
    for (i in seq_len(length(nodes))) {

        r2ogs6_obj <- node_to_r2ogs6_obj(nodes[[i]],
                                         xpath_expr,
                                         subclasses_names)

        #Add r2ogs6_obj with code snippet
        # cat("\n", add_call, "\n")
        eval(parse(text = add_call))
    }

    return(invisible(r2ogs6_obj))
}


#'node_to_r2ogs6_obj
#'@description Takes an XML node and turns it into a class object
#'@param xml_node An XML node (of class xml2::xml_node)
#'@param xpath_expr An XPath expression (for subclass differentiation)
#'@param subclasses_names Optional: A character vector containing the names of
#' r2ogs6 subclasses (r2ogs6 classes without a method for input_add)
node_to_r2ogs6_obj <- function(xml_node,
                               xpath_expr,
                               subclasses_names = character()){

    assertthat::assert_that(class(xml_node) == "xml_node")

    parameter_nodes <- xml2::xml_children(xml_node)

    parameters <- list()

    init_prefix <- ""

    if(length(xml2::xml_attrs(xml_node)) != 0){
        parameters <- c(parameters, xml2::xml_attrs(xml_node))
    }

    for(i in seq_len(length(parameter_nodes))){

        new_xpath_expr <- paste0(xpath_expr,
                             "/",
                             xml2::xml_name(parameter_nodes[[i]]))

        #Guess R representation of node, add it to parameter list
        parameters <- c(parameters, list(guess_structure(parameter_nodes[[i]],
                                                         new_xpath_expr,
                                                         subclasses_names)))

        #Name parameter after the xml_node child name
        names(parameters)[[length(parameters)]] <-
            xml2::xml_name(parameter_nodes[[i]])
    }

    class_name <- ""
    tag_name <- xml2::xml_name(xml_node)

    #If node represented by subclass, get class name
    if(tag_name %in% names(subclasses_names)){
        class_name <- select_fitting_subclass(xpath_expr, subclasses_names)

    #Else assume class name is r2ogs6_ + node name
    }else{
        class_name <- get_tag_class_name(tag_name)
    }

    #If it's an R6 class, we need to alter constructor syntax a bit
    if(grepl("OGS6", class_name)){
        init_prefix <- "$new"
    }

    ordered_parameters <- order_parameters(parameters, class_name)

    param_call_strs <- lapply(names(parameters), function(x){
        call_str <- paste0("parameters[[\"", x, "\"]]")
        return(call_str)
    })

    #Construct the call to the r2ogs6_object helper
    class_constructor_call <-
        paste0(class_name,
               init_prefix,
               "(",
               paste(
                   names(parameters),
                   param_call_strs,
                   sep = " = ",
                   collapse = ", "
               ),
               ")")

    #Evaluate the constructed call
    r2ogs6_obj <- eval(parse(text = class_constructor_call))

    return(invisible(r2ogs6_obj))
}


get_class_args <- function(class_name){

    assertthat::assert_that(assertthat::is.string(class_name))

    formals_call <- class_name

    if(grepl("OGS6", class_name, fixed = TRUE)){
        formals_call <- paste0(class_name,
                               "$public_methods$initialize")
    }

    class_args <- names(as.list(formals(eval(parse(text = formals_call)))))

    return(invisible(class_args))
}


#'order_parameters
#'@description Orders a list of parameters corresponding to the argument order
#' of a class
#'@param parameters list: Parameters
#'@param class_name string: The name of a class
order_parameters <- function(parameters, class_name){

    assertthat::assert_that(is.list(parameters))
    assertthat::assert_that(assertthat::is.string(class_name))

    ordered_parameters <- list()

    class_args <- get_class_args(class_name)

    # cat("\nParameter names not in", class_name, "class arguments:")
    # print(names(parameters)[!names(parameters) %in% class_args])

    #Check for length and value mismatches if class does not have Ellipsis
    if(!"..." %in% class_args){
        assertthat::assert_that(length(parameters) <= length(class_args))

        for(i in seq_len(length(parameters))){
            # cat("\n", names(parameters)[[i]], "\n")
            assertthat::assert_that(names(parameters)[[i]] %in% class_args)
        }
    }

    # Order regular arguments
    for(i in seq_len(length(class_args))){
        if(class_args[[i]] != "..."){
            if(!class_args[[i]] %in% names(parameters)){
                ordered_parameters[[class_args[[i]]]] <- NULL
            }else{
                ordered_parameters[[class_args[[i]]]] <-
                    parameters[[class_args[[i]]]]
            }
        }
    }

    # Add ellipsis content at the end
    ellipsis_content <- parameters[!names(parameters) %in% class_args]

    for(i in seq_len(length(ellipsis_content))){
        ordered_parameters[[length(ordered_parameters) + 1]] <-
            ellipsis_content[[i]]
        names(ordered_parameters)[[length(ordered_parameters)]] <-
            names(ellipsis_content)[[i]]
    }

    return(invisible(ordered_parameters))
}


#===== GUESS STRUCTURE FUNCTIONALITY =====


#'guess_structure
#'@description Guesses the R representation of an XML node and adds it to
#' parameter list. This is a recursive function.
#'ASSUMPTIONS:
#'1) Leaf nodes will have EITHER a value OR attributes (and will not be missing
#' both, e.g. '<a/>').
#'2) Leaf nodes will never be r2ogs6_* objects
#'3) If there are multiple occurrences of r2ogs6_* class (and subclass)
#' elements on the same level, they have a wrapper node as their parent
#' (e.g. <processes>, <properties>) which  will contain ONLY elements of this
#' type
#'4) Wrapper nodes are represented as lists
#'5) Parent nodes whose children have no children are represented as lists
#'@param xml_node xml2::xml_node: XML node
#'@param xpath_expr string: XPath expression (for subclass differentiation)
#'@param subclasses_names Optional: character: Names of r2ogs6 subclasses
#' (r2ogs6 classes without a OGS6$add method)
guess_structure <- function(xml_node,
                            xpath_expr,
                            subclasses_names = character()){

    assertthat::assert_that("xml_node" %in% class(xml_node))
    assertthat::assert_that(assertthat::is.string(xpath_expr))

    node_name <- xml2::xml_name(xml_node)
    # cat("\n", xpath_expr, check_could_be_subclass(node_name, xpath_expr), "\n")

    #Node is leaf
    if(length(xml2::xml_children(xml_node)) == 0){
        if(xml2::xml_text(xml_node) != ""){
            return(invisible(xml2::xml_text(xml_node)))
        }else{
            return(invisible(xml2::xml_attrs(xml_node)))
        }

    #Node is represented by subclass
    }else if(node_name %in% names(subclasses_names) &&
             check_could_be_subclass(node_name, xpath_expr)){
        return(invisible(node_to_r2ogs6_obj(xml_node,
                                            xpath_expr,
                                            subclasses_names)))

    #Node has children but is not represented by subclass
    }else{

        wrapper_list <- list()

        for (i in seq_len(length((xml2::xml_children(xml_node))))) {
            child_node <- xml2::xml_children(xml_node)[[i]]
            child_name <- xml2::xml_name(child_node)

            list_content <- NULL

            new_xpath_expr <- paste0(xpath_expr,
                                     "/",
                                     child_name)

            if (child_name %in% names(subclasses_names) &&
                check_could_be_subclass(child_name, new_xpath_expr)) {
                list_content <- node_to_r2ogs6_obj(child_node,
                                                   new_xpath_expr,
                                                   subclasses_names)
            }else{
                list_content <- guess_structure(child_node,
                                                new_xpath_expr,
                                                subclasses_names)
            }

            wrapper_list <- c(wrapper_list, list(list_content))
            names(wrapper_list)[[length(wrapper_list)]] <- child_name

        }

        return(invisible(wrapper_list))
    }
}


#===== RECURSIVE IMPORT (WIP)  =====


#
# to_object <- function(xml_node,
#                       xpath_expr,
#                       subclasses_names = character()){
#
#
#
#
# }


#===== FILE HANDLING UTILITY  =====


#'check_file_extension
#'@description Helper function to check the extension of a file
#'@param file A file
#'@param expected_extension The expected file extension
check_file_extension <- function(file, expected_extension){

    assertthat::assert_that(assertthat::is.string(file))
    assertthat::assert_that(assertthat::is.string(expected_extension))

    if(tools::file_ext(file) != expected_extension){
        stop(paste("File must have extension", expected_extension),
             call. = FALSE)
    }
}


#Source: https://stackoverflow.com/questions/48218491/os-independent-way-to-
# select-directory-interactively-in-r/48296736
#Helper function for choosing a directory (platform independent!)
choose_directory = function(ini_dir = getwd(),
                            caption = 'Select data directory') {
    if (exists('utils::choose.dir')) {
        utils::choose.dir(default = ini_dir, caption = caption)
    } else {
        tcltk::tk_choose.dir(default = ini_dir, caption = caption)
    }
}