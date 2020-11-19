#This is a script containing experimental functions which generate other functions.
#These will not be exported, but in the best case developers can use them to make their workflow more efficient.

#Experimental stuff
generate_from_element <- function(element_name, attrs, children) {


}



generate_class_from_element <- function(element, export = TRUE) {

    children <- xml2::xml_children(element)
    attrs <- xml2::xml_attrs(element)
    element_name <- xml2::xml_name(element)

    #Generate name of new class from element name if class_name is not specified
    if(is.null(class_name)){
        class_name <- snakecase::to_any_case(element_name, "snake")
    }


    #Start of description
    class_str <- paste0("#'r2ogs6_", element_name, "\n",
                        "#'@description r2ogs6 class representing a ", element_name, " element \n")

    #Add parameter documentation tag(s)
    parameters <- list()

    for(i in seq_len(length(children))){
        parameters <- c(parameters, xml2::xml_name(children[[i]]))

        class_str <- paste0(class_str, "#'@param ", xml2::xml_name(children[[i]]), " (Auto generated)\n")
    }

    #Add export documentation tag if class should be exported
    if(export){
        class_str <- paste0(class_str, "#'@export\n")
    }

    #Start of declaration
    class_str <- paste0(class_str, "r2ogs6_", element_name, " <- function(\n")

    #ADD PARAMETERS HERE

    class_str <- paste0(class_str, ") {\n")

    #ADD FUNCTION CONTENTS

    class_str <- paste0(class_str, "}\n")


    generated_class <- structure(list(), class = element_name)

    return(generated_class)
}

generate_validator_from_element <- function() {

}


#============================== AS_NODE METHOD GENERATOR ================================


#'generate_as_node_func
#'@description Generates a method for the generic function as_node based on an XML element
#'@param file The XML file to parse
#'@param element_name The name of the XML element to base the method on
#'@param subclasses Optional: A named vector of subclasses
#'@param show_result Should the generated function be printed to the console?
generate_as_node_func <- function(file, element_name, subclasses = NULL, show_result = TRUE) {

    xml_doc <- validate_read_in_xml(file)

    #doc_matches <- xml2::xml_find_all(xml_doc, paste("//", element_name, sep = ""))
    element <- xml2::xml_find_first(xml_doc, paste("//", element_name, sep = ""))
    element_attrs <- xml2::xml_attrs(element)

    method_str <- paste0("#'as_node.r2ogs6_", element_name, "\n",
                         "#'@description Implementation of generic as_node for class r2ogs6_", element_name, "\n",
                         "#'@param x A r2ogs6_", element_name, " class object\n",
                         "#'@param ... Ellipsis\n",
                         "as_node.r2ogs6_", element_name, " <- function(x, ...) {\n")

    method_str <- paste0(method_str, generate_as_node_method_content(element, element_attrs))

    method_str <- paste0(method_str, "\treturn(invisible(", element_name, "_node))\n", "}\n")

    if(show_result){
        cat(method_str)
    }

    return(invisible(method_str))
}


#'generate_as_node_method_content
#'@description ...
#'@param element The XML element to base the method on
#'@param iteration_depth Utility parameter for the recursion, just leave this alone!
#'@param subclasses Optional: A named vector of subclasses
generate_as_node_method_content <- function(element, iteration_depth = 0, subclasses = NULL) {

    children <- xml2::xml_children(element)
    attrs <- xml2::xml_attrs(element)
    element_name <- xml2::xml_name(element)

    if(length(children) == 0){
        #return(paste0("\t", element_name, "_node <- as_node(x, )\n"))

        return_str <- paste0("\t", element_name, "_node <- as_node(x$", element_name)

        if(length(attrs) != 0){
            return_str <- paste0(return_str, ", '', ", attrs)
        }

        return_str <- paste0(return_str, ")\t#atomic\n")
        return(return_str)
    }

    child_strings <- list()

    for(i in seq_len(length(children))){
        child_strings[[length(child_strings) + 1]] <-
            generate_as_node_method_content(children[[i]], (iteration_depth + 1), subclasses)
    }

    if(strings_equal(child_strings)){
        return_str <- paste0("\t", element_name, "_node <- group_nodes(x$", element_name)

        if(length(attrs) != 0){
            return_str <- paste0(return_str, ", ", attrs)
        }

        return_str <- paste0(return_str, ")\n")

        return(return_str)
    }

}


#'strings_equal
#'@description Checks if all strings in a vector or list are equal
#'@param strings_vector A vector or a list of strings
strings_equal <- function(strings_vector) {
    assertthat::assert_that(is.vector(strings_vector))

    for(i in seq_len(length(strings_vector))){
        if(strings_vector[[i]] != strings_vector[[1]]){
            return(invisible(FALSE))
        }
    }

    return(invisible(TRUE))
}


#... (WIP)
group_nodes <- function(parent_list, parent_attributes = NULL, child_attributes = NULL) {

    assertthat::assert_that(is.list(parent_list))

    #Deparses the parameter given to the function
    parameter_name <- deparse(substitute(parent_list))

    #Automatically generates the element name based on the name of the parameter
    parent_name <- strsplit(parameter_name, "$", fixed = TRUE)[[2]]

    if(!is.null(parent_attributes)){
        assertthat::assert_that(is.vector(parent_attributes))
    }

    node <- list(structure(list(), parent_attributes))
    names(node)[[1]] <- parent_name

    for(i in seq_len(length(parent_list))){
        node[[1]][[length(node[[1]] + 1)]] <- as_node(parent_list[[i]])
    }

    return(invisible(node))
}


#============================== READ_IN FUNCTION GENERATOR ================================

#'generate_simple_read_in
#'@description Assuming function read_in gets good enough results, this could save code later.
#'@param element_name The name of the .prj element to be read from (wrapper element, e.g. 'processes')
#'@param child_name The name of the element children (e.g. 'process')
#'@param has_name_tag Do the child elements have a child element with the name 'name'?
#'@param subclasses_names Optional: A character vector containing the names of r2ogs6_*
#' subclasses (r2ogs6_* classes without a method for input_add)
generate_simple_read_in <- function(element_name, child_name,
                                    has_name_tag = TRUE, subclasses_names = NULL){

    assertthat::assert_that(assertthat::is.string(element_name))
    assertthat::assert_that(assertthat::is.string(child_name))
    assertthat::assert_that(assertthat::is.flag(has_name_tag))

    if(!is.null(subclasses_names)){
        assertthat::assert_that(is.character(subclasses_names))
    }


    func_str <- paste0("#'read_in_", element_name, "\n",
                       "#'@description Reads in ", child_name, " elements from a .prj file\n",
                       "#'@param ogs6_obj A OGS6 class object\n",
                       "#'@param prj_path The path to the project file the ", child_name,
                       " elements should be read from\n")

    if(has_name_tag){
        func_str <- paste0(func_str, "#'@param ", child_name, "_names Optional: The names of the ",
                           child_name, " elements to be read in\n")
    }else{
        func_str <- paste0(func_str, "#'@param ", child_name, "_indices Optional: The indices of the ",
                           child_name, " elements to be read in\n")
    }

    func_str <- paste0(func_str, "#'@export\n",
                       "read_in_", element_name, " <- function(ogs6_obj, prj_path, ")

    if(has_name_tag){
        func_str <- paste0(func_str, child_name, "_names = NULL) {\n")
    }else{
        func_str <- paste0(func_str, child_name, "_indices = NULL) {\n")
    }

    func_str <- paste0(func_str, "read_in(ogs6_obj, prj_path, \"", element_name,
                       "\", \"", child_name, "\", selection_vector = ")

    if(has_name_tag){
        func_str <- paste0(func_str, child_name, "_names, subclasses_names = ")
    }else{
        func_str <- paste0(func_str, child_name, "_indices, subclasses_names = ")
    }

    if(!is.null(subclasses_names)){
        subclasses_str <- utils::capture.output(invisible(dput(subclasses_names)))
        subclasses_str <- paste(subclasses_str, collapse = "")
        func_str <- paste0(func_str, subclasses_str, ")\n")
    }else{
        func_str <- paste0(func_str, "NULL)\n")
    }

    func_str <- paste0(func_str, "}\n")

    cat(func_str)
    return(invisible(func_str))
}