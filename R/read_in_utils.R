
#============================== VALIDATION UTILITY ================================


#'validate_read_in_xml
#'@description Utility function, tries parsing the provided file as an XML document
#'@param file A file to be parsed as XML
#'@return The parsed XML file (as class object of type xml2::xml_document)
validate_read_in_xml <- function(file){

    assertthat::assert_that(assertthat::is.string(file))

    #Attempt to read in file
    xml_doc <- tryCatch(
        {
            return(invisible(xml2::read_xml(file, encoding="ISO-8859-1")))
        },
        error = function(e){
            print(e)
            stop("Could not find file (see error message above), aborting call.", call. = FALSE)
        }
    )
}


#============================== GENERAL READ IN UTILITY ================================


#'read_in
#'@description Reads in elements from a .prj file
#'@param ogs6_obj A OGS6 class object
#'@param prj_path The path to the project file the elements should be read from
#'@param element_name The name of the .prj element to be read from (wrapper element, e.g. 'processes')
#'@param child_name The name of the child elements (e.g. 'process')
#'@param selection_vector Optional: Either a character vector containing the names of the children
#' OR a numeric vector containing their wanted indices
#'@param subclasses_names Optional: A named character vector containing the names of r2ogs6_*
#' subclasses (r2ogs6_* classes without a method for input_add)
#' e.g. c(process = "r2ogs6_tl_process") if child_name would be time_loop
read_in <- function(ogs6_obj, prj_path, element_name, child_name,
                            selection_vector = NULL, subclasses_names = character()){

    assertthat::assert_that("OGS6" %in% class(ogs6_obj))
    xml_doc <- validate_read_in_xml(prj_path)

    assertthat::assert_that(assertthat::is.string(element_name))
    assertthat::assert_that(assertthat::is.string(child_name))

    has_names <- FALSE
    has_indices <- FALSE

    if(!is.null(selection_vector)){
        if(is.numeric(selection_vector)){
            has_indices <- TRUE
        }else if(is.character(selection_vector)){
            has_names <- TRUE
        }else{
            stop("selection_vector must either be of type 'numeric' or 'character'", call. = FALSE)
        }
    }

    assertthat::assert_that(is.character(subclasses_names))

    element <- xml2::xml_find_first(xml_doc, paste0("/OpenGeoSysProject/", element_name))

    if(class(element) == "xml_missing"){
        # warning(paste("read_in: Could not find element of name ", element_name,
        #               ". Skipping.", call. = FALSE))
        return(invisible(FALSE))
    }

    #For most wrapper classes the child_name parameter is useless
    #(e.g. they contain only elements of type child_name),
    #but for time_loop, finding it by name is required instead of just getting all children
    #because it's parent is the .prj file root node and it'd get the whole document
    element_children <- NULL

    if(element_name != child_name){
        element_children <- xml2::xml_find_all(element, child_name)
    }else{
        element_children <- list(element)
    }

    #cat(paste0("Found ", length(element_children), " child objects of name '", child_name, "'\n"))

    r2ogs6_obj <- NULL

    #Code to be parsed later, when r2ogs6_obj has been defined
    add_call <- paste0("ogs6_obj$add_", child_name, "(r2ogs6_obj)")

    #If selection_vector was a character vector
    if(has_names){
        for(i in seq_len(length(selection_vector))){
            specified_name <- selection_vector[[i]]
            regex <- paste0("./", child_name, "[./name = '", specified_name, "']")
            child <- xml2::xml_find_first(xml_doc, regex)
            if(class(child) == "xml_missing"){
                warning(paste("Child with name", xml2::xml_name(child),
                              "not found. Skipping."), call. = FALSE)
                next
            }
            r2ogs6_obj <- node_to_r2ogs6_obj(child, subclasses_names)

            #Now r2ogs6_obj is defined, so we can add it with our code snippet.
            eval(parse(text = add_call))
        }

    #If selection_vector was a numeric vector
    }else if(has_indices){
        for(i in seq_len(length(selection_vector))){
            if(selection_vector[[i]] > length(element_children)){
                warning(paste("Specified child index", selection_vector[[i]],
                              "out of range. Skipping."), call. = FALSE)
                next
            }
            child <- element_children[[selection_vector[[i]]]]
            r2ogs6_obj <- node_to_r2ogs6_obj(child, subclasses_names)

            #Now r2ogs6_obj is defined, so we can add it with our code snippet.
            eval(parse(text = add_call))
        }

    #If selection_vector was NULL, parse all children
    }else{
        for(i in seq_along(element_children)){
            r2ogs6_obj <- node_to_r2ogs6_obj(element_children[[i]], subclasses_names)

            #Now r2ogs6_obj is defined, so we can add it with our code snippet.
            eval(parse(text = add_call))
        }
    }

    return(invisible(TRUE))
}


#'node_to_r2ogs6_obj
#'@description Takes an XML node and turns it into a class object
#'@param xml_node An XML node (of class xml2::xml_node)
#'@param subclasses_names Optional: A character vector containing the names of r2ogs6_*
#' subclasses (r2ogs6_* classes without a method for input_add)
node_to_r2ogs6_obj <- function(xml_node, subclasses_names = character()){

    assertthat::assert_that(class(xml_node) == "xml_node")

    parameter_nodes <- xml2::xml_children(xml_node)

    parameters <- list()

    if(length(xml2::xml_attrs(xml_node)) != 0){
        parameters <- c(parameters, xml2::xml_attrs(xml_node))
    }

    for(i in seq_along(parameter_nodes)){

        #Guesses the R representation of the node and adds it to parameter list
        parameters <- c(parameters, list(guess_structure(parameter_nodes[[i]], subclasses_names)))

        #Names the parameter after the xml_node child name
        names(parameters)[[length(parameters)]] <- xml2::xml_name(parameter_nodes[[i]])
    }

    class_name <- ""

    #If the node is a subclass, get its class name from the subclasses_names vector
    if(xml2::xml_name(xml_node) %in% names(subclasses_names)){
        class_name <- paste0(subclasses_names[[xml2::xml_name(xml_node)]])
        #Else just assume the class name is r2ogs6_ + the node name
    }else{
        class_name <- paste0("r2ogs6_", xml2::xml_name(xml_node))
    }

    ordered_parameters <- order_parameters(parameters, class_name)

    #Construct the call to the r2ogs6_object helper
    class_constructor_call <- paste0(class_name,
                                     "(",
                                     paste(names(parameters),
                                           lapply(parameters,
                                                  function(x){paste(utils::capture.output(dput(x)), collapse="\n")}),
                                           sep = " = ",
                                           collapse = ", "),
                                     ")")

    #Evaluate the constructed call
    r2ogs6_obj <- eval(parse(text = class_constructor_call))

    return(invisible(r2ogs6_obj))
}


#'order_parameters
#'@description Orders a list of parameters corresponding to the argument order of a class
#'@param parameter_list A list of parameters
#'@param class_name The name of a class
order_parameters <- function(parameter_list, class_name){

    assertthat::assert_that(is.list(parameter_list))
    assertthat::assert_that(assertthat::is.string(class_name))

    ordered_parameters <- list()

    #Gets the class parameters in the correct order
    class_args <- names(as.list(formals(class_name)))

    #Check for length mismatches
    if(length(parameter_list) > length(class_args)){
        stop(paste0("order_parameters: More parameters in parameter_list",
                    "than parameters in definition of class '", class_name,
                    "'. Please check the class definition!"), call. = FALSE)
    }

    #Check for value mismatches
    for(i in seq_len(length(parameter_list))){
        if(!names(parameter_list)[[i]] %in% class_args){
            stop(paste0("order_parameters: Found element named '",
                        names(parameter_list)[[i]],
                        "', in parameter_list.",
                        " This element is not a parameter of class '", class_name,
                        "'. Please check the class definition!"), call. = FALSE)
        }
    }

    for(i in seq_len(length(class_args))){
        if(!class_args[[i]] %in% names(parameter_list)){
            ordered_parameters[[class_args[[i]]]] <- NULL
        }else{
            ordered_parameters[[class_args[[i]]]] <- parameter_list[[class_args[[i]]]]
        }
    }

    return(invisible(ordered_parameters))
}


#============================== GUESS STRUCTURE FUNCTIONALITY ================================


#'guess_structure
#'@description Guesses the R representation of an XML node and adds it to parameter list
#'ASSUMPTIONS:
#'1) Leaf nodes will have EITHER a value OR attributes (and will not be missing both, e.g. '<a/>').
#'2) Leaf nodes will never be r2ogs6_* objects
#'3) If there are multiple occurrences of r2ogs6_* class (and subclass) elements on the same level,
#' they have a wrapper node as their parent (e.g. <processes>, <properties>) which
#' will contain ONLY elements of this type
#'4) Wrapper nodes are represented as lists
#'5) Parent nodes whose children have no children are represented as lists
#'@param xml_node An XML node (of class xml2::xml_node)
#'@param subclasses_names Optional: A character vector containing the names of r2ogs6_*
#' subclasses (r2ogs6_* classes without a method for input_add)
guess_structure <- function(xml_node, subclasses_names = character()){

    assertthat::assert_that(class(xml_node) == "xml_node")

    #Node is a leaf node
    if(length(xml2::xml_children(xml_node)) == 0){
        if(xml2::xml_text(xml_node) != ""){
            return(invisible(xml2::xml_text(xml_node)))
        }else{
            return(invisible(xml2::xml_attrs(xml_node)))
        }

    #Node is represented by subclass
    }else if(xml2::xml_name(xml_node) %in% names(subclasses_names)){
        return(invisible(node_to_r2ogs6_obj(xml_node, subclasses_names)))

    #Node has children that are represented by a subclass
    }else if(is_subclass_wrapper(xml_node, subclasses_names) ||
             is_het_wrapper(xml_node, subclasses_names)){

        wrapper_list <- list()

        for(i in seq_along(xml2::xml_children(xml_node))){
            child_node <- xml2::xml_children(xml_node)[[i]]
            list_content <- NULL
            if(xml2::xml_name(child_node) %in% names(subclasses_names)){
                list_content <- node_to_r2ogs6_obj(child_node, subclasses_names)
            }else{
                list_content <- guess_structure(child_node)
            }
            wrapper_list <- c(wrapper_list, list(list_content))
        }

        return(invisible(wrapper_list))

    #Return values for parent nodes whose children have no children
    }else if(all(get_grandchild_length_vector(xml_node) == 0)){
        return(invisible(list_from_nodeset(xml2::xml_children(xml_node))))

    #If the children of the parent node do have children, look for custom function
    }else{
        read_in_func_call <- find_read_in_func_call(xml2::xml_name(xml_node))

        if(read_in_func_call != ""){
            obj <- eval(parse(text = read_in_func_call))
            return(invisible(obj))

        #If the node does NOT have its own function, abort mission
        }else{
            stop(paste0("Could not guess the structure of node with name '",
                        xml2::xml_name(xml_node), "', please read the ",
                        "documentation for guess_structure() and which ",
                        "assumptions it is based on."), call. = FALSE)
        }
    }
}


is_subclass_wrapper <- function(xml_node, subclasses_names){

    child_nodes <- xml2::xml_children(xml_node)

    for(i in seq_along(child_nodes)){
        if(!xml2::xml_name(child_nodes[[i]]) %in% names(subclasses_names)){
            return(invisible(FALSE))
        }
    }

    return(invisible(TRUE))
}

#'is_het_wrapper
#'@description Tests if a node is a heterogenous wrapper
#'@param xml_node An XML node (of class xml2::xml_node)
#'@param subclasses_names Optional: A character vector containing the names of r2ogs6_*
#' subclasses (r2ogs6_* classes without a method for input_add)
is_het_wrapper <- function(xml_node, subclasses_names = character()){

    child_nodes <- xml2::xml_children(xml_node)

    leaves_found <- FALSE
    parent_found <- FALSE

    for(i in seq_along(child_nodes)){
        if(length(xml2::xml_children(child_nodes[[i]])) != 0 &&
           !xml2::xml_name(child_nodes[[i]]) %in% names(subclasses_names) &&
           find_read_in_func_call(xml2::xml_name(child_nodes[[i]])) == ""){
            return(invisible(FALSE))
        }

        if(length(xml2::xml_children(child_nodes[[i]])) == 0){
            leaves_found <- TRUE
        }

        if(xml2::xml_name(child_nodes[[i]]) %in% names(subclasses_names) ||
           find_read_in_func_call(xml2::xml_name(child_nodes[[i]])) != ""){
            parent_found <- TRUE
        }
    }

    if(leaves_found && parent_found){
        return(invisible(TRUE))
    }

    return(invisible(FALSE))
}


#'get_grandchild_length_vector
#'@description Helper function to check the number of children of children of an XML node
#' (i.e. grandchildren)
#'@param xml_node An XML node
#'@return A numeric vector containing the number of children of each child of xml_node
get_grandchild_length_vector <- function(xml_node){

    length_vector <- c()

    for(i in seq_along(xml2::xml_children(xml_node))){
        child_node <- xml2::xml_children(xml_node)[[i]]

        length_vector <- c(length_vector, length(xml2::xml_children(child_node)))
    }

    return(invisible(length_vector))
}


#'list_from_nodeset
#'@description Creates a named vector from a nodeset
#'@param xml_nodeset An XML nodeset (of class xml2::xml_nodeset)
list_from_nodeset <- function(xml_nodeset){
    assertthat::assert_that(class(xml_nodeset) == "xml_nodeset")

    my_list <- list()

    for(i in seq_along(xml_nodeset)){
        if(xml2::xml_text(xml_nodeset[[i]]) != ""){
            my_list <- c(my_list, list(xml2::xml_text(xml_nodeset[[i]])))
        }else{
            my_list <- c(my_list, list(xml2::xml_attrs(xml_nodeset[[i]])))
        }

        names(my_list)[[length(my_list)]] <- xml2::xml_name(xml_nodeset[[i]])
    }

    return(invisible(my_list))
}


#============================== FIND CUSTOM READ IN FUNCTIONS UTILITY ================================


#'find_read_in_func_call
#'@description Checks if a custom read_in function was defined for nodes with the given name
#'@param node_name The name of an XML node
#'@return A ready-to-call string if a corresponding function was found, an empty string otherwise
find_read_in_func_call <- function(node_name){

    assertthat::assert_that(assertthat::is.string(node_name))

    func_name <- paste0("read_in_", node_name, "_node")

    if(exists(func_name, where=asNamespace("r2ogs6"), mode="function")){
        func_call <- paste0(func_name, "(xml_node)")
        return(invisible(func_call))
    }

    return(invisible(""))
}


#============================== FILE HANDLING UTILITY ================================

#'check_file_extension
#'@description Helper function to check the extension of a file
#'@param file A file
#'@param expected_extension The expected file extension
check_file_extension <- function(file, expected_extension){

    assertthat::assert_that(assertthat::is.string(file))
    assertthat::assert_that(assertthat::is.string(expected_extension))

    if(tools::file_ext(file) != expected_extension){
        stop(paste("File must have extension", expected_extension), call. = FALSE)
    }
}


#Source: https://stackoverflow.com/questions/48218491/os-independent-way-to-select-directory-interactively-in-r/48296736
#Helper function for choosing a directory (platform independent!)
choose_directory = function(ini_dir = getwd(), caption = 'Select data directory') {
    if (exists('utils::choose.dir')) {
        utils::choose.dir(default = ini_dir, caption = caption)
    } else {
        tcltk::tk_choose.dir(default = ini_dir, caption = caption)
    }
}