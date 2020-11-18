#Functions to read in data from a .prj file to an ogs6 object


#'read_in_all
#'@description Wrapper function to read in all relevant elements from a .prj file
#'
read_in_all <- function(ogs6_obj, path){

    xml_doc <- ""

    read_in_processes(ogs6_obj, xml_doc)

}

#============================== "LET'S TYPE IT OUT" APPROACH ================================

#'read_in_processes
#'@description Reads in parameter elements from a .prj file
#'@param ogs6_obj A OGS6 class object
#'@param prj_path
#'@param process_names Optional: The names of the process elements to be read in
read_in_processes <- function(ogs6_obj, prj_path, process_names = NULL){

    xml_doc <- ""

    processes <- xml2::xml_find_first(xml_doc, "//processes")

    if(!is.null(process_names)){
        assertthat::assert_that(is.vector(process_names))
    }

    #... differ between process by names and just all processes

    process_elements <- xml2::xml_children(processes)

    for(i in seq_len(length(process_elements))){
        name <- ""
        type <- ""
        integration_order <- ""
        dimension <- ""
        constitutive_relation <- ""
        process_variables <- ""
        secondary_variables <- ""
        specific_body_force <-
        coupling_scheme <- ""

        ogs6_obj$add_process(name, type, integration_order, dimension, constitutive_relation,
                             process_variables, secondary_variables, specific_body_force, coupling_scheme)


    }

}


#'read_in_parameter
#'@description Reads in parameter elements from a .prj file
#'@param ogs6_obj A OGS6 class object
#'@param parameter_names Optional: The names of the parameter elements to be read in
read_in_parameter <- function(ogs6_obj, xml_doc, parameter_names = NULL){

    parameters <- xml2::xml_find_first(xml_doc, "//parameters")

    if(!is.null(process_names)){
        assertthat::assert_that(is.vector(process_names))
    }

    #... differ between process by names and just all parameters

    parameter_elements <- xml2::xml_children(parameters)

    for(i in seq_len(length(parameter_elements))){
        name <- ""
        type <- ""

        ogs6_obj$add_parameter(name, type, integration_order, dimension, constitutive_relation,
                             process_variables, secondary_variables, specific_body_force, coupling_scheme)


    }
}


#============================== EXPERIMENTAL APPROACH ================================
#(could be better, could be shit...)


#'read_in_element
#'@description Reads in child elements from a .prj file
#'@param ogs6_obj A OGS6 class object
#'@param prj_path The path to the project file the child elements should be read from
#'@param element_name The name of the .prj element to be read from (wrapper element, e.g. 'processes')
#'@param child_name The name of the element children (e.g. 'process')
#'@param selection_vector Optional: Either a character vector containing the names of the children
#' OR a numeric vector containing their wanted indices
#'@param subclasses_names Optional: A character vector containing the names of r2ogs6_*
#' subclasses (r2ogs6_* classes without a method for input_add)
read_in_element <- function(ogs6_obj, prj_path, element_name, child_name,
                            selection_vector = NULL, subclasses_names = NULL){

    assertthat::assert_that(class(ogs6_obj) == "OGS6")

    #Attempt to read in file
    out <- tryCatch(
        {
            xml_doc <- xml2::read_xml(prj_path, encoding="ISO-8859-1")
        },
        error = function(e){
            print(e)
            stop("Could not find prj_path (see error message above), aborting call.", call. = FALSE)
        }
    )

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

    if(!is.null(subclasses_names)){
        assertthat::assert_that(is.character(subclasses_names))
    }

    element <- xml2::xml_find_first(xml_doc, paste0("//", element_name))
    element_children <- xml2::xml_children(element)

    r2ogs6_obj <- NULL

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
        }

    #If selection_vector was NULL, parse all children
    }else{
        for(i in seq_len(length(element_children))){
            r2ogs6_obj <- node_to_r2ogs6_obj(element_children[[i]], subclasses_names)
        }
    }

    #Add the object to the OGS6 object
    add_call <- paste0("ogs6_obj.add_", child_name, "(r2ogs6_obj)")
    eval(parse(text = add_call))

    return(invisible(TRUE))
}


#'node_to_r2ogs6_obj
#'@description Takes an XML node and turns it into a class object
#'@param xml_node An XML node (of class xml2::xml_node)
#'@param subclasses_names Optional: A character vector containing the names of r2ogs6_*
#' subclasses (r2ogs6_* classes without a method for input_add)
node_to_r2ogs6_obj <- function(xml_node, subclasses_names = NULL){

    assertthat::assert_that(class(xml_node) == "xml_node")

    class_name <- paste0("r2ogs6_", xml2::xml_name(xml_node))

    parameter_nodes <- xml2::xml_children(xml_node)
    parameters <- list()

    for(i in seq_len(length(parameter_nodes))){

        #Guesses the R representation of the node and adds it to parameter list
        parameters <- c(parameters, list(guess_structure(parameter_nodes[[i]])))

        #Names the parameter after the xml_node child name
        names(parameters)[[length(parameters)]] <- xml2::xml_name(parameter_nodes[[i]])
    }

    ordered_parameters <- order_parameters(parameters)

    #Construct the call to the r2ogs6_object helper
    param_str <- paste(names(ordered_parameters), ordered_parameters, sep = " = ", collapse = ", ")
    constr_call <- paste0(class_name, "(", param_str, ")")

    #Evaluate the constructed call
    r2ogs6_obj <- eval(parse(text = constr_call))

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
    for(i in seq_len(parameter_list)){
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
guess_structure <- function(xml_node, subclasses_names = NULL){

    assertthat::assert_that(class(xml_node) == "xml_node")

    #Return values for leaf nodes
    if(length(xml2::xml_children(xml_node)) == 0){
        if(xml2::xml_text(xml_node) != ""){
            return(invisible(xml2::xml_text(xml_node)))
        }else{
            return(invisible(xml2::xml_attrs(xml_node)))
        }

    #If the node is represented by a subclass
    }else if(xml2::xml_name(xml_node) %in% names(subclasses_names)){
        #Call another instance of node_to_r2ogs6_obj for the subclass
        return(invisible(node_to_r2ogs6_obj(xml_node)))

    #If the node has children that are represented by a subclass
    }else if(xml2::xml_name(xml2::xml_children(xml_node)[[1]]) %in% names(subclasses_names)){
        #WIP(for loop)
        return(invisible(node_to_r2ogs6_obj(xml_node)))

    #Return values for parent nodes whose children have no children
    }else if(any(length(xml2::xml_children(xml2::xml_children(xml_node))) > 0)){
        return(invisible(vector_from_nodeset(xml_children(xml_node))))

    #If the structure goes deeper than that, abort mission
    }else{
        stop(paste0("Could not guess the structure of node with name '",
                   xml2::xml_name(xml_node), "', please read the ",
                   "documentation for guess_structure() and which ",
                   "assumptions it is based on."), call. = FALSE)
    }
}


#'vector_from_nodeset
#'@description Creates a named vector from a nodeset
#'@param xml_nodeset An XML nodeset (of class xml2::xml_nodeset)
vector_from_nodeset <- function(xml_nodeset){
    assertthat::assert_that(class(xml_nodeset) == "xml_nodeset")

    my_vector <- character()

    for(i in seq_len(length(xml_nodeset))){
        if(xml2::xml_text(xml_nodeset[[i]]) != ""){
            my_vector <- c(my_vector, character(xml2::xml_text(xml_nodeset[[i]])))
        }else{
            my_vector <- c(my_vector, character(xml2::xml_attrs(xml_nodeset[[i]])))
        }

        names(my_vector)[[length(my_vector)]] <- xml2::xml_name(xml_nodeset[[i]])
    }

    return(invisible(my_vector))
}

