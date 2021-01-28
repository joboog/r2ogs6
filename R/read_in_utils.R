
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


#===== GENERAL READ IN UTILITY =====


#'read_in
#'@description Reads in elements from a file
#'@param ogs6_obj A OGS6 class object
#'@param path string: Path to file XML elements should be read from
#'@param xpath string: An XPath expression (should be absolute!)
read_in <- function(ogs6_obj,
                    path,
                    xpath){

    assertthat::assert_that("OGS6" %in% class(ogs6_obj))
    xml_doc <- validate_read_in_xml(path)

    assertthat::assert_that(assertthat::is.string(xpath))

    split_path <- unlist(strsplit(xpath, "/", fixed = TRUE))
    child_name <- split_path[[length(split_path)]]

    nodes <- xml2::xml_find_all(xml_doc, xpath)

    if(length(nodes) == 0){
        return(invisible(NULL))
    }

    # Remove root expression for better readability
    xpath <- stringr::str_remove(xpath, "\\/[A-Za-z_]*\\/")

    r2ogs6_obj <- NULL

    #Parse all children
    for (i in seq_len(length(nodes))) {

        r2ogs6_obj <- node_to_r2ogs6_class_object(nodes[[i]],
                                                  xpath)

        #Add r2ogs6_obj with code snippet
        eval(parse(text = "ogs6_obj$add(r2ogs6_obj)"))
    }

    return(invisible(r2ogs6_obj))
}


#'node_to_r2ogs6_class_object
#'@description Takes an XML node and turns it into a class object
#'@param xml_node xml2::xml_node: XML node
#'@param xpath string: XPath expression (for subclass differentiation)
node_to_r2ogs6_class_object <- function(xml_node,
                                        xpath){

    assertthat::assert_that(class(xml_node) == "xml_node")

    parameter_nodes <- xml2::xml_children(xml_node)
    parameters <- c(list(), xml2::xml_attrs(xml_node))

    for(i in seq_len(length(parameter_nodes))){

        new_xpath <- paste0(xpath,
                             "/",
                             xml2::xml_name(parameter_nodes[[i]]))

        #Guess R representation of node, add it to parameter list
        parameters <- c(parameters, list(node_to_object(parameter_nodes[[i]],
                                                        new_xpath)))

        #Name parameter after the xml_node child name
        names(parameters)[[length(parameters)]] <-
            xml2::xml_name(parameter_nodes[[i]])
    }

    tag_name <- xml2::xml_name(xml_node)

    class_name <- get_class_from_xpath(xpath)

    ordered_parameters <- order_parameters(parameters, class_name)

    param_call_strs <- lapply(names(parameters), function(x){
        return(invisible(paste0("parameters[[\"", x, "\"]]")))
    })

    #Construct the call to the r2ogs6_object helper
    class_constructor_call <-
        paste0(class_name,
               ifelse(grepl("OGS6", class_name), "$new", ""),
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



#'node_to_object
#'@description Returns representation of an XML node. This is a recursive
#' function.
#'ASSUMPTIONS:
#'1) Leaf nodes will never be r2ogs6_* objects
#'2) If there are multiple occurrences of r2ogs6_* class (and subclass)
#' elements on the same level, they have a wrapper node as their parent
#' (e.g. <processes>, <properties>) which  will contain ONLY elements of this
#' type
#'3) Wrapper nodes are represented as lists
#'4) Parent nodes whose children have no children are represented as lists
#'@param xml_node xml2::xml_node: XML node
#'@param xpath string: XPath expression (for subclass differentiation)
node_to_object <- function(xml_node,
                           xpath = ""){

    assertthat::assert_that("xml_node" %in% class(xml_node))
    assertthat::assert_that(assertthat::is.string(xpath))

    node_name <- xml2::xml_name(xml_node)

    #Node is leaf
    if(length(xml2::xml_children(xml_node)) == 0){

        xml_text_clean <-
            stringr::str_remove_all(xml2::xml_text(xml_node),
                                    "[\n|[:space:]]")

        if(xml_text_clean != "" &&
           length(xml2::xml_attrs(xml_node)) != 0){
            return(invisible(c(xml2::xml_attrs(xml_node),
                               xml_text = xml2::xml_text(xml_node))))
        }

        if(xml_text_clean != ""){
            return(invisible(xml2::xml_text(xml_node)))
        }

        return(invisible(xml2::xml_attrs(xml_node)))
    }

    #Node is represented by subclass
    if(!is.null(get_class_from_xpath(xpath))){
        return(invisible(node_to_r2ogs6_class_object(xml_node,
                                                     xpath)))
    }

    #Node has children but is not represented by subclass
    wrapper_list <- list()

    for (i in seq_len(length((xml2::xml_children(xml_node))))) {
        child_node <- xml2::xml_children(xml_node)[[i]]
        child_name <- xml2::xml_name(child_node)

        list_content <- NULL

        new_xpath <- paste0(xpath,
                            "/",
                            child_name)

        if (!is.null(get_class_from_xpath(new_xpath))) {
            list_content <- node_to_r2ogs6_class_object(child_node,
                                                        new_xpath)
        } else{
            list_content <- node_to_object(child_node,
                                           new_xpath)
        }

        wrapper_list <- c(wrapper_list, list(list_content))
        names(wrapper_list)[[length(wrapper_list)]] <- child_name
    }

    return(invisible(wrapper_list))
}


#'get_class_args
#'@description Gets class arguments
#'@param class_name string: The name of a class
#'@return character: Named vector of class arguments
get_class_args <- function(class_name){

    assertthat::assert_that(assertthat::is.string(class_name))

    formals_call <- ifelse(grepl("OGS6", class_name, fixed = TRUE),
                           paste0(class_name,
                                  "$public_methods$initialize"),
                           class_name)

    class_args <- names(as.list(formals(eval(parse(text = formals_call)))))

    return(invisible(class_args))
}


#'order_parameters
#'@description Orders a list of parameters corresponding to the argument order
#' of a class
#'@param parameters list: Parameters
#'@param class_name string: The name of a class
#'@return list: Parameters ordered by argument order of class
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
