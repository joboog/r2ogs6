
#===== XML validation =====


#' validate_read_in_xml
#' @description Utility function, tries parsing the provided file as an XML
#' document
#' @param path string: A file to be parsed as XML
#' @return The parsed XML file (as class object of type
#' \code{xml2::xml_document})
#' @noRd
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


#' validate_read_include
#' @description Utility function, tries to parse the provided file as XML or
#' reads in as string and converts to XML.
#' @param path string: A file to be parsed.
#' @param parent_name string: Name of the parent node that has to be added
#' in case \code{path} is read in as string and converted to XML.
#' @return The parsed file as class object of type \code{xml2::xml_document}).
#' @noRd
validate_read_include <- function(path, parent_name){

    assertthat::assert_that(assertthat::is.string(path))

    incld_xml <- tryCatch(
        {
            return(invisible(xml2::read_xml(path, encoding="ISO-8859-1")))
        },
        error = function(e){

            incld_str <- suppressWarnings(readLines(path))
            incld_str <- paste(c(paste0("<", parent_name, ">"), incld_str,
                                 paste0("</", parent_name, ">")),
                               collapse = " ")
            incld_xml <- xml2::read_xml(incld_str)
            incld_xml <- xml2::xml_children(incld_xml)
            return(invisible(incld_xml))
        }
    )
}

#===== General read_in utility =====


#' read_in
#' @description Reads in elements from a file
#' @param ogs6_obj A OGS6 class object
#' @param xml_doc A xml_document class object.
#' @param xpath string: An XPath expression (should be absolute!)
#' @noRd
read_in <- function(ogs6_obj,
                    xml_doc,
                    xpath){

    assertthat::assert_that("OGS6" %in% class(ogs6_obj))
    assertthat::assert_that("xml_document" %in% class(xml_doc))
    #xml_doc <- validate_read_in_xml(path)

    assertthat::assert_that(assertthat::is.string(xpath))

    split_path <- unlist(strsplit(xpath, "/", fixed = TRUE))
    child_name <- split_path[[length(split_path)]]

    nodes <- xml2::xml_find_all(xml_doc, xpath)

    if(length(nodes) == 0){
        return(invisible(NULL))
    }

    # Remove root expression for better readability
    xpath <- stringr::str_remove(xpath, "\\/[A-Za-z_]*\\/")

    prj_obj <- NULL

    #Parse all children
    for (i in seq_len(length(nodes))) {

        prj_obj <- node_to_prj_class_object(nodes[[i]],
                                                  xpath)

        #Add prj_obj with code snippet
        eval(parse(text = "ogs6_obj$add(prj_obj)"))
    }

    return(invisible(prj_obj))
}


#' node_to_prj_class_object
#' @description Takes an XML node and turns it into a class object
#' @param xml_node xml2::xml_node: XML node
#' @param xpath string: XPath expression (for subclass differentiation)
#' @noRd
node_to_prj_class_object <- function(xml_node,
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

    class_name <- get_class_from_xpath(xpath)

    ordered_parameters <- order_parameters(parameters, class_name)

    param_call_strs <- character()
    seen <- numeric()

    for(i in seq_len(length(ordered_parameters))){

        name <- names(ordered_parameters)[[i]]

        if(length(ordered_parameters[names(ordered_parameters) == name]) == 1){
            param_call_str <- paste0("ordered_parameters[[\"", name, "\"]]")
        }else{

            if(!name %in% names(seen)){
                seen[[name]] <- 1
            }

            param_call_str <-
                paste0("ordered_parameters[names(ordered_parameters) == \"",
                       name, "\"][[",
                       seen[[name]], "]]")

            seen[[name]] <- seen[[name]] + 1
        }

        param_call_strs <- c(param_call_strs, param_call_str)
    }

    #Construct the call to the prj_object helper
    class_constructor_call <-
        paste0(class_name,
               ifelse(grepl("OGS6", class_name), "$new", ""),
               "(",
               paste(
                   names(ordered_parameters),
                   param_call_strs,
                   sep = " = ",
                   collapse = ", "
               ),
               ")")

    #Evaluate the constructed call
    prj_obj <- eval(parse(text = class_constructor_call))

    return(invisible(prj_obj))
}



#' node_to_object
#' @description Returns representation of an XML node. This is a recursive
#' function.
#' ASSUMPTIONS:
#' 1) Leaf nodes will never be prj_* objects
#' 2) Wrapper nodes are represented as lists
#' 3) Parent nodes whose children have no children are represented as lists
#' @param xml_node xml2::xml_node: XML node
#' @param xpath string: XPath expression (for subclass differentiation)
#' @noRd
node_to_object <- function(xml_node,
                           xpath = ""){

    assertthat::assert_that("xml_node" %in% class(xml_node))
    assertthat::assert_that(assertthat::is.string(xpath))

    node_name <- xml2::xml_name(xml_node)

    # joboog: I think this if statements should be combined to if-else
    # statements to be more explicit and to catch errors easier.
    #Node is leaf
    if(length(xml2::xml_children(xml_node)) == 0){

        xml_attrs <- xml2::xml_attrs(xml_node)
        xml_text <- xml2::xml_text(xml_node)
        xml_text_clean <- stringr::str_trim(xml_text)
        xml_text_clean <-
            stringr::str_remove_all(xml_text_clean, "[\n]")

        if(xml_text_clean != "" && length(xml_attrs) != 0){
            return(invisible(c(xml_attrs, xml_text = xml_text_clean)))
        }
        else if(xml_text_clean != ""){
            return(invisible(xml_text_clean))
        }
        else if(length(xml_attrs) != 0){
            return(invisible(xml_attrs))
        }
        else if(xml_text_clean == "" && length(xml_attrs) == 0){
            warning(paste(c("Tag '", xpath, "' was found empty.")))
            return(NULL)
        }
        else{
            stop(paste0("Unusual case for importing tag: '", xpath, "'."),
                 call. = F)
        }
    }

    #Node is represented by subclass
    if(!is.null(get_class_from_xpath(xpath))){
        return(invisible(node_to_prj_class_object(xml_node,
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
            list_content <- node_to_prj_class_object(child_node,
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


#' get_class_args
#' @description Gets class arguments
#' @param class_name string: The name of a class
#' @return character: Named vector of class arguments
#' @noRd
get_class_args <- function(class_name){

    assertthat::assert_that(assertthat::is.string(class_name))

    formals_call <- ifelse(grepl("OGS6", class_name, fixed = TRUE),
                           paste0(class_name,
                                  "$public_methods$initialize"),
                           class_name)

    class_args <- names(as.list(formals(eval(parse(text = formals_call)))))

    return(invisible(class_args))
}


#' order_parameters
#' @description Orders a list of parameters corresponding to the argument order
#' of a class
#' @param parameters list: Parameters
#' @param class_name string: The name of a class
#' @return list: Parameters ordered by argument order of class
#' @noRd
order_parameters <- function(parameters, class_name){

    assertthat::assert_that(is.list(parameters))
    assertthat::assert_that(assertthat::is.string(class_name))

    ordered_parameters <- list()

    class_args <- get_class_args(class_name)

    # logical vector of length(parameters) to exclude r2ogs6 specific parameters
    standard_parameters <-
        names(parameters) %in%
                  c("xpath", "attr_names", "flatten_on_exp", "unwrap_on_exp")

    #Check for length and value mismatches if class does not have Ellipsis
    if(!"..." %in% class_args){
        assertthat::assert_that(length(parameters[!standard_parameters]) <=
                                    length(class_args))

        for(i in seq_len(length(parameters))){
            # cat("\n", names(parameters)[[i]], "\n")
            assertthat::assert_that(names(parameters)[[i]] %in% class_args,
                                    msg = paste0(names(parameters)[[i]],
                                                 " not in class_args of class ",
                                                 class_name,
                                                 collapse = " "))
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
