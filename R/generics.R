


#============================== as_node ================================


#'as_node
#'@description S3 generic function, use to turn class data into XML friendly format
#'@param x Any object
#'@param ... In case arguments need to be passed from one method to another
as_node <- function(x, ...) {
    UseMethod("as_node")
}


#'as_node.default
#'@description Default implementation for S3 generic function as_node, to be used with
#' atomic objects. So far only returns the value it was given (will be altered later,
#' at the moment this is needed for the add_children function to work)
#'@param x Any object
#'@param node_name How the node should be called
#'@param node_attrs The attributes of the node (a named vector)
as_node.default <- function(x, node_name, node_attrs = NULL){

    if(!is.null(node_attrs)){
        assertthat::assert_that(is.vector(node_attrs))
        assertthat::assert_that(!is.null(names(node_attrs)))

        for(i in seq_len(length(node_attrs))){
            assertthat::assert_that(names(node_attrs)[[i]] != "")
        }
    }

    node <- list(structure(list(x)))
    names(node)[[1]] <- node_name

    attributes(node[[1]]) <- node_attrs

    return(invisible(node))
}


#'as_node.list
#'@description Implementation for S3 generic function as_node for lists. Works only with
#' named lists containing atomic values and wraps the list so that an element will
#' become an XML child to a parent node. The name of the list
#' (= the name of the new parent node) must be supplied.
#'@param x A named list
#'@param node_name How the node should be called
#'@param attr_flags Which of the list elements should be attributes?
as_node.list <- function(x, node_name, attr_flags = NULL){

    if(is.null(node_name) || node_name == ""){
        stop(paste("as_node.list 'node_name' parameter can't",
                    "be NULL or an empty string."), call. = FALSE)
    }

    node = list(structure(list()))
    names(node)[[1]] <- node_name

    for(i in seq_len(length(x))){
        if(is.null(x[[i]])){
            next
        }

        child_name <- names(x)[[i]]

        if(is.null(child_name) || child_name == ""){
            stop(paste("as_node.list list element names can't be",
                        "NULL or an empty string."), call. = FALSE)
        }

        if(is.atomic(x[[i]])){
            if(!is.null(attr_flags) && x[[i]] %in% attr_flags){
                if(child_name %in% attributes(node[[1]])$names){
                    warning("Overwriting attribute", call. = FALSE)
                }

                attributes(node[[1]])[[child_name]] <- x[[i]]
                next
            }

            node[[1]] <- c(node[[1]], as_node(x[[i]], child_name))
        }else{
            stop(paste("Attempting to call as_node on a list with elements not of",
                       "type atomic!"), call. = FALSE)
        }
    }

    return(invisible(node))
}


#============================== input_add ================================

#'S3 generic function, used to add input of different kinds to a OGS6 class object
#'@param obj A class object (must have implementation for input_add)
#'@param ogs6_obj The OGS6 class object the input should be added to
input_add <- function(obj, ogs6_obj) {

    assertthat::assert_that(inherits(ogs6_obj, "OGS6"))
    UseMethod("input_add")
}
