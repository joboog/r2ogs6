


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
#'@param ... Required: The name of the new node (string), Optional: A list of attributes
as_node.default <- function(x, ...){

    if(is.null(x)){
        return(invisible(NULL))
    }

    node_name <- ""
    params <- list(...)

    if(any(grep("\\$", deparse(substitute(x))))){
        node_name <- strsplit(deparse(substitute(x)), "$", fixed = TRUE)[[2]]
    }else if(length(params) > 0){
        assertthat::assert_that(assertthat::is.string(params[[1]]))
        assertthat::assert_that(params[[1]] != "")
        node_name <- params[[1]]
    }else{
        stop(paste("as_node.default must be given a non-empty string as second parameter",
                   " if its first parameter is not a class parameter",
                   " (e.g. x$some_name, then the parameter will be deparsed and the",
                   " name will automatically become 'some_name')"), call. = FALSE)
    }

    node <- list(structure(list(x)))
    names(node)[[1]] <- node_name

    has_attrs <- length(params) >= 2

    if(has_attrs){
        assertthat::assert_that(is.vector(params[[2]]))
        assertthat::assert_that(!is.null(names(params[[2]])))

        for(i in seq_len(length(params[[2]]))){
            assertthat::assert_that(names(params[[2]])[[i]] != "")
        }

        attributes(node[[1]]) <- params[[2]]
    }

    return(invisible(node))
}


#'as_node.list
#'@description Implementation for S3 generic function as_node for lists. Works only with
#' named lists containing atomic values and wraps the list so that an element will
#' become an XML child to a parent node. The name of the list
#' (= the name of the new parent node) must be supplied.
#'@param x A named list
#'@param ... Required: The name of the new node (string), Optional: A vector of attribute flags
as_node.list <- function(x, ...){

    params <- list(...)

    if(length(params) == 0){
        stop("as_node.list must be given a string as second parameter.", call. = FALSE)
    }

    assertthat::assert_that(assertthat::is.string(params[[1]]))
    node_name <- params[[1]]

    attr_flags <- list()

    if(length(params) >= 2){
        assertthat::assert_that(is.vector(params[[2]]))
        attr_flags <- params[[2]]
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
#'@param x A class object (must have implementation for input_add)
#'@param ogs6_obj The OGS6 class object the input should be added to
#'@export
input_add <- function(x, ogs6_obj) {

    assertthat::assert_that(inherits(ogs6_obj, "OGS6"))
    UseMethod("input_add")
}
