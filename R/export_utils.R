
#===== to_node =====


#' to_node
#' @description Recursive function to restructure objects so
#' xml2::as_xml_document() function will convert them to the desired XML format
#' @param object An object (so far works for r2ogs6 class objects, strings,
#' numbers, lists and vectors)
#' @param object_name string: Optional: The object name. If not supplied, this
#' function tries to guess the tag name by deparsing the 'object' parameter
#' @param attribute_names Optional: A character vector containing names of
#' attributes or attribute nodes
#' @param flatten_on_exp character: Optional: This is for vectors which will be
#' flattened to a string in XML
#' @param unwrap_on_exp character: Optional: This is for lists which will not
#' be exported to XML
#' @noRd
to_node <- function(object, object_name = "",
                    attribute_names = character(),
                    flatten_on_exp = character(),
                    unwrap_on_exp = character()){

    assertthat::assert_that(is.character(attribute_names))
    assertthat::assert_that(is.character(flatten_on_exp))
    assertthat::assert_that(is.character(unwrap_on_exp))

    if(is.null(object_name) || object_name == ""){
        object_name <- deparse(substitute(object))

        if(any(grep("\\$", object_name))){
            split_name <- unlist(strsplit(object_name, "$", fixed = TRUE))
            object_name <- split_name[[length(split_name)]]
        }
    }

    #Recursion ends here
    if(assertthat::is.string(object) ||
       assertthat::is.number(object) ||
       (is.vector(object) &&
        (object_name %in% flatten_on_exp ||
        object_name %in% attribute_names))){

        if(object_name %in% flatten_on_exp){
                ret_list <- list(list(paste(object, collapse = " ")))
                names(ret_list)[[1]] <- object_name
                return(invisible(ret_list))
        }

        if(object_name %in% attribute_names){

            if(length(object) > 1 ||
               (length(object) == 1 && !is.null(names(object)))){
                attr_node <- list(structure(list()))
                names(attr_node)[[1]] <- object_name

                for(i in seq_len(length(object))){
                    attr(attr_node[[1]], names(object)[[i]]) <- object[[i]]
                }

                return(invisible(attr_node))
            }

            ret_vect <- c(object)
            names(ret_vect)[[1]] <- object_name

            return(invisible(ret_vect))

        }else{
            ret_list <- list(list(object))
            names(ret_list)[[1]] <- object_name

            return(invisible(ret_list))
        }
    }

    #For r2ogs6 classes, we need recursion
    if(any(grepl("prj_", class(object), fixed = TRUE))){

        class_name <- class(object)[[1]]

        param_names <- names(as.list(formals(paste0("new_", class_name))))

        unwrap_on_exp <- character()
        unwrapped_params <- list(structure(list()))

        #This works because r2ogs6 S3 classes are built on lists
        if("unwrap_on_exp" %in% names(object)){
            unwrap_on_exp <- object$unwrap_on_exp

            for(i in seq_len(length(unwrap_on_exp))){
                param_names <- param_names[param_names != unwrap_on_exp[[i]]]
            }
        }

        object_node <- list(structure(list()))
        names(object_node)[[1]] <- get_tag_from_class(class_name)

        # For normal class variables we just get the parameter value
        for(i in seq_len(length(param_names))){
            get_param_call <- paste0("object$", param_names[[i]])
            param_value <- eval(parse(text = get_param_call))

            if(is.null(param_value)){
                next
            }

            param_node <- to_node(param_value,
                                  param_names[[i]],
                                  object$attr_names,
                                  object$flatten_on_exp,
                                  unwrap_on_exp)

            #Handle depending on if it's a child or attribute
            if(is.list(param_node)){
                object_node[[1]][[length(object_node[[1]])+1]] <-
                    param_node
            }else{
                attr(object_node[[1]], names(param_node)[[1]]) <-
                    param_node[[1]]
            }
        }

        # For non-exported wrappers we need to strip a layer
        for(i in seq_len(length(unwrap_on_exp))){
            get_wrapper_call <- paste0("object$", unwrap_on_exp[[i]])
            wrapper <- eval(parse(text = get_wrapper_call))

            for(j in seq_len(length(wrapper))){

                if(is.null(wrapper[[j]])){
                    next
                }

                param_node <- to_node(wrapper[[j]],
                                      unwrap_on_exp[[i]],
                                      object$attr_names,
                                      object$flatten_on_exp,
                                      unwrap_on_exp)

                object_node[[1]][[length(object_node[[1]])+1]] <-
                    param_node
            }
        }

        return(invisible(object_node))
    }

    if(is.list(object) ||
       is.vector(object)){

        object_node <- list(structure(list()))
        names(object_node)[[1]] <- object_name

        for(i in seq_len(length(object))){

            element_name <- names(object)[[i]]

            element_node <- to_node(object[[i]],
                                    element_name,
                                    attribute_names,
                                    unwrap_on_exp)

            #Handle depending on if it's a child or attribute
            if(is.list(element_node)){
                object_node[[1]][[length(object_node[[1]])+1]] <-
                    element_node
            }else{
                attr(object_node[[1]], names(element_node)[[1]]) <-
                    element_node[[1]]
            }
        }

        return(invisible(object_node))
    }
}
