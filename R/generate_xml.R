
#===== build_redux_doc =====


#' build_redux_doc
#' @description Builds an XML document based on the findings of
#' \code{analyse_xml()}. Calls recursive function \code{build_redux_tree()}
#' internally.
#' @param path string: See \code{?analyse_xml}
#' @param pattern string: See \code{?analyse_xml}
#' @param xpath string: See \code{?analyse_xml}
#' @param export_path string: Path to export the XML document to
#' @noRd
build_redux_doc <- function(path,
                            pattern,
                            xpath,
                            export_path){

    if(missing(benchmark_path)){
        benchmark_path <- unlist(options("r2ogs6.default_benchmark_path"))
    }

    assertthat::assert_that(assertthat::is.string(benchmark_path))

    # Default to
    if(missing(pattern) && missing(xpath)){
        pattern <- "\\.prj$"
        xpath <- "/OpenGeoSysProject"
    }

    assertthat::assert_that(assertthat::is.string(pattern))
    assertthat::assert_that(assertthat::is.string(xpath))
    assertthat::assert_that(assertthat::is.string(export_path))

    redux_node <- build_redux_tree(path = benchmark_path,
                                   pattern = pattern,
                                   xpath = xpath,
                                   required = TRUE)

    redux_doc <- xml2::as_xml_document(redux_node)
    xml2::write_xml(redux_doc, export_path)

    return(invisible())
}


#===== build_redux_tree =====


#' build_redux_tree
#' @description Builds an XML tree based on the findings of
#' \code{analyse_xml()}. This is a recursive function.
#' @param path string: See \code{?analyse_xml}
#' @param pattern string: See \code{?analyse_xml}
#' @param xpath string: See \code{?analyse_xml}
#' @param required flag: Recursion utility
#' @noRd
build_redux_tree <- function(path,
                             pattern,
                             xpath,
                             required){

    analysis_results <- analyse_xml(path = path,
                                    pattern = pattern,
                                    xpath = xpath,
                                    print_findings = FALSE)

    xpath <- analysis_results[["xpath"]]
    children <- names(analysis_results[["children"]])
    attr_names <- names(analysis_results[["attributes"]])


    # Create a redux base node
    redux_node <- list(structure(list()))
    names(redux_node) <- get_tag_from_xpath(xpath)
    attributes(redux_node[[1]])$required <- required

    if(length(attr_names) > 0){
        attr_names_str <- paste(attr_names, collapse = " ")
        attributes(redux_node[[1]])$attr_names <- attr_names_str
    }

    # Recursion stops here
    if(length(children) == 0){
        attributes(redux_node[[1]])$read_content_as <- "string"
        return(invisible(redux_node))
    }

    redux_node_attrs <- c(list(read_content_as = "list"),
                          attributes(redux_node[[1]]))

    for(i in seq_len(length(children))){

        new_xpath <- paste0(xpath, "/", children[[i]])
        required <- analysis_results[["children"]][[i]]

        child_redux_node <-
            build_redux_tree(path = path,
                             pattern = pattern,
                             xpath = new_xpath,
                             required = required)

        redux_node[[1]] <- c(redux_node[[1]],
                             list(child_redux_node))
    }

    # Needed because attributes get lost when adding to redux_node[[1]]
    attributes(redux_node[[1]]) <- redux_node_attrs

    return(invisible(redux_node))
}
