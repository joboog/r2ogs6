

#'compare_xml
#'@description Helper function to check if 2 XML documents have the same
#' content. This is a recursive function. Strictly WIP!
#'@param xml_node_x xml2::xml_node: Node to compare
#'@param xml_node_y xml2::xml_node: Node to compare to
compare_xml <- function(xml_node_x, xml_node_y){

    assertthat::assert_that("xml_node" %in% class(xml_node_x))
    assertthat::assert_that("xml_node" %in% class(xml_node_y))

    x_node_name <- xml2::xml_name(xml_node_x)
    y_node_name <- xml2::xml_name(xml_node_y)

    ret_str <- ""

    # Recursion stops here
    if(length(xml2::xml_children(xml_node_x)) == 0 ||
       length(xml2::xml_children(xml_node_y)) == 0 ){

        if(length(xml2::xml_children(xml_node_x)) !=
           length(xml2::xml_children(xml_node_y))){
            ret_str <- paste0("Child mismatch: <",
                              x_node_name, "> in x has ",
                              length(xml2::xml_children(xml_node_x)),
                              " children while <",
                              y_node_name, " in y has ",
                              length(xml2::xml_children(xml_node_y)), ".\n")
        }

        if(x_node_name != y_node_name){
            ret_str <- paste0(ret_str,
                              "Name mismatch: <", x_node_name, "> in x, <",
                              y_node_name, " in y.\n")
        }

        if(xml2::xml_text(xml_node_x) != xml2::xml_text(xml_node_y)){
            ret_str <- paste0(ret_str,
                              "Text mismatch: \"", xml2::xml_text(xml_node_x),
                              "\" in x, \"",
                              xml2::xml_text(xml_node_x), "\" in y.\n")
        }

        if(xml2::xml_attrs(xml_node_x) != xml2::xml_attrs(xml_node_y)){

            x_node_attrs <- paste(names(xml2::xml_attrs(xml_node_x)),
                             xml2::xml_attrs(xml_node_x),
                             sep = " = ", collapse = ",\n")

            y_node_attrs <- paste(names(xml2::xml_attrs(xml_node_y)),
                                  xml2::xml_attrs(xml_node_y),
                                  sep = " = ", collapse = ",\n")

            ret_str <- paste0(ret_str,
                              "Attribute mismatch: (", x_node_attrs,
                              ") in x, (",
                              y_node_attrs, ") in y.\n")
        }

        return(invisible(ret_str))
    }

    y_children_ordered <- xml2::xml_children(xml_node_y)
    y_child_names <- get_nodeset_names(y_children_ordered)

    x_children_ordered <- xml2::xml_children(xml_node_x)

    if(0){
        cat("Children of <", x_node_name, "> tags are not the same ",
            "for x and y. compare_xml() may not yield all differences. ",
            "Continuing iteration based on first parameter (x).", sep = "")
    }

    for(i in seq_len(length(x_children_ordered))){

        x_child <- x_children_ordered[[i]]

        # If x has a child y doesn't have
        if(xml2::xml_name(x_child) %in% y_child_names){


            ret_str <- paste0(ret_str, compare_xml(x_child))

        }else{
            ret_str <- paste0(ret_str,
                              "<", xml2::xml_name(x_child),
                              "> is a child of <",
                              x_node_name,
                              "> in x but not of <",
                              y_node_name, "> in y.")
        }
    }

    return(invisible(ret_str))
}


# order_nodeset <- function(nodeset){
#
#     nodeset_names <- sort(get_nodeset_names(nodeset))
#     ordered_nodeset <- rep(NULL, length(nodeset_names))
#
#     for(){
#
#     }
#
#
# }


get_nodeset_names <- function(nodeset){

    nodeset_names <- c()

    for(i in seq_along(nodeset)){
        nodeset_names <- c(nodeset_names, xml2::xml_name(nodeset[[i]]))
    }

    return(invisible(nodeset_names))
}

