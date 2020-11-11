#This is a helper function which can be used to combat missing documentation.

#Benchmark HydroMechanics:
#analyse_xml("D:\\Programme\\OpenGeoSys\\ogs-master-Tests-Data-HydroMechanics/", "\\.vtu$", "VTKFile")

#Personal playground:
#analyse_xml("./inst/extdata/flow_free_expansion/playground/", "\\.vtu$", "VTKFile")

#' analyse_xml
#' @description This is a helper function which can be used to combat missing documentation. It looks
#'  for files in a path which match the given pattern and then attempts to parse them as XML.
#'  For each occurence of the specified element, it documents its attributes and direct children and prints a little
#'  summary of its findings at the end.
#' @param path A path
#' @param pattern A regex pattern
#' @param element_name The name of the XML element to look for
analyse_xml <- function(path, pattern, element_name) {

    xml_files <- list.files(path = path, pattern = pattern, recursive = TRUE)

    #Think about input...

    files_found <- length(xml_files)

    valid_files_count <- 0
    valid_files_names <- character()

    element_found_files_names <- character()

    invalid_files_count <- 0
    invalid_files_names <- character()

    total_matches <- 0

    attribute_name_counts <- list()
    child_exists_counts <- list()
    child_total_counts <- list()

    #Return values, will contain a named list of flags (TRUE if required, else FALSE)
    attribute_flags <- list()
    child_flags <- list()


    if(length(xml_files) == 0) {
        stop(paste("No files found for defined pattern ", pattern), call. = FALSE)
    }

    for(i in seq_len(length(xml_files))){
        #If there's invalid XML somewhere comment out the following for debugging:

        skip_to_next <- FALSE

        out<- tryCatch(
            {
                xml_doc <- xml2::read_xml(paste(path, xml_files[[i]], sep = ""), encoding="ISO-8859-1")
            },

            error = function(cond){
                invalid_files_count <<- invalid_files_count + 1
                invalid_files_names <<- c(invalid_files_names, basename(xml_files[[i]]))
                skip_to_next <<- TRUE
            }
        )

        if(skip_to_next){
            next
        }

        valid_files_count <- valid_files_count + 1
        valid_files_names <- c(valid_files_names, basename(xml_files[[i]]))

        doc_matches <- xml2::xml_find_all(xml_doc, paste("//", element_name, sep = ""))
        total_matches <- total_matches + length(doc_matches)

        if(length(doc_matches) > 0){
            element_found_files_names <- c(element_found_files_names, xml_files[[i]])
        }

        #Get attribute names and counts
        for (j in seq_len(length(doc_matches))){
            attr_names <- names(xml2::xml_attrs(doc_matches[[j]]))

            for(k in seq_len(length(attr_names))) {
                if(!attr_names[[k]] %in% names(attribute_name_counts)) {
                    attribute_name_counts[[attr_names[[k]]]] <- 1
                }else{
                    attribute_name_counts[[attr_names[[k]]]] <- attribute_name_counts[[attr_names[[k]]]] + 1
                }
            }
        }

        #Get child names and counts
        for (j in seq_len(length(doc_matches))){

            children <- xml2::xml_children(doc_matches[[j]])

            first_found <- list()

            for(k in seq_len(length(children))) {

                child_name <- xml2::xml_name(children[[k]])

                if(!child_name %in% first_found){

                    first_found <- c(first_found, child_name)


                    if(!child_name %in% names(child_exists_counts)) {
                        child_exists_counts[[child_name]] <- 1
                    }else{
                        child_exists_counts[[child_name]] <- child_exists_counts[[child_name]] + 1
                    }
                }

                if(!child_name %in% names(child_total_counts)) {
                    child_total_counts[[child_name]] <- 1
                }else{
                    child_total_counts[[child_name]] <- child_total_counts[[child_name]] + 1
                }
            }
        }
    }

    if(invalid_files_count > 0) {
        cat("I skipped ", invalid_files_count, " malformed XML files matching your pattern:\n", sep = "")
        for(i in seq_len(length(invalid_files_names))){
            cat(invalid_files_names[[i]], "\n")
        }
    }

    cat("\nI parsed ", valid_files_count, " valid XML files matching your pattern.\n", sep = "")

    cat("\nI found at least one element named ", element_name, " in the following file(s):\n", sep = "")

    for(i in seq_len(length(element_found_files_names))){
        cat(element_found_files_names[[i]], "\n")
    }

    cat("\nIn total, I found ", total_matches, " element(s) named ", element_name, ".\n", sep = "")

    #List attributes

    if(length(attribute_name_counts) > 0) {
        cat("\nThese are the attributes I found:\n")

        attr_name_count_vector <- unlist(attribute_name_counts, use.names=FALSE)
        attr_name_occ_vector <- unlist(lapply(attribute_name_counts, function(x){round((x / total_matches), 2)}))

        attr_df <- data.frame(names(attribute_name_counts),
                              attr_name_count_vector,
                              attr_name_occ_vector)

        names(attr_df) <- c("name", "occ", "P(occ)")
        rownames(attr_df) <- seq(1, length(attribute_name_counts))

        print(attr_df)

        attribute_flags <- get_required(names(attribute_name_counts), attr_name_occ_vector)
    }

    #List child elements

    if(length(child_exists_counts) > 0) {

        cat("\nThese are the child elements I found:\n")

        child_ex_count_vector <- unlist(child_exists_counts, use.names=FALSE)
        child_ex_occ_vector <- unlist(lapply(child_exists_counts, function(x){round((x / total_matches), 2)}))

        child_tot_count_vector <- unlist(child_total_counts, use.names=FALSE)
        child_tot_occ_vector <- unlist(lapply(child_total_counts, function(x){round((x / total_matches), 2)}))

        child_df <- data.frame(names(child_exists_counts),
                               child_ex_count_vector,
                               child_ex_occ_vector,
                               child_tot_count_vector,
                               child_tot_occ_vector)

        names(child_df) <- c("name", "ex_occ", "P(occ)", "total", "total_mean")
        rownames(child_df) <- seq(1, length(child_exists_counts))

        print(child_df)

        child_flags <- get_required(names(child_exists_counts), child_ex_occ_vector)
    }

    #Return attributes and children (if found), else NULL
    return(invisible(list(attribute_flags, child_flags)))
}


#Experimental stuff
generate_from_element <- function(element_name, attrs, children) {


}

generate_class_from_element <- function(element_name, class_name = NULL) {

    #Generate name of new class from element name if class_name is not specified
    if(is.null(class_name)){
        class_name <- snakecase::to_any_case(element_name, "snake")
    }


    generated_class <- structure(list(), class = class_name)

    return(generated_class)
}

generate_validator_from_element <- function() {

}


#cat(generate_as_node_from_element("DataArray", c("SexyAttr")))

#'generate_as_node_from_element
#'@description Generates a method for the generic function as_node based on an XML element
#'@param element_name The name of the XML element to base the function on
#'@param attrs Optional: A list of element attributes
#'@param children Optional: A list of element children
generate_as_node_from_element <- function(element_name,
                                          attrs = NULL,
                                          children = NULL) {

    attrs_snake_names <- sapply(attrs, snakecase::to_any_case, case = "snake")
    children_snake_names <- sapply(children, snakecase::to_any_case, case = "snake")

    element_snake_name <- snakecase::to_any_case(element_name, "snake")

    f_name <- paste0("as_node.", element_snake_name)

    #Is the child a parent itself?
    is_parent = TRUE

    func_str <- paste0(f_name, " <- function(obj) {\n")

    node_name <- paste0(element_snake_name, "_node")

    func_str <- paste0(func_str, "\t", node_name, " <- list(", element_name," = structure(list())\n")

    #Add attributes
    for(i in seq_len(length(attrs))){
        nattr <- attrs_snake_names[[i]]
        attr <- attrs[[i]]

        func_str <- paste0(func_str, "\t", node_name, " <- add_attr(", node_name,
                               ", obj$", nattr, ", '", attr, "')\n")
    }

    #Add children
    for(i in seq_len(length(children))){
        child_snake <- children_snake_names[[i]]
        child <- children[[i]]

        #If a child is a parent, either call as_node or adopt_nodes
        if(is_parent){
            if(1){
                func_str <- paste0(func_str, "\t", node_name, " <- add_child(", node_name,
                               ", obj$", child_snake, ", '", child, "')\n")
            }else{

            }
        }else{
            func_str <- paste0(func_str, "\t", node_name, " <- add_child(", node_name,
                               ", obj$", child_snake, ")\n")
        }

    }

    func_str <- paste0(func_str, "\t", "return(", node_name, ")\n")

    func_str <- paste0(func_str, "}")

    return(invisible(func_str))
}

#============================== HELPERS FOR analyse_xml ================================

#'get_required
#'@description Helper function to mark required attributes or children from a vector of names and a
#' vector of occurrence probabilities
#'@param names A vector of names
#'@param occurence_probabilities A vector of occurrence probabilities
get_required <- function(names, occurence_probabilities){
    required <- list()

    for(i in seq_len(length(names))) {
        if(occurence_probabilities[[i]] != 1) {
            required[[names[[i]]]] <- FALSE
        }else{
            required[[names[[i]]]] <- TRUE
        }
    }

    return(required)
}
