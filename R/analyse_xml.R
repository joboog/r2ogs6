#This is a helper function which can be used to combat missing documentation.

#Benchmark HydroMechanics:
# analyse_xml("D:\\Programme\\OpenGeoSys\\ogs-master-Tests-Data/",
#             "\\.vtu$",
#             "VTKFile")

#'analyse_xml
#'@description This is a helper function which can be used to combat missing
#' documentation. It looks for files in a path which match the given pattern
#' and then attempts to parse them as XML. For each occurence of the specified
#' element, it documents its attributes and direct children and prints a little
#' summary of its findings at the end.
#'@param path string: A path
#'@param pattern string: A regex pattern
#'@param element_name string: The name of the XML element to look for
#'@param xpath_prefix string: Optional: The XPath prefix to use
#' (defaults to "//")
#'@param print_findings Optional: Should the results be printed to the console?
analyse_xml <- function(path,
                        pattern,
                        element_name,
                        xpath_prefix = "//",
                        print_findings = TRUE) {

    assertthat::assert_that(assertthat::is.string(path))
    assertthat::assert_that(assertthat::is.string(pattern))
    assertthat::assert_that(assertthat::is.string(element_name))
    assertthat::assert_that(assertthat::is.string(xpath_prefix))

    xml_files <- list.files(path = path, pattern = pattern, recursive = TRUE)

    files_found <- length(xml_files)

    valid_files_count <- 0
    valid_files_names <- character()

    element_found_files_names <- character()

    invalid_files_count <- 0
    invalid_files_names <- character()

    total_matches <- 0

    attr_ex_counts <- list()
    child_ex_counts <- list()
    child_tot_counts <- list()

    #Return values, will contain a named list of flags
    attr_flags <- list()
    child_flags <- list()

    if(length(xml_files) == 0) {
        stop(paste("No files found for pattern ", pattern), call. = FALSE)
    }

    for(i in seq_len(length(xml_files))){

        skip_to_next <- FALSE

        out<- tryCatch(
            {
                xml_doc <- xml2::read_xml(paste0(path, xml_files[[i]]),
                                          encoding="ISO-8859-1")
            },

            error = function(cond){
                invalid_files_count <<- invalid_files_count + 1
                invalid_files_names <<- c(invalid_files_names,
                                          basename(xml_files[[i]]))
                skip_to_next <<- TRUE
            }
        )

        if(skip_to_next){
            next
        }

        valid_files_count <- valid_files_count + 1
        valid_files_names <- c(valid_files_names, basename(xml_files[[i]]))

        xpath_exp <- paste0(xpath_prefix, element_name)

        doc_matches <- xml2::xml_find_all(xml_doc, xpath_exp)
        total_matches <- total_matches + length(doc_matches)

        if(length(doc_matches) > 0){
            element_found_files_names <- c(element_found_files_names,
                                           xml_files[[i]])
        }

        #Get attribute names and counts
        for (j in seq_len(length(doc_matches))){
            attr_names <- names(xml2::xml_attrs(doc_matches[[j]]))

            for(k in seq_len(length(attr_names))) {
                if(!attr_names[[k]] %in% names(attr_ex_counts)) {
                    attr_ex_counts[[attr_names[[k]]]] <- 1
                }else{
                    attr_ex_counts[[attr_names[[k]]]] <-
                        attr_ex_counts[[attr_names[[k]]]] + 1
                }
            }
        }

        #Get child names and counts
        for (j in seq_len(length(doc_matches))) {
            children <- xml2::xml_children(doc_matches[[j]])

            first_found <- list()

            for (k in seq_len(length(children))) {
                child_name <- xml2::xml_name(children[[k]])

                if (!child_name %in% first_found) {
                    first_found <- c(first_found, child_name)

                    if (!child_name %in% names(child_ex_counts)) {
                        child_ex_counts[[child_name]] <- 1
                    } else{
                        child_ex_counts[[child_name]] <-
                            child_ex_counts[[child_name]] + 1
                    }
                }

                if (!child_name %in% names(child_tot_counts)) {
                    child_tot_counts[[child_name]] <- 1
                } else{
                    child_tot_counts[[child_name]] <-
                        child_tot_counts[[child_name]] + 1
                }
            }
        }
    }

    make_df_vector <- function(x){
        round((x / total_matches), 2)
    }

    #Turn attribute data into a nice data frame

    attr_name_count_vector <- unlist(attr_ex_counts, use.names=FALSE)

    attr_name_occ_vector <-
        unlist(lapply (attr_ex_counts, make_df_vector))

    attr_flags <- get_required(names(attr_ex_counts), attr_name_occ_vector)

    attr_df <- data.frame(name = names(attr_ex_counts),
                          occ = attr_name_count_vector,
                          p_occ = attr_name_occ_vector)

    if(length(attr_name_count_vector) > 0){
        attr_df <- attr_df[order(attr_df$p_occ, decreasing = TRUE),]
        rownames(attr_df) <- seq(1, length(attr_name_count_vector))
    }

    #Turn children data into a nice data frame

    child_ex_count_vector <- unlist(child_ex_counts, use.names=FALSE)

    child_ex_occ_vector <-
        unlist(lapply(child_ex_counts, make_df_vector))

    child_tot_count_vector <- unlist(child_tot_counts, use.names=FALSE)

    child_tot_occ_vector <-
        unlist(lapply(child_tot_counts, make_df_vector))

    child_flags <- get_required(names(child_ex_counts), child_ex_occ_vector)

    child_df <- data.frame(name = names(child_ex_counts),
                           ex_occ = child_ex_count_vector,
                           p_occ = child_ex_occ_vector,
                           total = child_tot_count_vector,
                           total_mean = child_tot_occ_vector)

    if(length(child_ex_count_vector) > 0){
        child_df <- child_df[order(child_df$p_occ, decreasing = TRUE),]
        rownames(child_df) <- seq(1, length(child_ex_count_vector))
    }

    if(print_findings) {
        print_analysis_findings(
            invalid_files_count,
            invalid_files_names,
            valid_files_count,
            element_name,
            element_found_files_names,
            total_matches,
            attr_ex_counts,
            attr_df,
            child_ex_counts,
            child_df)
    }

    #Return attributes and children (if found)
    return(invisible(
        list(
            tag_name = element_name,
            children = child_flags,
            attributes = attr_flags
        )
    ))
}


#=== PRINT FUNCTIONALITY FOR analyse_xml ===


print_analysis_findings <- function(invalid_files_count,
                                    invalid_files_names,
                                    valid_files_count,
                                    element_name,
                                    element_found_files_names,
                                    total_matches,
                                    attr_ex_counts,
                                    attr_df,
                                    child_ex_counts,
                                    child_df) {

    if(invalid_files_count > 0) {
        cat("I skipped ", invalid_files_count,
            " malformed XML files matching your pattern:\n", sep = "")
        for(i in seq_len(length(invalid_files_names))){
            cat(invalid_files_names[[i]], "\n")
        }
    }

    cat("\nI parsed ", valid_files_count,
        " valid XML files matching your pattern.\n", sep = "")

    if(length(element_found_files_names) > 0){
        cat("\nI found at least one element named ",
            element_name, " in the following file(s):\n", sep = "")

        for(i in seq_len(length(element_found_files_names))){
            cat(element_found_files_names[[i]], "\n")
        }
    }

    cat("\nIn total, I found ", total_matches,
        " element(s) named ", element_name, ".\n", sep = "")

    if(length(attr_ex_counts) > 0) {
        cat("\nThese are the attributes I found:\n")
        print(attr_df)
    }

    if(length(child_ex_counts) > 0){
        cat("\nThese are the child elements I found:\n")
        print(child_df)
    }

    return(invisible())
}


#=== HELPERS FOR analyse_xml ===


#'get_required
#'@description Helper function to mark required attributes or children from a
#' vector of names and a vector of occurrence probabilities
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
