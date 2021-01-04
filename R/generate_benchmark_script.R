
#===== generate_all_benchmarks =====


#'generate_all_benchmark_scripts
#'@description Wrapper function to generate benchmark scripts from all .prj
#' files in a directory
#'@param path string: Path to a benchmark directory to generate scripts from
#'@param scripts_path string: Optional: Path where benchmark scripts will be
#' saved. Change this to fit your system!
#'@param starting_from_prj_path string: Optional:
#'@param skip_prj_paths character: Optional: .prj paths to skip
generate_all_benchmark_scripts <-
    function(path,
             scripts_path = "D:/OGS_scripts/",
             starting_from_prj_path = "",
             skip_prj_paths = character()){

    if(missing(path) ||
       !assertthat::is.string(path) ||
       path == ""){
        path <- get_default_benchmark_path()
    }

    path <- validate_is_dir_path(path)
    scripts_path <- validate_is_dir_path(scripts_path)
    assertthat::assert_that(assertthat::is.string(starting_from_prj_path))
    assertthat::assert_that(is.character(skip_prj_paths))

    prj_paths <- list.files(path = path,
                            pattern = "\\.prj$",
                            recursive = TRUE,
                            full.names = TRUE)

    # If we know the benchmarks up to a specific file are working, skip them
    if(starting_from_prj_path != ""){
        prj_paths <- get_path_sublist(prj_paths, starting_from_prj_path)
    }

    invalid_xml_paths <- character()

    for(i in seq_len(length(prj_paths))){

        if(prj_paths[[i]] %in% skip_prj_paths){
            next
        }

        skip_to_next <- FALSE
        invalid_xml_path <- ""

        out<- tryCatch(
            {
                xml2::read_xml(prj_paths[[i]],
                               encoding="ISO-8859-1")
            },
            error = function(cond){
                skip_to_next <<- TRUE
                invalid_xml_path <<- prj_paths[[i]]
            }
        )

        if(skip_to_next){
            invalid_xml_paths <- c(invalid_xml_paths, invalid_xml_path)
            next
        }

        cat("\nGenerating script from path", prj_paths[[i]])

        generate_benchmark_script(prj_paths[[i]], scripts_path)
    }

    cat("\nFailed parsing the following files:")
    print(invalid_xml_paths)

    return(invisible())
}


#===== generate_benchmark_script =====


#'generate_benchmark_script
#'@description Generates a benchmark script from an existing .prj file.
#'@param prj_path The path to the project file the script will be based on
#'@param script_path string: Optional: Path where benchmark script will be
#' saved. Change this to fit your system!
#'@export
generate_benchmark_script <- function(prj_path,
                                      script_path = "D:/OGS_scripts/") {

    assertthat::assert_that(assertthat::is.string(prj_path))
    assertthat::assert_that(assertthat::is.string(script_path))

    #Construct an object from a benchmark and then reverse engineer the call
    ogs6_obj <- OGS6$new(sim_name = "",
                         sim_id = 1,
                         sim_path = "",
                         ogs_bin_path = "",
                         test_mode = TRUE)

    read_in_prj(ogs6_obj, prj_path)

    impl_classes = get_implemented_classes()

    sim_name <- tools::file_path_sans_ext(basename(prj_path))

    script_str <- paste0("library(r2ogs6)\n\n",
                         "ogs6_obj <- OGS6$new(sim_name = \"",
                         sim_name, "\",\n",
                         "sim_id = 1,\n",
                         "sim_path = \"your_sim_path\",\n",
                         "ogs_bin_path = \"your_bin_path\")\n\n\n")

    for(i in seq_len(length(impl_classes))){
        get_component_call <- paste0("ogs6_obj$", names(impl_classes)[[i]])
        ogs6_component <- eval(parse(text = get_component_call))

        #If benchmark doesn't have components of specified name, skip
        if(is.null(ogs6_component) || length(ogs6_component) == 0){
            next
        }

        #If objects are not in wrapper list, wrap them up for seq_along()
        if(any(grepl("r2ogs6_", class(ogs6_component), fixed = TRUE))){
            ogs6_component <- list(ogs6_component)
        }

        for(j in seq_along(ogs6_component)){
            add_call_str <- paste0(construct_add_call(ogs6_component[[j]]),
                                   "\n\n")
            script_str <- paste0(script_str, add_call_str)
        }
    }

    script_str <- paste0(script_str, "run_simulation(ogs6_obj)\n")

    #If no destination file was defined, print output to console
    if(script_path != ""){
        if(!dir.exists(script_path)){
            dir.create(script_path, showWarnings = FALSE)
        }

        filename <- paste0(script_path, sim_name, ".R")

        if(file.exists(filename)){
            filename <- paste0(script_path,
                               basename(dirname(prj_path)),
                               "__",
                               sim_name,
                               ".R")

            if(file.exists(filename)){
                warning("\nMultiple .prj files with same name in 'path'\n",
                        call. = FALSE)
            }
        }

        cat(script_str, file = filename)
    }else{
        cat(script_str)
    }

    return(invisible(script_str))
}


#'construct_add_call
#'@description Constructs a call based on an OGS6 component. This is a
#' recursive function, handle with care.
#'@param object An object (numeric, character, list, NULL, OGS6 or r2ogs6 class
#' object)
#'@param nested_call Optional: For recursion purposes, you should leave this as
#' it is.
#'@return A string representing the code with which the component would be added
#' to an OGS6 object
construct_add_call <- function(object, nested_call = FALSE) {

    #For values of type numeric or character, dput will give us usable output
    if(is.character(object) ||
       is.numeric(object)){
        ret_str <- paste(utils::capture.output(dput(object)), collapse="\n")
        return(invisible(ret_str))
    }

    #For NULL values we return "NULL" as string
    if(is.null(object)){
        return("NULL")
    }

    #For r2ogs6 objects we need to use recursion
    if(any(grepl("r2ogs6", class(object), fixed = TRUE)) ||
       any(grepl("OGS6", class(object), fixed = TRUE))){

        class_name <- ""
        formals_call <- ""
        init_prefix <- ""
        use_s3_syntax <- TRUE

        if(any(grepl("r2ogs6", class(object), fixed = TRUE))){
            class_name <- grep("r2ogs6", class(object),
                               fixed = TRUE, value = TRUE)
            # formals_call <- paste0("new_", class_name)
            formals_call <- class_name
        }else{
            class_name <- grep("OGS6", class(object),
                               fixed = TRUE, value = TRUE)
            use_s3_syntax <- FALSE
            formals_call <- paste0(class_name,
                                   "$public_methods$initialize")
            init_prefix <- "$new"
        }

        assertthat::assert_that(length(class_name) == 1)

        tag_name <- paste(utils::tail(unlist(strsplit(class_name, "_")), -1),
                          collapse = "_")

        #Grab helper
        param_names <- names(as.list(formals(eval(parse(text = formals_call)))))

        #Handle Ellipsis if it exists by removing and substituting it
        if("..." %in% param_names){
            param_names <- param_names[param_names != "..."]
            param_names <- c(param_names, object$unwrap_on_exp)
        }

        param_strs <- list()

        for(i in seq_len(length(param_names))){
            get_param_call <- paste0("object$", param_names[[i]])
            param <- eval(parse(text = get_param_call))

            param_str <- construct_add_call(param, TRUE)
            param_strs <- c(param_strs, list(param_str))
        }

        content_str <- paste(param_names,
                             param_strs,
                             sep = " = ",
                             collapse = ",\n")

        ret_str <- paste0(class_name, init_prefix,
                          "(", content_str, ")")

        #If call isn't nested, it has a OGS6$add_* function
        if(!nested_call){
            ret_str <- paste0("ogs6_obj$add_", tag_name, "(", ret_str, ")\n")
        }


        ret_str <- delete_nulls_from_str(ret_str)
        ret_str <- delete_keywords_from_str(ret_str)
        ret_str <- delete_empty_from_str(ret_str)

        return(invisible(ret_str))
    }

    #For tibbles we don't need recursion, but they still require extra handling
    if("tbl_df" %in% class(object)){
        tib_str <- paste(names(object), object, sep = " = ", collapse = ",\n")
        ret_str <- paste0("tibble::tibble(", tib_str, ")")
        return(invisible(ret_str))
    }

    #Positioning is important here - r2ogs6 objects are built on top of lists!
    #If is.list is checked before class, results will not be as intended!

    #For lists we need to use recursion
    if(is.list(object)){

        element_strs <- lapply(object, function(x){construct_add_call(x, TRUE)})

        if(is.null(names(object)) ||
           rlist::list.any(names(object) == "")){
            content_str <- paste(element_strs, collapse = ",\n")
        }else{
            content_str <- paste(names(object),
                                 element_strs,
                                 sep = " = ",
                                 collapse = ",\n")
        }

        ret_str <- paste0("list(", content_str, ")")
        return(invisible(ret_str))
    }
}


#'delete_nulls_from_str
#'@description Utility function to delete "param_name = NULL" from a string,
#' this isn't necessary for functionality of generate_benchmark_script but will
#' make generated scripts way more readable.
#'@param string string
delete_nulls_from_str <- function(string){

    regexp_1 <- ",[\n|[:space:]]?[\\w_]* = NULL"
    regexp_2 <- "[\\w_]* = NULL,[\n|[:space:]]?"

    string <- stringr::str_remove_all(string, regexp_1)
    string <- stringr::str_remove_all(string, regexp_2)

    return(invisible(string))
}


#'delete_empty_from_str
#'@description Utility function to delete "param_name = list()" from a string,
#' this isn't necessary for functionality of generate_benchmark_script but will
#' make generated scripts way more readable.
#'@param string string
delete_empty_from_str <- function(string){

    regexp_1 <- ",[\n|[:space:]]?[\\w_]* = list\\(\\)"
    regexp_2 <- "[\\w_]* = list\\(\\),[\n|[:space:]]?"

    string <- stringr::str_remove_all(string, regexp_1)
    string <- stringr::str_remove_all(string, regexp_2)

    return(invisible(string))
}


#'delete_keywords_from_str
#'@description Utility function to delete keywords from a string,
#' this important because there is a <repeat> tag in <time_loop> and
#' "repeat" is a reserved word in R (extend this function if you find more
#' reserved words)
#'@param string string
delete_keywords_from_str <- function(string){

    string <- stringr::str_remove_all(string, "repeat = ")

    return(invisible(string))
}
