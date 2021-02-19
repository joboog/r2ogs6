
#===== generate_all_benchmarks =====


#' generate_all_benchmark_scripts
#' @description Wrapper function to generate benchmark scripts from all
#' \code{.prj} files in a directory
#' @param path string: Path to a benchmark directory to generate scripts from
#' @param sim_path string: Path where all simulation files will be saved
#' @param scripts_path string: Path where benchmark scripts will be saved
#' @param read_in_gmls flag: Optional: Should \code{.gml} files just be copied
#' or read in too?
#' @param read_in_vtus flag: Optional: Should \code{.vtu} files just be copied
#' or read in too?
#' @param starting_from_prj_path string: Optional:
#' @param skip_prj_paths character: Optional: \code{.prj} paths to skip
generate_all_benchmark_scripts <-
    function(path,
             sim_path,
             scripts_path,
             read_in_gmls,
             read_in_vtus = FALSE,
             starting_from_prj_path = "",
             skip_prj_paths = character()){

    if(missing(path)){
        path <- unlist(options("r2ogs6.default_benchmark_path"))
    }

    if(missing(sim_path)){
        sim_path <- unlist(options("r2ogs6.default_sim_path"))
    }

    if(missing(scripts_path)){
        scripts_path <- unlist(options("r2ogs6.default_script_path"))
    }

    missing_read_in_gmls <- missing(read_in_gmls)

    path <- as_dir_path(path)
    scripts_path <- as_dir_path(scripts_path)
    assertthat::assert_that(assertthat::is.string(starting_from_prj_path))
    assertthat::assert_that(is.character(skip_prj_paths))
    assertthat::assert_that(assertthat::is.flag(read_in_vtus))

    prj_paths <- list.files(path = path,
                            pattern = "\\.prj$",
                            recursive = TRUE,
                            full.names = TRUE)

    # If we know the benchmarks up to a specific file are working, skip them
    if(starting_from_prj_path != ""){

        if(is.na(match(starting_from_prj_path, prj_paths))){
            warning(paste("Couldn't find path to start from.",
                          "Returning all paths."),
                    call. = FALSE)
        }else{
            start_index <- match(starting_from_prj_path, prj_paths)
            prj_paths <- prj_paths[start_index:length(prj_paths)]
        }
    }

    invalid_xml_paths <- filter_invalid_xml(prj_paths)
    prj_paths <- prj_paths[!prj_paths %in% invalid_xml_paths]

    generate_failed_paths <- character()

    for(i in seq_len(length(prj_paths))){

        if(prj_paths[[i]] %in% skip_prj_paths){
            next
        }

        # cat("\nGenerating script from path", prj_paths[[i]])

        # Put simulations in their own subfolders under sim_path
        sim_subdir <-
            paste0(sim_path,
                   basename(dirname(prj_paths[[i]])), "_",
                   tools::file_path_sans_ext(basename(prj_paths[[i]])))

        out<- tryCatch(
            {
                if(missing_read_in_gmls){
                    ogs6_generate_benchmark_script(prj_path = prj_paths[[i]],
                                              sim_path = sim_subdir,
                                              script_path = scripts_path,
                                              read_in_vtu = read_in_vtus)
                }else{
                    ogs6_generate_benchmark_script(prj_path = prj_paths[[i]],
                                              sim_path = sim_subdir,
                                              script_path = scripts_path,
                                              read_in_gml = read_in_gmls,
                                              read_in_vtu = read_in_vtus)
                }

            },
            error = function(cond){
                message(paste("\nogs6_generate_benchmark_script() failed for",
                              prj_paths[[i]], ". Original error message:"))
                message(cond)
                generate_failed_paths <<-
                    c(generate_failed_paths, prj_paths[[i]])
            }
        )
    }

    return(invisible(list(invalid_xml_paths, generate_failed_paths)))
}


#===== ogs6_generate_benchmark_script =====


#' ogs6_generate_benchmark_script
#' @description Generates a benchmark script from an existing \code{.prj} file.
#' @param prj_path string: \code{.prj} file the script will be based on
#' @param sim_path string: Path where all simulation files will be saved
#' @param ogs6_bin_path string: OpenGeoSys bin folder path
#' @param script_path string: Path where benchmark script will be saved
#' @param read_in_gml flag: Optional: Should \code{.gml} file just be copied or
#'   read in too?
#' @param read_in_vtu flag: Optional: Should \code{.vtu} file(s) just be copied
#'   or read in too?
#' @export
ogs6_generate_benchmark_script <- function(prj_path,
                                           sim_path,
                                           ogs6_bin_path,
                                           script_path,
                                           read_in_gml,
                                           read_in_vtu = FALSE) {

    if(missing(sim_path)){
        sim_path <- unlist(options("r2ogs6.default_sim_path"))
    }

    if(missing(ogs6_bin_path)){
        ogs6_bin_path <- unlist(options("r2ogs6.default_ogs6_bin_path"))
    }

    if(missing(script_path)){
        script_path <- unlist(options("r2ogs6.default_script_path"))
    }

    assertthat::assert_that(assertthat::is.string(prj_path))
    assertthat::assert_that(assertthat::is.string(sim_path))
    assertthat::assert_that(assertthat::is.string(ogs6_bin_path))
    assertthat::assert_that(assertthat::is.string(script_path))
    assertthat::assert_that(assertthat::is.flag(read_in_vtu))

    #Construct an object from a benchmark and then reverse engineer the call
    ogs6_obj <- OGS6$new(sim_name = "",
                         sim_path = "")

    read_in_prj(ogs6_obj,
                prj_path,
                read_in_vtu,
                read_in_gml = FALSE)

    prj_components = prj_top_level_classes()

    sim_name <- tools::file_path_sans_ext(basename(prj_path))

    script_str <- paste0("library(r2ogs6)\n\n",
                         "ogs6_obj <- OGS6$new(sim_name = \"",
                         sim_name, "\",\n",
                         "sim_path = \"", sim_path, "\")\n\n\n")

    # If there is a .gml but it shouldn't be read in, add reference
    if (!is.null(ogs6_obj$geometry)) {

        script_str <- paste0(script_str, "ogs6_obj$add_gml(")

        # If read_in_gml isn't supplied, check number of lines in .gml file
        # since string concatenation is slow
        if(missing(read_in_gml)){
            read_in_gml <- (length(readLines(ogs6_obj$geometry)) <=
                                unlist(options("r2ogs6.max_lines_gml")))
        }

        assertthat::assert_that(assertthat::is.flag(read_in_gml))

        if(!read_in_gml){
            script_str <- paste0(script_str,
                                 construct_add_call(ogs6_obj$geometry,
                                                    nested_call = TRUE))
        }else{
            ogs6_obj$add_gml(OGS6_gml$new(ogs6_obj$geometry))
            script_str <- paste0(script_str,
                                 construct_add_call(ogs6_obj$gml,
                                                    nested_call = TRUE))
        }

        script_str <- paste0(script_str, ")\n\n")
    }

    # Add .vtu references and optionally, OGS6_vtu objects
    for(i in seq_len(length(ogs6_obj$meshes))){
        script_str <- paste0(script_str,
                             "ogs6_obj$add_vtu(",
                             construct_add_call(ogs6_obj$meshes[[i]]), ",\n",
                             read_in_vtu,
                             ")\n\n")
    }

    # Add class objects (and such in wrapper lists)
    for(i in seq_len(length(prj_components))){

        get_component_call <- paste0("ogs6_obj$", names(prj_components)[[i]])
        ogs6_component <- eval(parse(text = get_component_call))

        # If benchmark doesn't have components of specified name, skip
        if(is.null(ogs6_component) || length(ogs6_component) == 0){
            next
        }

        #If objects are not in wrapper list, wrap them up for seq_along()
        if(any(grepl("prj_", class(ogs6_component), fixed = TRUE)) ||
           any(grepl("OGS6_", class(ogs6_component), fixed = TRUE))){
            ogs6_component <- list(ogs6_component)
        }

        for(j in seq_along(ogs6_component)){
            script_str <-
                paste0(script_str,
                       paste0(construct_add_call(ogs6_component[[j]]),
                              "\n\n"))
        }
    }

    script_str <- paste0(script_str,
                         "ogs6_run_simulation(ogs6_obj)\n")

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


#' construct_add_call
#' @description Constructs a call based on an \code{OGS6} component. This is a
#' recursive function, handle with care.
#' @param object An object (numeric, character, list, NULL, \code{OGS6} or
#' \code{r2ogs6} class object)
#' @param nested_call Optional: For recursion purposes, you should leave this as
#' it is.
#' @return A string representing the code with which the component would be
#' added to an \code{OGS6} object
construct_add_call <- function(object, nested_call = FALSE) {

    #For values of type numeric or character, dput will give us usable output
    if(is.character(object) ||
       is.numeric(object)){
        ret_str <- paste(utils::capture.output(dput(object)), collapse="\n")
        return(invisible(ret_str))
    }

    #For NULL values and empty lists, return "NULL" as string
    if(length(object) == 0){
        return("NULL")
    }

    #For r2ogs6 objects we need to use recursion
    if(any(grepl("prj_", class(object), fixed = TRUE)) ||
       any(grepl("OGS6", class(object), fixed = TRUE))){

        class_name <- ""
        formals_call <- ""
        init_prefix <- ""
        use_s3_syntax <- TRUE

        if("R6" %in% class(object)){
            class_name <- grep("OGS6", class(object),
                               fixed = TRUE, value = TRUE)
            use_s3_syntax <- FALSE
            init_prefix <- "$new"
        }else{
            class_name <- grep("prj_", class(object),
                               fixed = TRUE, value = TRUE)
        }

        assertthat::assert_that(length(class_name) == 1)

        tag_name <- paste(utils::tail(unlist(strsplit(class_name, "_")), -1),
                          collapse = "_")

        #Grab helper
        param_names <- get_class_args(class_name)

        #Handle Ellipsis if it exists by removing
        if("..." %in% param_names){
            param_names <- param_names[param_names != "..."]
            param_names <- c(param_names, object$unwrap_on_exp)
        }

        param_strs <- list()

        for(i in seq_len(length(param_names))){

            param <- eval(parse(text = paste0("object$", param_names[[i]])))
            param_str <- construct_add_call(param, TRUE)

            if(param_names[[i]] %in% object$unwrap_on_exp){
                param_str <- stringr::str_remove_all(param_str,
                                                     "(^list\\()|(\\)$)")
            }

            param_strs <- c(param_strs, list(param_str))
        }

        content_str <- paste(param_names,
                             param_strs,
                             sep = " = ",
                             collapse = ",\n")

        ret_str <- paste0(class_name, init_prefix,
                          "(", content_str, ")")

        #If call isn't nested, it can be added
        if(!nested_call){
            ret_str <- paste0("ogs6_obj$add(", ret_str, ")\n")
        }

        regexp_1 <- "(,[:space:])[\\w_]* = ((NULL)|(list\\(\\)))"
        regexp_2 <- "[\\w_]* = ((NULL)|(list\\(\\)))(,[:space:])"
        ret_str <- stringr::str_remove_all(ret_str, regexp_1)
        ret_str <- stringr::str_remove_all(ret_str, regexp_2)
        ret_str <- delete_keywords_from_str(ret_str)
        ret_str <- stringr::str_replace_all(ret_str,
                                            " = [A-Za-z_]* = ", " = ")

        return(invisible(ret_str))
    }

    #For tibbles we don't need recursion, but they still require extra handling
    if("tbl_df" %in% class(object)){
        tib_str <- paste(names(object), object, sep = " = ", collapse = ",\n")
        ret_str <- paste0("tibble::tibble(", tib_str, ")")
        return(invisible(ret_str))
    }

    #For lists we need to use recursion
    if(class(object) == "list"){

        element_strs <- lapply(object, function(x){construct_add_call(x, TRUE)})

        if(is.null(names(object)) ||
           length(object[names(object) == ""]) > 0){
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


#' delete_keywords_from_str
#' @description Utility function to delete keywords from a string,
#' this is important because there is a <repeat> tag in <time_loop> and
#' "repeat" is a reserved word in R (extend this function if you find more
#' reserved words)
#' @param string string
delete_keywords_from_str <- function(string){

    string <- stringr::str_remove_all(string, "repeat = ")

    return(invisible(string))
}
