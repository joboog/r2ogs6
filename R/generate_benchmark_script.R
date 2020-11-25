

#'generate_benchmark_script
#'@description Generates a benchmark script from an existing .prj file.
#'@param prj_path The path to the project file the script will be based on
#'@param dest_dir Optional: The directory to write the script to
#'@export
generate_benchmark_script <- function(prj_path, dest_dir = "") {

    assertthat::assert_that(assertthat::is.string(prj_path))
    assertthat::assert_that(assertthat::is.string(dest_dir))

    #We'll construct an object from a benchmark and then work our way back from there
    ogs6_obj <- OGS6$new(sim_name = "",
                         sim_id = 1,
                         sim_path = "",
                         ogs_bin_path = "",
                         test_mode = TRUE)

    read_in_prj(ogs6_obj, prj_path)

    impl_classes = c(meshes = "r2ogs6_mesh",
                     gml = "r2ogs6_gml",
                     processes = "r2ogs6_process",
                     media = "r2ogs6_medium",
                     time_loop = "r2ogs6_time_loop",
                     parameters = "r2ogs6_parameter",
                     curves = "r2ogs6_curve",
                     process_variables = "r2ogs6_process_variable",
                     nonlinear_solvers = "r2ogs6_nonlinear_solver",
                     linear_solvers = "r2ogs6_linear_solver",
                     test_definition = "r2ogs6_vtkdiff")

    sim_name <- tools::file_path_sans_ext(basename(prj_path))

    script_str <- paste0("library(r2ogs6)\n\n",
                         "ogs6_obj <- OGS6$new(sim_name = ", sim_name, ",\n",
                         "sim_id = 1,\n",
                         "sim_path = \"your_sim_path\",\n",
                         "ogs_bin_path = \"your_bin_path\")\n\n\n")

    for(i in seq_len(length(impl_classes))){
        get_component_call <- paste0("ogs6_obj$", names(impl_classes)[[i]])
        ogs6_component <- eval(parse(text = get_component_call))

        #If the benchmark doesn't have any components of the specified name defined, skip
        if(is.null(ogs6_component) || length(ogs6_component) == 0){
            next
        }

        #If objects are not in a wrapper list, wrap them up so seq_along yields desired results
        if(any(grepl("r2ogs6_", class(ogs6_component), fixed = TRUE))){
            ogs6_component <- list(ogs6_component)
        }

        for(j in seq_along(ogs6_component)){
            add_call_str <- paste0(construct_add_call(ogs6_component[[j]]), "\n\n")
            script_str <- paste0(script_str, add_call_str)
        }
    }

    script_str <- paste0(script_str, "run_simulation(ogs6_obj)\n")

    #If no destination file was defined, print output to console
    if(dest_dir != ""){
        if(!dir.exists(dest_dir)){
            dir.create(dest_dir, showWarnings = FALSE)
        }

        cat(script_str, file = paste0(dest_dir, sim_name, ".R"))
    }else{
        cat(script_str)
    }

    return(invisible(script_str))
}


#'construct_add_call
#'@description Constructs a call based on an OGS6 component. This is a recursive function
#' and its feelings are easily hurt. Please handle it with care.
#'@param object An object (numeric, character, list, NULL, or r2ogs6 class object)
#'@param nested_call Optional: For recursion purposes, you should leave this as it is.
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
    if(any(grepl("r2ogs6_", class(object), fixed = TRUE))){
        class_name <- class(object)
        tag_name <- paste(utils::tail(unlist(strsplit(class_name, "_")), -1), collapse = "_")

        #Grab the constructor since the helper might have extra parameters that will be coerced
        param_names <- names(as.list(formals(paste0("new_", class_name))))
        param_strs <- list()

        for(i in seq_len(length(param_names))){
            get_param_call <- paste0("object$", param_names[[i]])
            param <- eval(parse(text = get_param_call))

            param_str <- construct_add_call(param, TRUE)
            param_strs <- c(param_strs, list(param_str))
        }

        content_str <- paste(param_names, param_strs, sep = " = ", collapse = ",\n")

        #If the call is nested, it's a subclass object without an OGS6$add_* function
        if(nested_call){
            ret_str <- paste0(class_name, "(", content_str, ")")
            return(invisible(ret_str))
        }

        ret_str <- paste0("ogs6_obj$add_", tag_name, "(", class_name, "(", content_str, "))\n")
        return(invisible(ret_str))
    }

    #For tibbles we don't need recursion, but they still require extra handling
    if("tbl_df" %in% class(object)){
        tib_str <- paste(names(object), object, sep = " = ", collapse = ",\n")
        ret_str <- paste0("tibble::tibble(", tib_str, ")")
        return(invisible(ret_str))
    }

    #Positioning is very important here since r2ogs6 objects are built on top of lists!
    #If is.list is checked before the class, the results will not be as intended!

    #For lists we need to use recursion
    if(is.list(object)){

        element_strs <- lapply(object, function(x){construct_add_call(x, TRUE)})

        if(is.null(names(object)) ||
           rlist::list.any(names(object) == "")){
            content_str <- paste(element_strs, collapse = ",\n")
        }else{
            content_str <- paste(names(object), element_strs, sep = " = ", collapse = ",\n")
        }

        ret_str <- paste0("list(", content_str, ")")
        return(invisible(ret_str))
    }
}
