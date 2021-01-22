
#===== run_simulation =====


#'run_simulation
#'@description Calls OGS6 object validator functions, exports all necessary
#' files and starts OpenGeoSys6
#'@param ogs6_obj OGS6: Simulation object
#'@param write_logfile flag: Should output be written to a logfile?
#'@export
run_simulation <- function(ogs6_obj, write_logfile = TRUE) {

    assertthat::assert_that(inherits(ogs6_obj, "OGS6"))
    assertthat::assert_that(assertthat::is.flag(write_logfile))

    # Call all validators
    validate_all(ogs6_obj)

    # Create the simulation folder
    if (!dir.exists(ogs6_obj$sim_path)) {
        dir.create(ogs6_obj$sim_path)
    } else{
        if (length(list.files(ogs6_obj$sim_path, all.files = TRUE)) != 0) {
            warning(
                paste0(
                    "The defined sim_path directory '",
                    ogs6_obj$sim_path,
                    "' is not empty. Files may be overwritten."
                ),
                call. = FALSE
            )
        }
    }

    # Export (and / or copy referenced) simulation files
    export_all_sim_files(ogs6_obj)

    # Construct the call
    ogs6_command_str <- paste0(ogs6_obj$ogs_bin_path, "ogs.exe")
    sim_path_full <- paste0(ogs6_obj$sim_path,
                            ogs6_obj$sim_name,
                            ".prj")
    ogs6_args <- c(sim_path_full, "-o", ogs6_obj$sim_path)

    exit_code <- 0

    # Finally, make the system call to start the simulation
    if (write_logfile) {

        # Create logfile directory
        logfile_dir <- paste0(ogs6_obj$sim_path, "logfiles/")

        # Set logfile parameter of simulation object
        ogs6_obj$logfile <- paste0(logfile_dir, ogs6_obj$sim_name, "_log.txt")

        if(!dir.exists(logfile_dir)){
            dir.create(logfile_dir)
        }

        assertthat::assert_that(!file.exists(ogs6_obj$logfile))
        file.create(ogs6_obj$logfile)

        cat("\nRunning sim...\n")

        exit_code <- system2(command = ogs6_command_str,
                             args = ogs6_args,
                             stdout = ogs6_obj$logfile)
    } else{
        exit_code <- system2(command = ogs6_command_str,
                             args = ogs6_args)
    }

    closeAllConnections()

    # Read in generated .pvd file and add it to ogs6_obj
    pvd_path <- paste0(ogs6_obj$sim_path,
                       ogs6_obj$time_loop$output$prefix,
                       ".pvd")
    ogs6_obj$pvd <- OGS6_pvd$new(pvd_path = pvd_path)

    return(invisible(exit_code))
}


#===== Export utility =====


#'export_all_sim_files
#'@description Exports and / or copies all simulation files to
#' `ogs6_obj$sim_path`
#'@param ogs6_obj OGS6: Simulation object
export_all_sim_files <- function(ogs6_obj){

    assertthat::assert_that(inherits(ogs6_obj, "OGS6"))

    if(!is.null(ogs6_obj$gml)){
        export_gml(ogs6_obj$gml,
                   paste0(ogs6_obj$sim_path, basename(ogs6_obj$geometry)))
    }else if(!is.null(ogs6_obj$geometry)){
        file.copy(ogs6_obj$geometry, ogs6_obj$sim_path)
    }

    # If processes tag only contains reference, copy referenced file
    if(names(ogs6_obj$processes)[[1]] == "include"){

        include_dir <- paste0(ogs6_obj$sim_path, "include/")

        if(!dir.exists(include_dir)){
            dir.create(include_dir)
        }

        file.copy(ogs6_obj$processes[[1]][["file"]], include_dir)

        new_ref_path <- paste0(include_dir,
                               basename(ogs6_obj$processes[[1]][["file"]]))

        ogs6_obj$processes <- new_ref_path
    }

    # Copy all referenced .vtu files to ogs6_obj$sim_path
    lapply(ogs6_obj$meshes, function(x){
        file.copy(x, ogs6_obj$sim_path)
    })

    if(!is.null(ogs6_obj$python_script)){
        file.copy(ogs6_obj$python_script, ogs6_obj$sim_path)
    }

    export_prj(ogs6_obj)

    return(invisible())
}


#===== Validation utility =====


#'validate_all
#'@description Validates all necessary parameters
#'@param ogs6_obj OGS6: Simulation object
validate_all <- function(ogs6_obj) {

    if(!ogs6_obj$get_status(print_status = FALSE)){
        stop("There are some components missing from your OGS6 object.",
             call. = FALSE)
    }

    if(is.null(ogs6_obj$gml)){
        if(length(ogs6_obj$meshes) < 2){
            stop(paste("If you don't want to specify a gml object, you must",
                       "have multiple meshes. You can define more by calling",
                       "generate_structured_mesh()."), call. = FALSE)
        }
    }else if(length(ogs6_obj$meshes) != 1){
        stop(paste("If you want to specify a gml object, there may only be",
                   "one mesh (one vtk file)."), call. = FALSE)
    }

    #...
}


#===== Test benchmarks (not in tests because of paths) =====


#'run_benchmark
#'@description Utility function for quick benchmark runs
#'@param prj_path string:
#'@param ogs_bin_path string:
#'@param sim_path string: Path where simulation files will be saved.
run_benchmark <- function(prj_path,
                          ogs_bin_path,
                          sim_path){

    if(missing(ogs_bin_path)){
        ogs_bin_path <- unlist(options("r2ogs6.default_ogs_bin_path"))
    }

    if(missing(sim_path)){
        sim_path <- unlist(options("r2ogs6.default_sim_path"))
    }

    assertthat::assert_that(assertthat::is.string(prj_path))
    assertthat::assert_that(assertthat::is.string(ogs_bin_path))
    assertthat::assert_that(assertthat::is.string(sim_path))

    sim_path <- validate_is_dir_path(sim_path)

    sim_name <- tools::file_path_sans_ext(basename(prj_path))

    sim_subdir_path <- paste0(sim_path,
                              basename(dirname(prj_path)),
                              "_",
                              sim_name)

    ogs6_obj <- OGS6$new(sim_name = sim_name,
                         sim_id = 1,
                         sim_path = sim_subdir_path,
                         ogs_bin_path = ogs_bin_path)

    read_in_prj(ogs6_obj = ogs6_obj,
                prj_path = prj_path,
                read_in_vtu = FALSE,
                read_in_gml = TRUE)

    return(invisible(run_simulation(ogs6_obj)))
}


#'run_all_benchmarks
#'@description Utility function, for quick benchmark runs. Calls
#' run_benchmark internally.
#'@param path string:
#'@param ogs_bin_path string:
#'@param sim_path string:
#'@param starting_from_prj_path string: .prj path to start from
#'@param print_failed_prj_paths flag: Output paths where `read_in_prj()` failed?
run_all_benchmarks <- function(path,
                               ogs_bin_path,
                               sim_path,
                               starting_from_prj_path = "",
                               print_failed_prj_paths = TRUE){

    if(missing(path)){
        path <- unlist(options("r2ogs6.default_benchmark_path"))
    }

    if(missing(ogs_bin_path)){
        ogs_bin_path <- unlist(options("r2ogs6.default_ogs_bin_path"))
    }

    if(missing(sim_path)){
        sim_path <- unlist(options("r2ogs6.default_sim_path"))
    }

    assertthat::assert_that(assertthat::is.string(path))
    assertthat::assert_that(assertthat::is.string(ogs_bin_path))
    assertthat::assert_that(assertthat::is.string(sim_path))
    assertthat::assert_that(assertthat::is.string(starting_from_prj_path))
    assertthat::assert_that(assertthat::is.flag(print_failed_prj_paths))

    prj_paths <- list.files(path = path,
                            pattern = "\\.prj$",
                            recursive = TRUE,
                            full.names = TRUE)


    if(length(prj_paths) == 0) {
        stop(paste("No .prj files found in path", path), call. = FALSE)
    }

    # If we know the benchmarks up to a specific file are working, skip them
    if(starting_from_prj_path != ""){
        prj_paths <- get_path_sublist(prj_paths, starting_from_prj_path)
    }

    # Filter invalid .prj files
    invalid_prj_paths <- character()

    for(i in seq_len(length(prj_paths))){
        out <- tryCatch(
            {
                xml2::read_xml(prj_paths[[i]],
                               encoding="ISO-8859-1")
            },

            error = function(cond){
                invalid_prj_paths <<- c(invalid_prj_paths, prj_paths[[i]])
                prj_paths <<- prj_paths[-i]
            }
        )
    }

    # Read in valid .prj files and run simulations
    exit_codes <- numeric()
    failed_prj_paths <- character()

    for(i in seq_len(length(prj_paths))){

        cat("\nAttempting to run Benchmark", prj_paths[[i]])

        exit_code <- run_benchmark(prj_path = prj_paths[[i]],
                                   ogs_bin_path = ogs_bin_path,
                                   sim_path = sim_path)

        exit_codes <- c(exit_codes, exit_code)

        if(exit_code != 0){
            failed_prj_paths <- c(failed_prj_paths, prj_paths[[i]])
        }
    }

    if(print_failed_prj_paths){
        cat("\nFailed .prj paths:\n",
            paste(failed_prj_paths, collapse = "\n"),
            "\n")
    }

    return(invisible(list(invalid_prj_paths = invalid_prj_paths,
                          exit_codes = exit_codes,
                          failed_prj_paths = failed_prj_paths)))
}
