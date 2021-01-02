
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

    # Export the simulation files
    export_gml(ogs6_obj$gml, ogs6_obj$sim_path)
    export_prj(ogs6_obj)

    # Copy all referenced .vtu files to ogs6_obj$sim_path
    for(i in seq_len(length(ogs6_obj$meshes))){
        file.copy(ogs6_obj$meshes[[i]], ogs6_obj$sim_path)
    }

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

        exit_code <- system2(command = ogs6_command_str,
                             args = ogs6_args,
                             stdout = ogs6_obj$logfile)
    } else{
        exit_code <- system2(command = ogs6_command_str,
                             args = ogs6_args)
    }

    closeAllConnections()

    return(invisible(exit_code))
}


#===== Validation utility =====


#'validate_all
#'@description Validates all necessary parameters
#'@param ogs6_obj A OGS6 class object
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


#'get_default_benchmark_path
#'@description Utility function for testing, change this to fit your system!
get_default_benchmark_path <- function(){

    default_benchmark_path <-
        "D:/Programme/OpenGeoSys/ogs-master-Tests-Data/Tests/Data/"

    return(default_benchmark_path)
}


#'get_default_ogs_bin_path
#'@description Utility function for testing, change this to fit your system!
get_default_ogs_bin_path <- function(){

    default_ogs_bin_path <-
        paste0("D:/Programme/OpenGeoSys/",
               "ogs-6.3.2-Windows-10.0.14393-x64-python-3.7.2-de-utils",
               "/bin/")

    return(default_ogs_bin_path)
}


#'run_benchmark
#'@description Utility function for quick benchmark runs
#'@param prj_path string:
#'@param ogs_bin_path string:
#'@param sim_path string:
run_benchmark <- function(prj_path,
                          ogs_bin_path,
                          sim_path = "D:/OGS_all_simulations/"){

    if(missing(ogs_bin_path) ||
       !assertthat::is.string(ogs_bin_path) ||
       ogs_bin_path == ""){
        ogs_bin_path <- get_default_ogs_bin_path()
    }

    assertthat::assert_that(assertthat::is.string(prj_path))
    assertthat::assert_that(assertthat::is.string(sim_path))

    sim_path <- validate_is_dir_path(sim_path)

    sim_name <- substr(basename(prj_path),
                       start = 0,
                       stop = nchar(basename(prj_path)) - 4)

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
                read_in_vtu = FALSE)

    return(invisible(run_simulation(ogs6_obj)))
}


#'run_all_benchmarks
#'@description Utility function, for quick benchmark runs. Calls
#' run_benchmark internally.
#'@param path string:
#'@param ogs_bin_path string:
#'@param sim_path string:
#'@param starting_from_prj_path string:
#'@param print_failed_prj_paths flag:
run_all_benchmarks <- function(path,
                               ogs_bin_path,
                               sim_path = "D:/OGS_all_simulations/",
                               starting_from_prj_path = "",
                               print_failed_prj_paths = TRUE){

    if(missing(path) ||
       !assertthat::is.string(path) ||
       path == ""){
        path <- get_default_benchmark_path()
    }

    if(missing(ogs_bin_path) ||
       !assertthat::is.string(ogs_bin_path) ||
       ogs_bin_path == ""){
        ogs_bin_path <- get_default_ogs_bin_path()
    }

    assertthat::assert_that(assertthat::is.flag(print_failed_prj_paths))
    assertthat::assert_that(assertthat::is.string(starting_from_prj_path))

    prj_paths <- list.files(path = path, pattern = ".prj", recursive = TRUE)


    if(length(prj_paths) == 0) {
        stop(paste("No .prj files found in path", path), call. = FALSE)
    }

    # If we know the benchmarks up to a specific file are working, skip them
    if(starting_from_prj_path != ""){

        found_starting_path <- FALSE

        for(i in seq_len(length(prj_paths))){
            if(prj_paths[[i]] == starting_from_prj_path){
                prj_paths <- prj_paths[i : length(prj_paths)]
                found_starting_path <- TRUE
                break
            }
        }

        if(!found_starting_path){
            warning(paste("Couldn't find .prj path to start from.",
                          "Running all benchmarks in 'path'"),
                    call. = FALSE)
        }
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


#===== Chaining utility (WIP) =====


#'read_in_output
#'@description After a OGS6 simulation was run, reads in the generated .vtu
#' files as new input for
#' the .prj file
#'@param ogs6_obj A OGS6 class object
read_in_output <- function(ogs6_obj) {

    #....WIP
}




