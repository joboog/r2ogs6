
#===== ogs6_run_simulation =====


#' ogs6_run_simulation
#' @description Wrapper function that calls \code{ogs6_export_sim_files()},
#'   \code{ogs6_call_ogs6()} and \code{ogs6_read_output_files()}.
#' @param ogs6_obj OGS6: Simulation object
#' @param write_logfile flag: Should output be written to a logfile? If
#'   \code{FALSE}, output will be written to console. If \code{TRUE}, logfile
#'   directory will be created in \code{ogs6$sim_path} directory
#' @param ogs6_bin_path string: Optional: OpenGeoSys 6 bin folder path. Defaults
#'   to \code{options("r2ogs6.default_ogs6_bin_path")}
#' @param verbose flag
#' @export
ogs6_run_simulation <- function(ogs6_obj,
                               write_logfile = TRUE,
                               ogs6_bin_path,
                               verbose = F) {

    # Export (and / or copy referenced) simulation files
    ogs6_export_sim_files(ogs6_obj = ogs6_obj,
                         test_mode = FALSE)

    exit_code <- ogs6_call_ogs6(ogs6_obj = ogs6_obj,
                               write_logfile = write_logfile,
                               ogs6_bin_path = ogs6_bin_path,
                               verbose = verbose)

    ogs6_read_output_files(ogs6_obj = ogs6_obj)

    return(invisible(exit_code))
}


#===== ogs6_export_sim_files =====


#' ogs6_export_sim_files
#' @description Creates \code{ogs6$sim_path} directory if it does not exist yet
#'   and exports and / or copies all simulation files to it.
#' @param ogs6_obj OGS6: Simulation object
#' @param test_mode flag: If \code{TRUE}, Will not check status of
#'   \code{ogs6_obj} before exporting files. Defaults to \code{FALSE}
#' @export
ogs6_export_sim_files <- function(ogs6_obj,
                                 test_mode = FALSE){

    assertthat::assert_that(inherits(ogs6_obj, "OGS6"))
    assertthat::assert_that(assertthat::is.flag(test_mode))

    # Call all validators
    if(!test_mode &&
       !ogs6_obj$get_status(print_status = FALSE)){
        stop("There are some components missing from your OGS6 object.",
             call. = FALSE)
    }

    # Create the simulation folder
    if (!dir.exists(ogs6_obj$sim_path)) {
        dir.create(ogs6_obj$sim_path)
    } else{
        if (length(list.files(ogs6_obj$sim_path)) != 0) {
            warning(
                paste0("sim_path directory '", ogs6_obj$sim_path,
                       "' is not empty. Files may be overwritten."
                ),
                call. = FALSE
            )
        }
    }

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

#===== ogs6_call_ogs6 =====


#' ogs6_call_ogs6
#' @description Makes system call to OpenGeoSys 6 and retrieves exit code.
#' @param ogs6_obj OGS6: Simulation object
#' @param write_logfile flag: Should output be written to a logfile? If
#'   \code{FALSE}, output will be written to console. If \code{TRUE}, logfile
#'   directory will be created in \code{ogs6$sim_path} directory
#' @param ogs6_bin_path string: Optional: OpenGeoSys 6 bin folder path. Defaults
#'   to \code{options("r2ogs6.default_ogs6_bin_path")}
#' @param verbose flag
#' @export
ogs6_call_ogs6 <- function(ogs6_obj,
                          write_logfile = TRUE,
                          ogs6_bin_path,
                          verbose = F){

    assertthat::assert_that(inherits(ogs6_obj, "OGS6"))
    assertthat::assert_that(assertthat::is.flag(write_logfile))

    if(missing(ogs6_bin_path)){
        ogs6_bin_path <- unlist(options("r2ogs6.default_ogs6_bin_path"))
    }

    assertthat::assert_that(assertthat::is.string(ogs6_bin_path))
    assertthat::assert_that(assertthat::is.flag(verbose))

    # Construct the call
    exe_str <- ifelse(Sys.info()["sysname"] == "Windows",
                      "ogs.exe",
                      "ogs")

    ogs6_command_str <- paste0(ogs6_bin_path, exe_str)
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

        if(verbose){
            cat("\nRunning simulation '", ogs6_obj$sim_name, "'\n", sep = "")
        }

        exit_code <- system2(command = ogs6_command_str,
                             args = ogs6_args,
                             stdout = ogs6_obj$logfile)
    } else{
        exit_code <- system2(command = ogs6_command_str,
                             args = ogs6_args)
    }

    return(invisible(exit_code))
}


#===== ogs6_read_output_files =====


#' ogs6_read_output_files
#' @description Read in generated \code{.pvd} files and add it to ogs6_obj
#' @param ogs6_obj OGS6: Simulation object
#' @export
ogs6_read_output_files <- function(ogs6_obj){

    assertthat::assert_that(inherits(ogs6_obj, "OGS6"))

    pvd_paths <- list.files(ogs6_obj$sim_path,
                            "\\.pvd$",
                            full.names = TRUE)

    for(i in seq_len(length(pvd_paths))){
        ogs6_obj$pvds <- c(ogs6_obj$pvds,
                           list(OGS6_pvd$new(pvd_path = pvd_paths[[i]])))
    }

    return(invisible())
}


#===== Test benchmarks =====


#' run_benchmark
#' @description Utility function for quick benchmark runs
#' @param prj_path string:
#' @param ogs6_bin_path string:
#' @param sim_path string: Path where simulation files will be saved
run_benchmark <- function(prj_path,
                          ogs6_bin_path,
                          sim_path){

    if(missing(ogs6_bin_path)){
        ogs6_bin_path <- unlist(options("r2ogs6.default_ogs6_bin_path"))
    }

    if(missing(sim_path)){
        sim_path <- unlist(options("r2ogs6.default_sim_path"))
    }

    assertthat::assert_that(assertthat::is.string(prj_path))
    assertthat::assert_that(assertthat::is.string(ogs6_bin_path))
    assertthat::assert_that(assertthat::is.string(sim_path))

    sim_name <- tools::file_path_sans_ext(basename(prj_path))

    ogs6_obj <- OGS6$new(sim_name = sim_name,
                         sim_id = 1,
                         sim_path = sim_path)

    read_in_prj(ogs6_obj = ogs6_obj,
                prj_path = prj_path)

    return(invisible(ogs6_run_simulation(ogs6_obj,
                                    ogs6_bin_path = ogs6_bin_path)))
}


#' run_all_benchmarks
#' @description Utility function, for quick benchmark runs. Calls
#'   run_benchmark internally.
#' @param path string: Path to benchmark folder
#' @param ogs6_processlib_path string: Path to OpenGeoSys 6 ProcessLib folder
#'   which contains relevant Tests.cmake files
#' @param ogs6_bin_path string:
#' @param sim_path string: Path where simulation files will be saved
#' @param starting_from_prj_path string: \code{.prj} path to start from
#' @param print_results flag: Print results in the end?
run_all_benchmarks <- function(path,
                               ogs6_processlib_path,
                               ogs6_bin_path,
                               sim_path,
                               starting_from_prj_path = "",
                               print_results = TRUE){

    if(missing(path)){
        path <- unlist(options("r2ogs6.default_benchmark_path"))
    }

    if(missing(ogs6_processlib_path)){
        ogs6_processlib_path <-
            unlist(options("r2ogs6.default_ogs6_processlib_path"))
    }

    if(missing(ogs6_bin_path)){
        ogs6_bin_path <- unlist(options("r2ogs6.default_ogs6_bin_path"))
    }

    if(missing(sim_path)){
        sim_path <- unlist(options("r2ogs6.default_sim_path"))
    }

    assertthat::assert_that(assertthat::is.string(path))
    assertthat::assert_that(assertthat::is.string(ogs6_processlib_path))
    assertthat::assert_that(assertthat::is.string(ogs6_bin_path))
    assertthat::assert_that(assertthat::is.string(sim_path))
    assertthat::assert_that(assertthat::is.string(starting_from_prj_path))
    assertthat::assert_that(assertthat::is.flag(print_results))

    # Get relevant .prj paths from ProcessLib Tests.cmake files
    prj_paths <-
        lapply(get_benchmark_paths(ogs6_processlib_path), function(x){
            paste0(path, x)
        })

    assertthat::assert_that(length(prj_paths) > 0)

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


    # Filter nonexisting files from prj_paths
    nonexisting_prj_paths <- prj_paths[!file.exists(prj_paths)]
    prj_paths <- prj_paths[!prj_paths %in% nonexisting_prj_paths]


    # Filter invalid XML from prj_paths
    invalid_xml_paths <- filter_invalid_xml(prj_paths)
    prj_paths <- prj_paths[!prj_paths %in% invalid_xml_paths]

    # Read in valid .prj files and try to run simulations
    failed_paths <- character()
    exit_codes <- numeric()

    for(i in seq_len(length(prj_paths))){

        cat("\nAttempting to run Benchmark", prj_paths[[i]])

        sim_name <- tools::file_path_sans_ext(basename(prj_paths[[i]]))

        sim_subdir_path <- paste0(sim_path,
                                  basename(dirname(prj_paths[[i]])),
                                  "_",
                                  sim_name)

        out<- tryCatch(
            {
                exit_code <- run_benchmark(prj_path = prj_paths[[i]],
                                           ogs6_bin_path = ogs6_bin_path,
                                           sim_path = sim_subdir_path)

                exit_codes <<- c(exit_codes, exit_code)
            },
            error = function(cond){
                message(paste("\nrun_benchmark() failed for",
                              prj_paths[[i]], ". Original error message:"))
                message(cond)
                failed_paths <<- c(failed_paths, prj_paths[[i]])
            }
        )
    }

    run_started_paths <- prj_paths[!prj_paths %in% failed_paths]

    if(print_results){
        print_run_all_benchmarks(
            nonexisting_prj_paths,
            invalid_xml_paths,
            failed_paths,
            run_started_paths,
            exit_codes)
    }

    return(invisible(list(nonexisting_prj_paths = nonexisting_prj_paths,
                          invalid_xml_paths = invalid_xml_paths,
                          failed_paths = failed_paths,
                          run_started_paths = run_started_paths,
                          exit_codes = exit_codes)))
}


print_run_all_benchmarks <- function(nonexisting_prj_paths,
                                     invalid_xml_paths,
                                     failed_paths,
                                     run_started_paths,
                                     exit_codes) {


    if(length(nonexisting_prj_paths) > 0){
        cat("\nCould not find the following .prj files ",
            "referenced in Tests.cmake:\n",
            paste(nonexisting_prj_paths, collapse = "\n"), "\n", sep = "")
    }

    if(length(invalid_xml_paths) > 0){
        cat("\nCould not parse the following .prj files as XML:\n",
            paste(invalid_xml_paths, collapse = "\n"), "\n", sep = "")
    }

    if(length(failed_paths) > 0){
        cat("\nThere was something else wrong with the following .prj files:\n",
            paste(failed_paths, collapse = "\n"), "\n", sep = "")
    }

    if(length(exit_codes) > 0){
        cat("\nOpenGeoSys produced the following exit codes:\n",
            paste(run_started_paths,
                  exit_codes,
                  sep = " produced exit code ",
                  collapse = "\n"), "\n", sep = "")
    }

    return(invisible())
}


#' get_benchmark_paths
#' @description Gets paths to all benchmarks that should work
#' @param ogs6_processlib_path string: Path to OpenGeoSys 6 ProcessLib folder
#'   which contains relevant Tests.cmake files
get_benchmark_paths <- function(ogs6_processlib_path){

    tests_cmake_files <- list.files(path = ogs6_processlib_path,
                                    pattern = "^Tests\\.cmake$",
                                    recursive = TRUE,
                                    full.names = TRUE)

    benchmark_paths <- list()

    for(i in seq_len(length(tests_cmake_files))){

        file_content <- readLines(tests_cmake_files[[i]])
        file_content <- paste(file_content[!grepl("^#", file_content)],
                              collapse = "\n")

        # Get AddTest blocks
        add_test_blocks <-
            stringr::str_extract_all(file_content,
                                     "AddTest\\([^\\)]*\\)",
                                     simplify = TRUE)

        mesh_sizes <- character()

        if(grepl("foreach(mesh_size", file_content, fixed = TRUE)){

            foreach_start <-
                stringr::str_extract(file_content,
                                     "foreach\\(mesh_size[^\\)]*\\)")
            foreach_content <-
                stringr::str_remove_all(foreach_start,
                                        "(foreach\\(mesh_size )|(\\))")

            mesh_sizes <- unlist(strsplit(foreach_content, " "))
        }

        # Get .prj paths from blocks
        for(j in seq_len(length(add_test_blocks))){

            atb <- add_test_blocks[[j]]

            benchmark_dir <- stringr::str_extract(atb,
                                                  "PATH[:space:]*[^ ]*")
            benchmark_dir <-
                stringr::str_remove_all(benchmark_dir,
                                        "^PATH|[:space:]")

            prj_filename <-
                stringr::str_extract(atb,
                                     "EXECUTABLE_ARGS [^ ]*")

            prj_filename <-
                stringr::str_remove_all(prj_filename,
                                        "^EXECUTABLE_ARGS |[:space:]")

            # We just take the first size so far, eventally put loop here later
            if(length(mesh_sizes) != 0){
                prj_filename <- gsub("${mesh_size}",
                                     mesh_sizes[[1]],
                                     prj_filename,
                                     fixed = TRUE)
            }

            benchmark_path <- paste0(benchmark_dir, "/", prj_filename)

            # cat("Benchmark path: ", benchmark_path, "\n",
            #     "Test.cmake file:", tests_cmake_files[[i]], "\n")

            benchmark_paths <- c(benchmark_paths, benchmark_path)
        }
    }

    return(unique(benchmark_paths))
}
