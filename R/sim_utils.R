
#===== ogs6_run_simulation =====


#' ogs6_run_simulation
#' @description Wrapper function that calls \code{ogs6_export_sim_files()},
#'   \code{ogs6_call_ogs6()} and \code{ogs6_read_output_files()}.
#' @param ogs6_obj OGS6: Simulation object
#' @param write_logfile flag: Should output be written to a logfile? If
#'   \code{FALSE}, output will be written to console. If \code{TRUE}, logfile
#'   directory will be created in \code{ogs6$sim_path} directory
#' @param ogs6_bin_path string: Optional: OpenGeoSys 6 executable path. Defaults
#'   to \code{options("r2ogs6.default_ogs6_bin_path")}
#' @param overwrite flag: Should existing files be overwritten?
#' @param copy_ext_files flag: Should external files that are references in the
#' \code{ogs6_obj} be  be copied to \code{ogs6_obj$sim_path}?
#' @param verbose flag
#' @param singularity_opts string: Optional: Options to singularity exec
#' command. Defaults to \code{options("r2ogs6.singularity_opts")}
#' @export
ogs6_run_simulation <- function(ogs6_obj,
                               write_logfile = TRUE,
                               ogs6_bin_path,
                               overwrite = T,
                               copy_ext_files = F,
                               verbose = F,
                               singularity_opts) {

    # Export (and / or copy referenced) simulation files
    ogs6_export_sim_files(ogs6_obj = ogs6_obj,
                          overwrite = overwrite,
                          copy_ext_files = copy_ext_files,
                          test_mode = FALSE)

    exit_code <- ogs6_call_ogs6(ogs6_obj = ogs6_obj,
                               write_logfile = write_logfile,
                               ogs6_bin_path = ogs6_bin_path,
                               verbose = verbose,
                               singularity_opts = singularity_opts)

    ogs6_read_output_files(ogs6_obj = ogs6_obj)

    return(exit_code)
}


#===== ogs6_export_sim_files =====


#' ogs6_export_sim_files
#' @description Creates \code{ogs6$sim_path} directory if it does not exist yet
#'   and exports and / or copies all simulation files to it.
#' @param ogs6_obj OGS6: Simulation object
#' @param overwrite flag: Should existing files be overwritten?
#' @param copy_ext_files flag: Should external files that are references in the
#' \code{ogs6_obj} be  be copied to \code{ogs6_obj$sim_path}?
#' @param test_mode flag: If \code{TRUE}, Will not check status of
#'   \code{ogs6_obj} before exporting files. Defaults to \code{FALSE}
#' @export
ogs6_export_sim_files <- function(ogs6_obj,
                                  overwrite = T,
                                  copy_ext_files = F,
                                  test_mode = F){

    assertthat::assert_that(inherits(ogs6_obj, "OGS6"))
    assertthat::assert_that(assertthat::is.flag(test_mode))
    assertthat::assert_that(assertthat::is.flag(copy_ext_files))

    # Call all validators
    if(!test_mode &&
       !ogs6_obj$get_status(print_status = FALSE) &&
       is.null(ogs6_obj$include)){ # joboog: this is a workaround for now
        stop("There are some components missing from your OGS6 object.",
             call. = FALSE)
    }

    # Create the simulation folder
    if (!dir.exists(ogs6_obj$sim_path)) {
        dir.create(ogs6_obj$sim_path)
    }
    else{
        if(!overwrite){
            assertthat::assert_that(length(list.files(ogs6_obj$sim_path)) == 0)
        }
    }

    # handle prj and referenced files
    export_prj(ogs6_obj, copy_ext_files)

    return(invisible())
}

#===== ogs6_call_ogs6 =====


#' ogs6_call_ogs6
#' @description Makes system call to OpenGeoSys 6 and retrieves exit code.
#' @param ogs6_obj OGS6: Simulation object
#' @param write_logfile flag: Should output be written to a logfile? If
#'   \code{FALSE}, output will be written to console. If \code{TRUE}, logfile
#'   directory will be created in \code{ogs6$sim_path} directory
#' @param ogs6_bin_path string: Optional: Path to OpenGeoSys 6 executable or
#'   OpenGeoSys container (singularity image) file. Defaults
#'   to \code{options("r2ogs6.default_ogs6_bin_path")}
#' @param verbose flag
#' @param singularity_opts string: Optional: Options to singularity exec
#' command. Defaults to \code{options("r2ogs6.singularity_opts")}
#' @export
ogs6_call_ogs6 <- function(ogs6_obj,
                          write_logfile = TRUE,
                          ogs6_bin_path,
                          verbose = F,
                          singularity_opts){

    assertthat::assert_that(inherits(ogs6_obj, "OGS6"))
    assertthat::assert_that(assertthat::is.flag(write_logfile))

    if(missing(ogs6_bin_path)){
        ogs6_bin_path <- unlist(options("r2ogs6.default_ogs6_bin_path"))
    }
    else if(is.null(ogs6_bin_path)){
        ogs6_bin_path <- unlist(options("r2ogs6.default_ogs6_bin_path"))
    }

    assertthat::assert_that(assertthat::is.string(ogs6_bin_path))
    assertthat::assert_that(assertthat::is.flag(verbose))

    if(missing(singularity_opts)){
        singularity_opts <- unlist(options("r2ogs6.default_singularity_opts"))
    }
    if(is.null(singularity_opts)){
        singularity_opts <- ""
    }
    assertthat::assert_that(assertthat::is.string(singularity_opts))

    # construt call to os
    prj_path_full <- paste0(ogs6_obj$sim_path,
                            ogs6_obj$sim_name,
                            ".prj")
    ogs6_args <- c(prj_path_full, "-o", ogs6_obj$sim_path)
    ogs6_command <- construct_ogs_command(ogs6_bin_path, singularity_opts)

    #  reorder for using 'system2()'
    if (length(ogs6_command)>1) {
        ogs6_command_str <- ogs6_command[1]
        ogs6_args <- c(ogs6_command[-1], ogs6_args)
    } else {
        ogs6_command_str <- ogs6_command
    }

    exit_code <- 0

    # Finally, make the system call to start the simulation
    if (write_logfile) {

        # Create logfile directory
        logfile_dir <- paste0(ogs6_obj$sim_path, "logfiles/")

        # Set logfile parameter of simulation object
        ogs6_obj$logfile <- paste0(logfile_dir, ogs6_obj$sim_name, "_log.txt")

        if(!dir.exists(logfile_dir)){
            dir.create(logfile_dir)
        }else{
            # If old logfile exists, delete it
            if(file.exists(ogs6_obj$logfile)){
                file.remove(ogs6_obj$logfile)
            }
        }

        # (Re)create logfile
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

    return(exit_code)
}



#' construct_ogs_command
#' @description Constructs the call string to for 'system2()'.
#' @param ogs6_bin_path string: Optional: Path to OpenGeoSys 6 executable or
#'   OpenGeoSys container (singularity image) file. Defaults
#'   to \code{options("r2ogs6.default_ogs6_bin_path")}
#' @param singularity_opts string: Optional: Options to singularity exec
#' command. Defaults to \code{options("r2ogs6.singularity_opts")}
#'
#' @return string: Call object.
construct_ogs_command <- function(ogs6_bin_path, singularity_opts){

    assertthat::assert_that(assertthat::is.string(ogs6_bin_path))
    assertthat::assert_that(assertthat::is.string(singularity_opts))

    # check if existent
    if (dir.exists(ogs6_bin_path)) {
        stop("'ogs6_bin_path' has to be an executable or container image file.",
             call. = FALSE)
    }
    else if (!(file.exists(ogs6_bin_path))) {
        stop("'ogs6_bin_path' does not exist.'",
             call. = FALSE)
    }

    # Construct the call wether ogs6_bin_path is executable or
    # container image file
    if (stringr::str_sub(ogs6_bin_path, -4) == ".sif"){

        assertthat::assert_that(file.exists(ogs6_bin_path))
        ogs6_command <- c("singularity","exec", singularity_opts,
                          ogs6_bin_path, "ogs")
    }
    else {
        ogs6_command <- paste0(ogs6_bin_path)
        assertthat::assert_that(file.exists(ogs6_command))
    }

    return(ogs6_command)
}

#===== ogs6_read_output_files =====


#' ogs6_read_output_files
#' @description Read in generated \code{.pvd} files and add it to ogs6_obj
#' @param ogs6_obj OGS6: Simulation object
#' @export
ogs6_read_output_files <- function(ogs6_obj){

    assertthat::assert_that(inherits(ogs6_obj, "OGS6"))

    output_paths <- list.files(ogs6_obj$sim_path,
                            "\\.pvd$|\\.h5$",
                            full.names = TRUE)
    # Wait for eventual file writing processes to finish
    t0 <- Sys.time()
    while(((length(output_paths) == 0) | any(file.size(output_paths) <= 64)) &
          difftime(Sys.time(), t0, units = "secs") < 2) {

        output_paths <- list.files(ogs6_obj$sim_path,
                                   "\\.pvd$|\\.h5$",
                                   full.names = TRUE)
        Sys.sleep(0.01)
    }
    if (((length(output_paths) == 0) | any(file.size(output_paths) <= 64)))  {
        stop("Output file not written out correctly.
                    Unable to import *.pvd")
    } else {
        pvds_exist <- grepl("\\.pvd$", output_paths)
        h5s_exist <- grepl("\\.h5$", output_paths)
        if (any(pvds_exist)) {
            ogs6_obj$pvds <- lapply(output_paths[which(pvds_exist)],
                                    function(x) {OGS6_pvd$new(pvd_path = x)})
    }
        if (any(h5s_exist)) {
            ogs6_obj$h5s <- lapply(output_paths[which(h5s_exist)],
                                   function(x) {OGS6_h5$new(h5_path = x)})
        }
    }




    return(invisible())
}

#===== Test benchmarks =====


#' Run benchmark
#'
#' Utility function for quick benchmark runs
#'
#' @param prj_path string:
#' @param ogs6_bin_path string:
#' @param copy_ext_files flag: Should external files that are references in the
#' \code{ogs6_obj} be  be copied to \code{ogs6_obj$sim_path}?
#' @param sim_path string: Path where simulation files will be saved
#' @noRd
run_benchmark <- function(prj_path,
                          ogs6_bin_path,
                          sim_path,
                          copy_ext_files = F){

    if(missing(ogs6_bin_path)){
        ogs6_bin_path <- unlist(options("r2ogs6.default_ogs6_bin_path"))
    }

    if(missing(sim_path)){
        sim_path <- unlist(options("r2ogs6.default_sim_path"))
    }

    if(grepl("\\.xml$", prj_path)) {
        # some *.prj files are indicated as *.xml in their Tests.cmake file
        prj_path <- sub("\\.xml$", replacement = ".prj", x =  prj_path)
    }

    assertthat::assert_that(assertthat::is.string(prj_path))
    assertthat::assert_that(assertthat::is.string(ogs6_bin_path))
    assertthat::assert_that(assertthat::is.string(sim_path))
    assertthat::assert_that(assertthat::is.flag(copy_ext_files))

    sim_name <- tools::file_path_sans_ext(basename(prj_path))

    ogs6_obj <- OGS6$new(sim_name = sim_name,
                         sim_path = sim_path)

    # check if *.gml file is present
    read_gml <-  ifelse(
                    any(sapply(list.files(), function(x) grepl(".gml", x))),
                    T, F)

    read_in_prj(ogs6_obj = ogs6_obj,
                prj_path = prj_path,
                read_in_gml = read_gml,
                read_includes = T)

    return(invisible(ogs6_run_simulation(
                        ogs6_obj, ogs6_bin_path = ogs6_bin_path,
                        copy_ext_files = copy_ext_files)))
}


#' Run benchmarks
#'
#' This is a wrapper function for `run_benchmark()`.
#'
#' @param path string: Path to benchmark folder
#' @param ogs6_processlib_path string: Path to OpenGeoSys 6 ProcessLib folder
#'   which contains relevant Tests.cmake files
#' @param ogs6_bin_path string:
#' @param sim_path string: Path where simulation files will be saved
#' @param starting_from_prj_path string: `.prj` path to start from
#' @param print_results flag: Print results in the end?
#' @noRd
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
    nonexisting_prj_paths <- prj_paths[!file.exists(unlist(prj_paths))]
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


#' Get benchmark paths
#'
#' Gets paths to all benchmarks that should work from `Tests.cmake` files
#'
#' @param ogs6_processlib_path string: Path to OpenGeoSys 6 ProcessLib folder
#'   which contains relevant `Tests.cmake` files
#' @noRd
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
