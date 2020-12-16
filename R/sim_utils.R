
#===== run_simulation =====


#'run_simulation
#'@description Calls OGS6 object validator functions, exports all necessary
#' files and starts OpenGeoSys6
#'@param ogs6_obj OGS6: Simulation object
#'@param output_to_log_file flag: Should output be written to a log file?
#'@export
run_simulation <- function(ogs6_obj, output_to_log_file = TRUE) {

    assertthat::assert_that(inherits(ogs6_obj), "OGS6")
    assertthat::assert_that(assertthat::is.flag(output_to_log_file))

    # Call all validators
    validate_all(ogs6_obj)

    # Create the simulation folder
    if (!dir.exists(ogs6_obj$sim_path)) {
        dir.create(ogs6_obj$sim_path)
    } else{
        if (length(dir(ogs6_obj$sim_path, all.files = TRUE)) != 0) {
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
    export_gml(ogs6_obj)
    export_prj(ogs6_obj)

    # Construct the system call
    ogs6_call <- paste0(
        ogs6_obj$ogs_bin_path,
        "ogs.exe ",
        ogs6_obj$sim_path,
        ogs6_obj$sim_name,
        ".prj -o ",
        ogs6_obj$sim_path
    )

    # Finally, make the system call to start the simulation
    if (output_to_log_file) {
        system(command = setup_logging(ogs6_obj$sim_name,
                                       ogs6_obj$sim_path,
                                       ogs6_call))
    } else{
        system(command = ogs6_call)
    }

    closeAllConnections()
}


#===== LOGGING UTILITY =====


#'setup_logging
#'@description Sets up logging.
#'@param sim_name string: Simulation name
#'@param sim_path string: Simulation path
#'@param ogs6_call string: Corresponding OGS6 call
setup_logging <- function(sim_name, sim_path, ogs6_call){

    assertthat::assert_that(assertthat::is.string(sim_name))
    assertthat::assert_that(assertthat::is.string(sim_path))
    sim_path <- validate_is_dir_path(sim_path)

    assertthat::assert_that(assertthat::is.string(ogs6_call))

    # Create logfile directory
    logfile_dir <- paste0(sim_path, "logfiles/")

    if(!dir.exists(logfile_dir)){
        dir.create(logfile_dir)
    }

    # Create initialization script
    script_path <- paste0(logfile_dir, "sim_init.R")

    if(!file.exists(script_path)){
        file.create(script_path)
    }

    cmd_str <- paste0("system(command = \"", ogs6_call, "\")")

    file_conn <- file(script_path)
    writeLines(c(cmd_str), file_conn)
    close(file_conn)

    # Return string for calling R CMD BATCH
    batch_call <- paste0("R CMD BATCH --no-echo ",
                         script_path, " ",
                         sim_name, "_log.txt")

    return(invisible(batch_call))
}


#===== Validation utility =====


#'validate_all
#'@description Validates all necessary parameters
#'@param ogs6_obj A OGS6 class object
validate_all <- function(ogs6_obj) {

    if(!ogs6_obj$get_status()){
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


#===== Chaining utility (WIP) =====


#'read_in_output
#'@description After a OGS6 simulation was run, reads in the generated .vtu
#' files as new input for
#' the .prj file
#'@param ogs6_obj A OGS6 class object
read_in_output <- function(ogs6_obj) {

    #....WIP
}




