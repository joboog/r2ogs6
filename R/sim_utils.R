
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


#===== Chaining utility (WIP) =====


#'read_in_output
#'@description After a OGS6 simulation was run, reads in the generated .vtu
#' files as new input for
#' the .prj file
#'@param ogs6_obj A OGS6 class object
read_in_output <- function(ogs6_obj) {

    #....WIP
}




