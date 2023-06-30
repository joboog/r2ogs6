
# Set global variables

# Python vtk library reference
vtk <- NULL
dsa <- NULL

.onLoad <- function(libname, pkgname){

    op <- options()
    op.r2ogs6 <- list(
        # Default paths
        r2ogs6.default_sim_path = NULL,
        r2ogs6.default_script_path = NULL,
        r2ogs6.default_benchmark_path = NULL,
        r2ogs6.default_ogs6_processlib_path = NULL,
        r2ogs6.default_ogs6_bin_path = NULL,
        r2ogs6.default_singularity_opts = NULL,
        r2ogs6.max_lines_gml = 300
    )

    toset <- !(names(op.r2ogs6) %in% names(op))
    if (any(toset)) options(op.r2ogs6[toset])

    if(file.exists("config.yml")){
        cfg <- config::get()

        for(i in names(cfg)){
            if(i %in% names(op.r2ogs6)) {
                eval(parse(text = paste0("options(", i, " = cfg$", i, ")")))
            }
        }
    }

    # use superassignments to update global Python references
    vtk <<- reticulate::import("vtk", delay_load = TRUE)
    dsa <<- reticulate::import("vtk.numpy_interface.dataset_adapter",
                               delay_load = TRUE)
    # TODO: rhdf5 is now listed as dpendency in DESCRIPTION
    # check if it causes problems, if not delete the outcommented code
    # # check for rhdf5 package
    # if (!requireNamespace("rhdf5")) {
    #     if (!requireNamespace("BiocManager")) {
    #         warning("Required package 'rhdf5' is missing.")
    # }}
    return(invisible())
}

.onAttach <- function(libname, pkgname){

    packageStartupMessage(
        paste("r2ogs6 works best with its options set :)\nFor",
              "an overview, use the command",
              "'options()[grepl(\"^r2ogs6\",",
              "names(options()))]'\nTo set an option, use the",
              "command 'options(\"<option_name>\" = <option_value>)'\n"))

    # TODO: rhdf5 is now listed as dpendency in DESCRIPTION
    # check if it causes problems, if not delete the outcommented code
    # # check for rhdf5 package
    # if (!requireNamespace("rhdf5")) {
    #     if (!requireNamespace("BiocManager")) {
    #
    #         packageStartupMessage(
    #             "The packages rhdf5 and BiocManager are required for r2ogs6
    #             \n but are not installed.
    #             \n Do you want to install it now? (y/n)")
    #         userin <- readline(" ")
    #         if (tolower(userin) == "y") {
    #             tryCatch({utils::install.packages("BiocManager")},
    #                      error = function(e) {
    #                          packageStartupMessage(
    #                     paste(c("\nPackage BiocManager could not be installed",
    #                              "\n", (e))))
    #                      }
    #             )
    #         } else {
    #             packageStartupMessage("\nBiocManager installation was skipped")
    #         }
    #     } else {
    #         packageStartupMessage(
    #             "The package rhdf5 required for r2ogs6 is not installed.
    #             \n Do you want to install it now? (y/n)")
    #         userin <- readline(" ")
    #     }
    #     if (tolower(userin) == "y") {
    #         tryCatch({BiocManager::install("rhdf5")},
    #                  error = function(e) {
    #                      packageStartupMessage(
    #                          paste(c("\nPackage rhdf5 could not be installed",
    #                                  "\n", (e))))
    #                  })
    #     } else {
    #         packageStartupMessage("\nrhdf5 installation was skipped")
    #     }
    # }
}
