
# Set global variables

# Python vtk library reference
vtk <- NULL

.onLoad <- function(libname, pkgname){

    op <- options()
    op.r2ogs6 <- list(
        # Default paths
        r2ogs6.default_sim_path = "",
        r2ogs6.default_script_path = "",
        r2ogs6.default_benchmark_path = "",
        r2ogs6.default_ogs6_processlib_path = "",
        r2ogs6.default_ogs6_bin_path = "",
        r2ogs6.max_lines_gml = 300,

        # External file reference tags

        # Reticulate setting for reading in VTK
        r2ogs6.use_python = ""
    )

    toset <- !(names(op.r2ogs6) %in% names(op))
    if (any(toset)) options(op.r2ogs6[toset])

    # use superassignments to update global Python references
    vtk <<- reticulate::import("vtk", delay_load = TRUE)

    test_config <- TRUE

    if (test_config) {
        options(
            r2ogs6.default_sim_path = "D:/OGS_sims/",
            r2ogs6.default_script_path = "D:/OGS_scripts/",
            r2ogs6.default_benchmark_path =
                "D:/Programme/OpenGeoSys/ogs-master/Tests/Data/",
            r2ogs6.default_ogs6_processlib_path =
                "D:/Programme/OpenGeoSys/ogs-master/ProcessLib/",
            r2ogs6.default_ogs6_bin_path =
                paste0(
                    "D:/Programme/OpenGeoSys/",
                    "ogs-6.3.2-Windows-10.0.14393-x64-python-3.7.2-de-utils",
                    "/bin/"
                ),
            r2ogs6.max_lines_gml = 300,
            r2ogs6.use_python = "D:/Programme/anaconda3/envs/rtest/python.exe"
        )

        reticulate::use_virtualenv(unlist(options("r2ogs6.use_python")))
    }

    invisible()
}


.onAttach <- function(libname, pkgname){

    packageStartupMessage(
        paste("r2ogs6 works best with its options set :)\nFor",
              "an overview, use the command",
              "'options()[grepl(\"r2ogs6.\",",
              "names(options()), fixed = TRUE)]'\nTo set an option, use the",
              "command 'options(\"<option_name>\" = <option_value>)'\n"))
}
