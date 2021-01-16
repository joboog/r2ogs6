
# global Python references (will be initialized in .onLoad)
vtk <- NULL
zlib <- NULL


.onLoad <- function(libname, pkgname){

    op <- options()
    op.r2ogs6 <- list(
        # Default paths
        r2ogs6.default_sim_path = "",
        r2ogs6.default_script_path = "",
        r2ogs6.default_benchmark_path = "",
        r2ogs6.default_ogs_bin_path = "",
        r2ogs6.max_lines_gml = 300,

        # External file reference tags

        # Reticulate setting for reading in VTK
        r2ogs6.use_python = ""
    )

    toset <- !(names(op.r2ogs6) %in% names(op))
    if (any(toset)) options(op.r2ogs6[toset])

    # use superassignments to update global Python references
    vtk <<- reticulate::import("vtk", delay_load = TRUE)
    vtk_dsa <<- reticulate::import("vtk.numpy_interface.dataset_adapter",
                                   delay_load = TRUE)
    zlib <<- reticulate::import("zlib", delay_load = TRUE)

    test_config <- TRUE

    if (test_config) {
        options(
            r2ogs6.default_sim_path = "D:/OGS_sims/",
            r2ogs6.default_script_path = "D:/OGS_scripts/",
            r2ogs6.default_benchmark_path =
                "D:/Programme/OpenGeoSys/ogs-master-Tests-Data/",
            r2ogs6.default_ogs_bin_path =
                paste0(
                    "D:/Programme/OpenGeoSys/",
                    "ogs-6.3.2-Windows-10.0.14393-x64-python-3.7.2-de-utils",
                    "/bin/"
                ),
            r2ogs6.use_python = "D:/Programme/anaconda3/envs/rtest/python.exe",
            r2ogs6.max_lines_gml = 300
        )

        reticulate::use_virtualenv(unlist(options("r2ogs6.use_python")))
    }

    cat("Loaded package r2ogs6. Your options are set as follows:\n\n",
        paste(
            names(options()[grepl("r2ogs6.", names(options()), fixed = TRUE)]),
            options()[grepl("r2ogs6.", names(options()), fixed = TRUE)],
            sep = " = ",
            collapse = "\n"),
        "\n\nTo change them, type options(\"<option_name>\" = <option_value>).",
        sep = "")

    invisible()
}