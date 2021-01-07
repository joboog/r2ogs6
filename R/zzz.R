.onLoad <- function(libname, pkgname){

    op <- options()
    op.r2ogs6 <- list(
        # Default paths
        r2ogs6.default_sim_path = "D:/OGS_sims/",
        r2ogs6.default_script_path = "D:/OGS_scripts/",
        r2ogs6.default_benchmark_path =
            "D:/Programme/OpenGeoSys/ogs-master-Tests-Data/Tests/Data/",
        r2ogs6.default_ogs_bin_path =
            paste0("D:/Programme/OpenGeoSys/",
                   "ogs-6.3.2-Windows-10.0.14393-x64-python-3.7.2-de-utils",
                   "/bin/")

        # External file reference tags
    )
    toset <- !(names(op.r2ogs6) %in% names(op))
    if (any(toset)) options(op.r2ogs6[toset])



    invisible()
}