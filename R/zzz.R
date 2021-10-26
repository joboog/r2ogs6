
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

    return(invisible())
}


.onAttach <- function(libname, pkgname){

    packageStartupMessage(
        paste("r2ogs6 works best with its options set :)\nFor",
              "an overview, use the command",
              "'options()[grepl(\"^r2ogs6\",",
              "names(options()))]'\nTo set an option, use the",
              "command 'options(\"<option_name>\" = <option_value>)'\n"))
}
