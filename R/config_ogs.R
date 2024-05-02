# Function based on: 
# https://rstudio.github.io/reticulate/articles/python_dependencies.html

#' Install specific version of OpenGeoSys 6 (OGS) along with necessary Python 
#' packages `vtk` and `numpy` into a specified Python virtual environment.
#'
#' @param ogs_version The version of OGS to install (default: 6.4.4).
#' @param envname The name of the Python virtual environment (default: r2ogs6).
#' @param ... Rest of the arguments passed to "py_install()"
#' @return None.
#'
#' @export
install_ogs <-
    function(...,
           ogs_version = "6.4.4",
           envname = "r2ogs6"
    ) {

    pkgs <- c(paste0("ogs==", ogs_version), "numpy", "vtk")

    reticulate::py_install(
      packages = pkgs,
      envname = envname,
      ...
    )
  }


#' Set default OpenGeoSys 6 binary path
#'
#' This function automatically detects and temporarily sets the path to the 
#' OpenGeoSys (OGS) binary within the active Python virtual environment. It is 
#' typically called after installing OGS using the `install_ogs()` function.
#' For a permanent setting of the OGS binary path, consider to define
#' `r2ogs6.default_ogs6_bin_path` in a `config.yml` file.'
#'
#' @return None, but it sets an option `r2ogs6.default_ogs6_bin_path` to point
#' to the detected OGS binary path.
#'
#' @export
set_ogs6_bin_path <-
    function() {
        cfg <- reticulate::py_config()

        has_python <- reticulate::py_available(initialize = FALSE)
        has_numpy <- reticulate::py_numpy_available(initialize = FALSE)
        has_vtk <- reticulate::py_module_available("vtk")
        has_ogs <- reticulate::py_module_available("ogs")
        assertthat::assert_that(
            isTRUE(has_python), isTRUE(has_numpy), isTRUE(has_vtk),
            isTRUE(has_ogs)
        )

        ogs_bin_dir <- file.path(cfg$virtualenv, "bin")
        files <- list.files(ogs_bin_dir, full.names = TRUE)
        if (Sys.info()["sysname"] == "Windows") {
            ogs_name <- "ogs.exe"
        } else {
            ogs_name <- "ogs"
        }
        ogs_bin_path <- files[grepl(paste0(ogs_name, "$"), basename(files))]
        
        assertthat::assert_that(file.exists(ogs_bin_path))
        assertthat::assert_that(
            basename(ogs_bin_path) %in% c("ogs.exe", "ogs")
        )
        options("r2ogs6.default_ogs6_bin_path" = ogs_bin_path)

        message(
            paste0(
                'The option "r2ogs6.default_ogs6_bin_path" was temporarily ',
                'set to "', ogs_bin_path, '" .\n',
                'For a permanent setting of "r2ogs6.default_ogs6_bin_path=',
                ogs_bin_path, ' define it in a "config.yml" file.'
            )
        )
  }