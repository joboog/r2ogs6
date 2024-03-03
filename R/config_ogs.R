# Function based on: 
# https://rstudio.github.io/reticulate/articles/python_dependencies.html

#' Install specific version of OpenGeoSys 6 (OGS) along with necessary Python 
#' packages
#'
#' This function handles the installation of a specific version of OGS and 
#' ensures that related Python packages such as `vtk` and `numpy` are 
#' installed in a specified Python virtual environment.
#'
#' @param ogs_version The version of OGS to install (default: 6.4.4).
#' @param envname The name of the Python virtual environment (default: r2ogs6).
#' @param ... Rest of the arguments passed to "py_install()"
#' @return None.
#' @examples
#' install_ogs(
#'  ogs_version = "6.4.4", 
#'  envname = "r2ogs6"
#' )
#'
#' @export
install_ogs <-
    function(...,
           ogs_version = "6.4.4",
           envname = "r2ogs6",
           py_version = "3.10:latest",
           py_exe = NULL,
           new_env = identical(envname, "r2ogs6")) {

    # Check or install python
    if (!(reticulate::py_available())) {
        print(
            paste0("No python installation found.\n", 
            "Attemping to install python ", py_version)
        )
        reticulate::install_python(py_version)
    } else {

        if (!(is.null(py_exe))) {
            reticulate::use_python(py_exe)
            print(paste0("Using python: ", py_exe))
        }
    }

    py_version <- reticulate::py_version()
    py_exe <- reticulate::py_exe()

    # Define packages to install
    pkgs <- c(paste0("ogs==", ogs_version), "numpy", "vtk")

    reticulate::py_install(
      packages = pkgs,
      envname = envname, ...
    )

    # Configure ogs_bin_path
    set_ogs6_bin_path()
  }


#' Set default OpenGeoSys 6 binary path
#'
#' This function automatically detects and sets the path to the OpenGeoSys (OGS)
#' binary within the active Python virtual environment. It is typically called
#' after installing OGS using the `install_ogs` function.
#'
#' @return None, but it sets an option `r2ogs6.default_ogs6_bin_path` to point
#' to the detected OGS binary path.
#' @examples
#' set_ogs_bin_path()
#'
#' @export
set_ogs6_bin_path <-
    function() {
        cfg <- reticulate::py_config()
        ogs_bin_dir <- file.path(cfg$virtualenv, "bin")
        files <- list.files(ogs_bin_dir, full.names = TRUE)

        if (Sys.info()["sysname"] == "Windows") {
            ogs_name <- "ogs.exe"
        } else {
            ogs_name <- "ogs"
        }
        ogs_bin_path <- files[grepl(paste0(ogs_name, "$"), basename(files))]
        options("r2ogs6.default_ogs6_bin_path" = ogs_bin_path)
  }