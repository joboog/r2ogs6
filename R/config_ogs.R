# Function based on:
# https://rstudio.github.io/reticulate/articles/python_dependencies.html

#' Install specific version of OpenGeoSys 6 (OGS) along with necessary Python
#' packages `vtk` and `numpy` into a specified Python virtual environment.
#' Note, this does not work with `conda` environments.
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

    if (!(reticulate::virtualenv_exists(envname))){
        stop(
            paste0('The Python virtual environment "', envname,
            '" does not exist. You can create it with ',
            'reticulate::virtualenv_create("r2ogs6").\n',
            'Make sure that this environment is used as the actual ',
            'Python configuration; see reticulate::py_config().'
            )
        )
    }

    pkgs <- c(paste0("ogs==", ogs_version), "numpy", "vtk")

    reticulate::py_install(
      packages = pkgs,
      envname = envname,
      ...
    )
  }


