test_that("install_ogs installs OGS correctly", {

  if(Sys.getenv("RUN_PYTHON_CONFIG_TESTS") != "true"){
    skip("Do not run config python tests.")
  }

  # Setup
  ogs_version <- "6.5.2"
  envname <- "r2ogs6"
  numpy_version <- "1.26.4"
  vtk_version <- "9.3.1"
  if (!(reticulate::virtualenv_exists(envname))){
    reticulate::virtualenv_create(
      envname = envname,
      packages = c(
        paste0("numpy==", numpy_version),
        paste0("vtk==", vtk_version)
      )
    )
  }
  reticulate::use_virtualenv(envname)
  reticulate::py_config()

  if (reticulate::py_module_available("ogs")){
    reticulate::virtualenv_remove(
      envname = envname, packages = "ogs", confirm = FALSE
    )
  }

  # Run the function
  install_ogs(
    ogs_version = ogs_version,
    envname = envname,
    numpy_version = numpy_version,
    vtk_version = vtk_version
  )

  # Check if ogs was installed
  expect_true(reticulate::py_module_available("ogs"))
})


test_that("check setting r2ogs6.default_ogs6_bin_path correctly", {

  if(Sys.getenv("RUN_PYTHON_CONFIG_TESTS") != "true"){
    skip("Do not run config python tests.")
  }

  # Setup
  envname <- "r2ogs6"
  reticulate::use_virtualenv(envname)
  reticulate::py_config()

  if (!(reticulate::py_module_available("ogs"))){
    skip("ogs is not installed.")
  }
  if (!(is.null(options("r2ogs6.default_ogs6_bin_path")))){
    options("r2ogs6.default_ogs6_bin_path" = NULL)
  }

  # Run the function
  set_ogs6_bin_path()

  # Check bin path
  cfg <- reticulate::py_config()
  venv_path <- cfg$virtualenv
  if (Sys.info()["sysname"] == "Windows") {
      ogs_dir <- "Scripts/ogs.exe"
  } else {
      ogs_dir <- "bin/ogs"
  }
  ogs_path_ref <- file.path(venv_path, ogs_dir) %>%
                    normalizePath()
  ogs_path <- options("r2ogs6.default_ogs6_bin_path")[[1]] %>%
                normalizePath()

  expect_equal(ogs_path, ogs_path_ref)
})


test_that("ogs can be called", {

  if(Sys.getenv("RUN_PYTHON_CONFIG_TESTS") != "true"){
    skip("Do not run config python tests.")
  }

  # Setup
  envname <- "r2ogs6"
  reticulate::use_virtualenv(envname)
  reticulate::py_config()
  if (!(reticulate::py_module_available("ogs"))){
    skip("ogs is not installed.")
  }

  if (is.null(options("r2ogs6.default_ogs6_bin_path"))){
    set_ogs6_bin_path()
  }

  # Check if ogs can be called
  ogs6_bin_path <- unlist(options("r2ogs6.default_ogs6_bin_path"))
  exit_code <- system2(command = ogs6_bin_path, args = "--version")

  expect_equal(exit_code, 0)
})