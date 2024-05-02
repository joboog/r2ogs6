test_that("install_ogs installs OGS correctly", {

  if(Sys.getenv("RUN_PYTHON_CONFIG_TESTS") != "true"){
    skip("Do not run config python tests.")
  }
    
  # Setup
  ogs_version <- "6.4.4"
  envname <- "r2ogs6"
  if (!(reticulate::virtualenv_exists(envname))){
    reticulate::virtualenv_create(envname)
  }
  reticulate::use_virtualenv(envname)

  # Run the function
  install_ogs(ogs_version = ogs_version, envname = envname)
  set_ogs6_bin_path()

  # Check if ogs was installed
  expect_true(reticulate::py_module_available("ogs"))

  # Check bin path
  ogs_path <- paste0("~/.virtualenvs/", envname, "/bin/ogs")
  expanded_ogs_path <- gsub("^~", Sys.getenv("HOME"), ogs_path)
  expect_equal(options("r2ogs6.default_ogs6_bin_path")[[1]], expanded_ogs_path)

  # Check if ogs can be called
  ogs6_bin_path <- unlist(options("r2ogs6.default_ogs6_bin_path"))
  exit_code <- system2(command = ogs6_bin_path, args = "--version")
  
  expect_equal(exit_code, 0)

})