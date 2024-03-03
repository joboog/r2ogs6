test_that("install_ogs installs OGS correctly", {

  if(Sys.getenv("RUN_PYTHON_CONFIG_TESTS") != "true"){
        skip("Do not run config python tests.")
    }
    
  # Setup
  ogs_version <- "6.4.4"
  envname <- "test_env"

  # Run the function
  install_ogs(ogs_version = ogs_version, envname = envname)

  # Check if the environment exists
  expect_true(reticulate::py_env_exists(envname))

  # Check if ogs was installed
  expect_true(reticulate::py_module_available("ogs"))

  # Check bin path
  ogs_path <- "~/.virtualenvs/test_env/bin/ogs"
  expanded_ogs_path <- gsub("^~", Sys.getenv("HOME"), ogs_path)
  expect_true(Sys.getenv("r2ogs6.default_ogs6_bin_path"), expanded_ogs_path)

  # Check if ogs can be called
  ogs6_bin_path <- unlist(options("r2ogs6.default_ogs6_bin_path"))
  exit_code <- system2(command = ogs6_bin_path, args = "--version")
  
  expect_true(exit_code, 0)

  # Cleanup
  reticulate::py_remove(envname)
})