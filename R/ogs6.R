

#============================== R6 ================================

#'OGS6
#'@description Constructor for the OGS6 base class
#'@param sim_name The name of the simulation
#'@param sim_id The ID of the simulation
#'@param sim_path The path where all relevant files for the simulation will be saved
#'@param ogs_bin_path Path to OpenGeoSys6 /bin directory
OGS6 <- R6::R6Class("OGS6",
  public = list(

    initialize = function(sim_name,
                          sim_id,
                          sim_path,
                          ogs_bin_path) {

      # Basic validation
      assertthat::assert_that(assertthat::is.string(sim_name))
      assertthat::assert_that(is.integer(sim_id))
      assertthat::assert_that(assertthat::is.string(sim_path))
      assertthat::assert_that(assertthat::is.string(ogs_bin_path))

        private$.sim_input <- list()
        private$.sim_output <- list()

        private$.sim_name <- sim_name
        private$.sim_id <- sim_id
        private$.sim_path <- sim_path
        private$.ogs_bin_path <- ogs_bin_path


        private$.meshes <- list()
        private$.geometry <- NULL
        private$.processes <- list()
        private$.time_loop <- NULL
        private$.media <- list()
        private$.parameters <- list()
        private$.curves <- list()
        private$.process_variables <- list()
        private$.nonlinear_solvers <- list()
        private$.linear_solvers <- list()
        private$.test_definition <- list()
    },

    add_sim_output = function(name, value) {
        private$.sim_output[[name]] <- value
    },

    add_mesh = function(mesh){
      assertthat::assert_that(assertthat::is.string(mesh))
      private$.meshes <- c(private$.meshes, mesh)
    },

    add_geometry = function(geometry){
      assertthat::assert_that(assertthat::is.string(geometry))
      private$.geometry <- c(private$.geometry, geometry)
    },

    add_process = function(process){
      assertthat::assert_that(class(process) == "r2ogs6_process")
      private$.processes <- c(private$.processes, process)
    },

    add_time_loop = function(time_loop){
      assertthat::assert_that(class(time_loop) == "r2ogs6_time_loop")
      if(!is.null(private$.time_loop)){
        warning("Overwriting time_loop variable of OGS6 object", call. = FALSE)

      }
      private$.time_loop <- time_loop
    },

    add_medium = function(medium){
      assertthat::assert_that(class(medium) == "r2ogs6_medium")
      private$.media <- c(private$.media, medium)
    },

    add_parameter = function(parameter){
      assertthat::assert_that(class(parameter) == "r2ogs6_parameter")
      private$.parameters <- c(private$.parameters, parameter)
    },

    add_curve = function(curve){
      assertthat::assert_that(class(curve) == "r2ogs6_curve")
      private$.curves <- c(private$.curves, curve)
    },

    add_process_variable = function(process_variable){
      assertthat::assert_that(class(process_variable) == "r2ogs6_process_variable")
      private$.process_variables <- c(private$.process_variables, process_variable)
    },

    add_nonlinear_solver = function(nonlinear_solver){
      assertthat::assert_that(class(parameter) == "r2ogs6_nonlinear_solver")
      private$.nonlinear_solvers <- c(private$.nonlinear_solvers, nonlinear_solver)
    },

    add_linear_solver = function(linear_solver){
      assertthat::assert_that(class(linear_solver) == "r2ogs6_linear_solver")
      private$.linear_solvers <- c(private$.linear_solvers, linear_solver)
    },

    add_vtkdiff = function(vtkdiff){
      assertthat::assert_that(class(vtkdiff) == "r2ogs6_vtkdiff")
      private$.test_definition <- c(private$.test_definition, vtkdiff)
    },

    get_status = function(){

      ready_for_sim <- list_has_element(private$.processes, "process")
      ready_for_sim <- obj_is_null(private$.time_loop, "time_loop")
      ready_for_sim <- list_has_element(private$.media, "medium")
      ready_for_sim <- list_has_element(private$.parameters, "parameter")
      ready_for_sim <- list_has_element(private$.process_variables, "process_variable")
      ready_for_sim <- list_has_element(private$.nonlinear_solvers, "nonlinear_solver")
      ready_for_sim <- list_has_element(private$.linear_solvers, "linear_solver")

      if(ready_for_sim){
        cat("Your simulation object has all necessary components. You can try to start the
             simulation by calling run_simulation() on your OGS6 object. Note that this will
             call more validation functions so you may not be done just yet.", "\n")
      }

      return(invisible(ready_for_sim))
    },

    validate = function(){

      if(!self$get_status()){
        stop("There are some components missing from your OGS6 object.", call. = FALSE)
      }


    }


  ),

  active = list(
      sim_input = function(value) {
          if (missing(value)) {
              private$.sim_input
          } else {
              stop("To modify `$sim_input`, use set_sim_input().", call. = FALSE)
          }
      },

      sim_output = function(value) {
        if (missing(value)) {
          private$.sim_output
        } else {
          stop("To modify `$sim_output`, use set_sim_output().", call. = FALSE)
        }
      },

      sim_name = function(value) {
          if (missing(value)) {
              private$.sim_name
          } else {
              stop("`$sim_name` is read only", call. = FALSE)
          }
      },

      sim_id = function(value) {
          if (missing(value)) {
              private$.sim_id
          } else {
              stop("`$sim_id` is read only", call. = FALSE)
          }
      },

      sim_path = function(value) {
          if (missing(value)) {
              private$.sim_path
          } else {
              stop("`$sim_path` is read only", call. = FALSE)
          }
      },

      ogs_bin_path = function(value) {
        if (missing(value)) {
          private$.ogs_bin_path
        } else {
          stop("`$ogs_bin_path` is read only", call. = FALSE)
        }
      }

  ),

  private = list(
      .sim_input = NULL,
      .sim_output = NULL,
      .sim_name = NULL,
      .sim_id = NULL,
      .sim_path = NULL,
      .ogs_bin_path = NULL,

      .meshes = NULL,
      .geometry = NULL,
      .processes = NULL,
      .time_loop = NULL,
      .media = NULL,
      .parameters = NULL,
      .curves = NULL,
      .process_variables = NULL,
      .nonlinear_solvers = NULL,
      .linear_solvers = NULL,
      .test_definition = NULL
  )
)




#============================== S3 ================================

#'Constructor for the ogs6 base class
#'
#'@param sim_io ...
#'@param sim_name A string value representing the simulation name
#'@param sim_id An integer value representing the simulation ID
#'@param sim_path A string value describing the path where the IO files will be saved
new_ogs6 <- function(sim_io = list(input = list(), output = list()),
                     sim_name = character(),
                     sim_id = integer(),
                     sim_path = character()) {

    # Basic validation
    if (!is.character(sim_name)) {
        stop("'sim_name' has to be of type character", call. = FALSE)
    }

    if (!is.integer(sim_id)) {
        stop("'sim_id' has to be of type integer", call. = FALSE)
    }

    if (!is.character(sim_path)) {
        stop("'sim_path' has to be of type character", call. = FALSE)
    }

    structure(
        sim_io,
        sim_name = sim_name,
        sim_id = sim_id,
        sim_path = sim_path,
        class = "ogs6"
    )
}

#' Validating functions for an ogs6 class object (to be run before a simulation is started)
validate_ogs6 <- function(ogs6_obj) {


    if (!class(ogs6_obj) == "ogs6") {
        stop("ogs6_obj is not of class 'ogs6' ", call. = FALSE)
    }
    if (is.null(x$input)) {
        stop("'input' list missing.", call. = FALSE)
    }
    if (is.null(x$output)) {
        stop("'output' list missing.", call. = FALSE)
    }

}

