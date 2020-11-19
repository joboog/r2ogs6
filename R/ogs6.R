

#============================== R6 ================================

#'OGS6
#'@description Constructor for the OGS6 base class
#'@param sim_name The name of the simulation
#'@param sim_id The ID of the simulation
#'@param sim_path The path where all relevant files for the simulation will be saved
#'@param ogs_bin_path Path to OpenGeoSys6 /bin directory
#'@export
OGS6 <- R6::R6Class("OGS6",
  public = list(

    initialize = function(sim_name,
                          sim_id,
                          sim_path,
                          ogs_bin_path) {

      # Basic validation
      assertthat::assert_that(assertthat::is.string(sim_name))
      assertthat::assert_that(assertthat::is.number(sim_id))
      assertthat::assert_that(assertthat::is.string(sim_path))
      assertthat::assert_that(assertthat::is.string(ogs_bin_path))

      validate_paths(sim_path, ogs_bin_path)

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

    add_gml = function(gml){
      assertthat::assert_that(class(gml) == "r2ogs6_gml")
      if(!is.null(private$.gml)){
        warning("Overwriting gml and geometry variable of OGS6 object", call. = FALSE)

      }
      private$.gml <- gml
      private$.geometry <- paste0(gml$name, ".gml")
    },

    add_process = function(process){
      assertthat::assert_that(class(process) == "r2ogs6_process")
      private$.processes <- c(private$.processes, list(process))
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
      private$.media <- c(private$.media, list(medium))
    },

    add_parameter = function(parameter){
      assertthat::assert_that(class(parameter) == "r2ogs6_parameter")
      private$.parameters <- c(private$.parameters, list(parameter))
    },

    add_curve = function(curve){
      assertthat::assert_that(class(curve) == "r2ogs6_curve")
      private$.curves <- c(private$.curves, list(curve))
    },

    add_process_variable = function(process_variable){
      assertthat::assert_that(class(process_variable) == "r2ogs6_process_variable")
      private$.process_variables <- c(private$.process_variables, list(process_variable))
    },

    add_nonlinear_solver = function(nonlinear_solver){
      assertthat::assert_that(class(nonlinear_solver) == "r2ogs6_nonlinear_solver")
      private$.nonlinear_solvers <- c(private$.nonlinear_solvers, list(nonlinear_solver))
    },

    add_linear_solver = function(linear_solver){
      assertthat::assert_that(class(linear_solver) == "r2ogs6_linear_solver")
      private$.linear_solvers <- c(private$.linear_solvers, list(linear_solver))
    },

    add_vtkdiff = function(vtkdiff){
      assertthat::assert_that(class(vtkdiff) == "r2ogs6_vtkdiff")
      private$.test_definition <- c(private$.test_definition, list(vtkdiff))
    },

    get_status = function(){

      flag <- TRUE

      #.gml
      #flag <- obj_is_defined(flag, private$.gml, "gml")

      #.vtu
      flag <- get_list_status(flag, private$.meshes, "mesh")

      #.prj
      flag <- get_list_status(flag, private$.processes, "process")
      flag <- obj_is_defined(flag, private$.time_loop, "time_loop")
      flag <- get_list_status(flag, private$.media, "medium")
      flag <- get_list_status(flag, private$.parameters, "parameter")
      flag <- get_list_status(flag, private$.curves, "curve", is_opt = TRUE)
      flag <- get_list_status(flag, private$.process_variables, "process_variable")
      flag <- get_list_status(flag, private$.nonlinear_solvers, "nonlinear_solver")
      flag <- get_list_status(flag, private$.linear_solvers, "linear_solver")
      flag <- get_list_status(flag, private$.test_definition, "vtkdiff", is_opt = TRUE)

      if(flag){
        cat(paste0("Your simulation object has all necessary components.\n",
        "You can try to start the simulation by calling run_simulation() on your OGS6 object.\n",
        "Note that this will call more validation functions so you may not be done just yet.\n"))
      }

      return(invisible(flag))
    }

  ),

  active = list(
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
      },

      gml = function(value) {
        if (missing(value)) {
          private$.gml
        } else {
          stop("`To modify `$gml`, use add_gml().", call. = FALSE)
        }
      },

      meshes = function(value) {
        if (missing(value)) {
          private$.meshes
        } else {
          stop("`$meshes` is read only", call. = FALSE)
        }
      },

      geometry = function(value) {
        if (missing(value)) {
          private$.geometry
        } else {
          stop("`$geometry` is read only", call. = FALSE)
        }
      },

      processes = function(value) {
        if (missing(value)) {
          private$.processes
        } else {
          stop("`To modify `$processes`, use add_process().", call. = FALSE)
        }
      },

      time_loop = function(value) {
        if (missing(value)) {
          private$.time_loop
        } else {
          stop("`To modify `$time_loop`, use add_time_loop().", call. = FALSE)
        }
      },

      media = function(value) {
        if (missing(value)) {
          private$.media
        } else {
          stop("`To modify `$media`, use add_medium().", call. = FALSE)
        }
      },

      parameters = function(value) {
        if (missing(value)) {
          private$.parameters
        } else {
          stop("`To modify `$parameters`, use add_parameter().", call. = FALSE)
        }
      },

      curves = function(value) {
        if (missing(value)) {
          private$.curves
        } else {
          stop("`To modify `$curves`, use add_curve().", call. = FALSE)
        }
      },

      process_variables = function(value) {
        if (missing(value)) {
          private$.process_variables
        } else {
          stop("`To modify `$process_variables`, use add_process_variable().", call. = FALSE)
        }
      },

      nonlinear_solvers = function(value) {
        if (missing(value)) {
          private$.nonlinear_solvers
        } else {
          stop("`To modify `$nonlinear_solvers`, use add_nonlinear_solver().", call. = FALSE)
        }
      },

      linear_solvers = function(value) {
        if (missing(value)) {
          private$.linear_solvers
        } else {
          stop("`To modify `$linear_solvers`, use add_linear_solver().", call. = FALSE)
        }
      },

      test_definition = function(value) {
        if (missing(value)) {
          private$.test_definition
        } else {
          stop("`To modify `$test_definition`, use add_test_definition().", call. = FALSE)
        }
      }
  ),

  private = list(
    #general parameters
      .sim_output = NULL,
      .sim_name = NULL,
      .sim_id = NULL,
      .sim_path = NULL,
      .ogs_bin_path = NULL,

      #.gml parameters
      .gml = NULL,

      #.prj parameters
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

