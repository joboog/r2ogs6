

#============================== R6 ================================

#'Constructor for the OGS6 base class
OGS6 <- R6::R6Class("OGS6",
  public = list(

    initialize = function(sim_name,
                          sim_id,
                          sim_path) {

      # Basic validation
      assertthat::assert_that(assertthat::is.string(sim_name))
      assertthat::assert_that(is.integer(sim_id))
      assertthat::assert_that(assertthat::is.string(sim_path))

        private$.sim_input <- list()
        private$.sim_output <- list()

        private$.sim_name <- sim_name
        private$.sim_id <- sim_id
        private$.sim_path <- sim_path
    },

    add_sim_input = function(name, value) {
        private$.sim_input[[name]] <- value
    },

    set_sim_input_obj_param = function(obj_name, obj_param_name, value) {
        private$.sim_input[[obj_name]]$obj_param_name <- value
    },

    add_sim_output = function(name, value) {
        private$.sim_output[[name]] <- value
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
      }

  ),

  private = list(
      .sim_input = NULL,
      .sim_output = NULL,
      .sim_name = NULL,
      .sim_id = NULL,
      .sim_path = NULL
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

