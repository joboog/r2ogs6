
#===== OGS6 =====


#'OGS6
#'@description Constructor for the OGS6 base class
#'@export
OGS6 <- R6::R6Class("OGS6",
  public = list(

    #'@description
    #'Creates new OGS6 object
    #'@param sim_name string: Simulation name
    #'@param sim_id double: Simulation ID
    #'@param sim_path string: Path where all files for the simulation will be
    #' saved
    #'@param ogs_bin_path string: Path to OpenGeoSys6 /bin directory
    #'@param test_mode In test mode, sim_path and ogs_bin_path will not be
    #' validated. If you're not a developer, please leave this as it is :)
    initialize = function(sim_name,
                          sim_id,
                          sim_path,
                          ogs_bin_path,
                          test_mode = FALSE) {

      # Basic validation
      assertthat::assert_that(assertthat::is.string(sim_name))
      assertthat::assert_that(assertthat::is.number(sim_id))
      assertthat::assert_that(assertthat::is.string(sim_path))
      assertthat::assert_that(assertthat::is.string(ogs_bin_path))

      sim_path <- validate_is_dir_path(sim_path)
      ogs_bin_path <- validate_is_dir_path(ogs_bin_path)

      if(!test_mode){
        if(!dir.exists(sim_path)){
          dir.create(sim_path)
        }else{
          if(length(dir(sim_path, all.files = TRUE)) != 0){
            warning(paste0("The defined sim_path directory '",
                           sim_path,
                           "' is not empty. Files may be overwritten."),
                    call. = FALSE)
          }
        }

        if(!file.exists(paste0(ogs_bin_path, "generateStructuredMesh.exe"))) {
          stop(paste("Could not find executable file",
                     "generateStructuredMesh.exe at location",
                     ogs_bin_path), call. = FALSE)
        }
      }

        private$.sim_output <- list()

        private$.sim_name <- sim_name
        private$.sim_id <- sim_id
        private$.sim_path <- sim_path
        private$.ogs_bin_path <- ogs_bin_path
    },


    #===== ADDING COMPONENTS =====


    #'@description
    #'... (WIP)
    #'@param name ...
    #'@param value ...
    add_sim_output = function(name, value) {
        private$.sim_output[[name]] <- value
    },

    #'@description
    #'Adds a r2ogs6_mesh object
    #'@param mesh r2ogs6_mesh
    add_mesh = function(mesh){
      assertthat::assert_that(class(mesh) == "r2ogs6_mesh")
      private$.meshes <- c(private$.meshes, list(mesh))
    },

    #'@description
    #'Adds a r2ogs6_gml object
    #'@param gml r2ogs6_gml
    add_gml = function(gml){
      assertthat::assert_that(class(gml) == "r2ogs6_gml")
      if(!is.null(private$.gml)){
        warning("Overwriting gml and geometry variable of OGS6 object",
                call. = FALSE)
      }
      private$.gml <- gml
      private$.geometry <- paste0(gml$name, ".gml")
    },

    #'@description
    #'Adds a python script
    #'@param python_script string: File name of python script
    add_python_script = function(python_script){
      assertthat::assert_that(assertthat::is.string(python_script))
      if(!is.null(private$.python_script)){
        warning("Overwriting python_script variable of OGS6 object",
                call. = FALSE)

      }
      private$.python_script <- python_script
    },

    #'@description
    #'Adds a r2ogs6_process object
    #'@param process r2ogs6_process
    add_process = function(process){
      assertthat::assert_that(class(process) == "r2ogs6_process")
      private$.processes <- c(private$.processes, list(process))
    },

    #'@description
    #'Adds a r2ogs6_time_loop object
    #'@param time_loop r2ogs6_time_loop
    add_time_loop = function(time_loop){
      assertthat::assert_that(class(time_loop) == "r2ogs6_time_loop")
      if(!is.null(private$.time_loop)){
        warning("Overwriting time_loop variable of OGS6 object", call. = FALSE)
      }
      private$.time_loop <- time_loop
    },

    #'@description
    #'Adds a r2ogs6_local_coordinate_system object
    #'@param local_coordinate_system r2ogs6_local_coordinate_system
    add_local_coordinate_system = function(local_coordinate_system){
      assertthat::assert_that(class(local_coordinate_system) ==
                                "r2ogs6_local_coordinate_system")
      if(!is.null(private$.local_coordinate_system)){
        warning("Overwriting local_coordinate_system variable of OGS6 object",
                call. = FALSE)
      }
      private$.local_coordinate_system <- local_coordinate_system
    },

    #'@description
    #'Adds a r2ogs6_medium object
    #'@param medium r2ogs6_medium
    add_medium = function(medium){
      assertthat::assert_that(class(medium) == "r2ogs6_medium")
      private$.media <- c(private$.media, list(medium))
    },

    #'@description
    #'Adds a r2ogs6_parameter object
    #'@param parameter r2ogs6_parameter
    add_parameter = function(parameter){
      assertthat::assert_that(class(parameter) == "r2ogs6_parameter")
      private$.parameters <- c(private$.parameters, list(parameter))
    },

    #'@description
    #'Adds a r2ogs6_curve object
    #'@param curve r2ogs6_curve
    add_curve = function(curve){
      assertthat::assert_that(class(curve) == "r2ogs6_curve")
      private$.curves <- c(private$.curves, list(curve))
    },

    #'@description
    #'Adds a r2ogs6_process_variable object
    #'@param process_variable r2ogs6_process_variable
    add_process_variable = function(process_variable){
      assertthat::assert_that(class(process_variable) ==
                                "r2ogs6_process_variable")
      private$.process_variables <- c(private$.process_variables,
                                      list(process_variable))
    },

    #'@description
    #'Adds a r2ogs6_nonlinear_solver object
    #'@param nonlinear_solver r2ogs6_nonlinear_solver
    add_nonlinear_solver = function(nonlinear_solver){
      assertthat::assert_that(class(nonlinear_solver) ==
                                "r2ogs6_nonlinear_solver")
      private$.nonlinear_solvers <- c(private$.nonlinear_solvers,
                                      list(nonlinear_solver))
    },

    #'@description
    #'Adds a r2ogs6_linear_solver object
    #'@param linear_solver r2ogs6_linear_solver
    add_linear_solver = function(linear_solver){
      assertthat::assert_that(class(linear_solver) == "r2ogs6_linear_solver")
      private$.linear_solvers <- c(private$.linear_solvers, list(linear_solver))
    },

    #'@description
    #'Adds a r2ogs6_vtkdiff object
    #'@param vtkdiff r2ogs6_vtkdiff
    add_vtkdiff = function(vtkdiff){
      assertthat::assert_that(class(vtkdiff) == "r2ogs6_vtkdiff")
      private$.test_definition <- c(private$.test_definition, list(vtkdiff))
    },

    #'@description
    #'Adds a r2ogs6_insitu object
    #'@param insitu r2ogs6_insitu
    add_insitu = function(insitu){
      assertthat::assert_that(class(insitu) == "r2ogs6_insitu")

      if(!is.null(private$.insitu)){
        warning("Overwriting insitu variable of OGS6 object", call. = FALSE)
      }
      private$.insitu <- insitu
    },


    #===== UTILITY FUNCTIONS =====


    #'@description
    #'Checks if the OGS6 object has all necessary parameters for
    #' starting a simulation
    get_status = function(){

      flag <- TRUE
      impl_classes <- get_implemented_classes()

      for(i in seq_len(length(impl_classes))){
        status_call <- paste0("get_obj_status(flag, private$.",
                              names(impl_classes)[[i]], ")")

        flag <- eval(parse(text = status_call))
      }

      if(flag){
        cat(paste0("Your simulation object has all necessary components.\n",
        "You can try to start the simulation by calling run_simulation() ",
        "on your OGS6 object.\n",
        "Note that this will call more validation functions, ",
        "so you may not be done just yet.\n"))
      }

      return(invisible(flag))
    },

    #'@description
    #'Clears components from the OGS6 object
    #'@param which character: The names of the components (all by default).
    #' If you want to delete only some components, run
    #' names(get_implemented_classes()) for the available options.
    clear = function(which = names(get_implemented_classes())){

      assertthat::assert_that(is.character(which))

      valid_input = names(get_implemented_classes())

      null_it <- c("geometry", "time_loop")

      for(i in seq_len(length(which))){
        if(!which[[i]] %in% valid_input){
          warning(paste0("Parameter '", which[[i]],
                         "' not recognized by OGS6$clear(). ",
                        "Valid parameters are:\n'",
                        paste(valid_input, sep = "", collapse = "', '"),
                        "'\nSkipping."), call. = FALSE)
          next
        }else{
          call_str <- ""
          if(which[[i]] %in% null_it){
            call_str <- paste0("private$.", which[[i]], " <- NULL")
          }else{
            call_str <- paste0("private$.", which[[i]], " <- list()")
          }
          eval(parse(text = call_str))
        }
      }
    }

  ),


  #===== ACTIVE FIELDS =====


  active = list(

      #'@field sim_output
      #'Getter for OGS6 private parameter '.sim_output'
      sim_output = function(value) {
        private$.sim_output
      },

      #'@field sim_name
      #'Getter for OGS6 private parameter '.sim_name'
      sim_name = function() {
        private$.sim_name
      },

      #'@field sim_id
      #'Getter for OGS6 private parameter '.sim_id'
      sim_id = function() {
        private$.sim_id
      },

      #'@field sim_path
      #'Getter for OGS6 private parameter '.sim_path'
      sim_path = function() {
        private$.sim_path
      },

      #'@field ogs_bin_path
      #'Getter for OGS6 private parameter '.ogs_bin_path'
      ogs_bin_path = function() {
        private$.ogs_bin_path
      },

      #'@field gml
      #'Getter for OGS6 private parameter '.gml'
      gml = function() {
        private$.gml
      },

      #'@field meshes
      #'Getter for OGS6 private parameter '.meshes'
      meshes = function() {
        private$.meshes
      },

      #'@field geometry
      #'Getter for OGS6 private parameter '.geometry'
      geometry = function() {
        private$.geometry
      },

      #'@field python_script
      #'Getter for OGS6 private parameter '.python_script'
      python_script = function() {
        private$.python_script
      },

      #'@field processes
      #'Getter for OGS6 private parameter '.processes'
      processes = function() {
        private$.processes
      },

      #'@field time_loop
      #'Getter for OGS6 private parameter '.time_loop'
      time_loop = function() {
        private$.time_loop
      },

      #'@field local_coordinate_system
      #'Getter for OGS6 private parameter '.local_coordinate_system'
      local_coordinate_system = function() {
        private$.local_coordinate_system
      },

      #'@field media
      #'Getter for OGS6 private parameter '.media'
      media = function() {
        private$.media
      },

      #'@field parameters
      #'Getter for OGS6 private parameter '.parameters'
      parameters = function() {
        private$.parameters
      },

      #'@field curves
      #'Getter for OGS6 private parameter '.curves'
      curves = function() {
        private$.curves
      },

      #'@field process_variables
      #'Getter for OGS6 private parameter '.process_variables'
      process_variables = function() {
        private$.process_variables
      },

      #'@field nonlinear_solvers
      #'Getter for OGS6 private parameter '.nonlinear_solvers'
      nonlinear_solvers = function() {
        private$.nonlinear_solvers
      },

      #'@field linear_solvers
      #'Getter for OGS6 private parameter '.linear_solvers'
      linear_solvers = function() {
        private$.linear_solvers
      },

      #'@field test_definition
      #'Getter for OGS6 private parameter '.test_definition'
      test_definition = function() {
        private$.test_definition
      },

      #'@field insitu
      #'Getter for OGS6 private parameter '.insitu'
      insitu = function() {
        private$.insitu
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
      .meshes = list(),
      .geometry = NULL,
      .python_script = NULL,
      .processes = list(),
      .time_loop = NULL,
      .local_coordinate_system = NULL,
      .media = list(),
      .parameters = list(),
      .curves = list(),
      .process_variables = list(),
      .nonlinear_solvers = list(),
      .linear_solvers = list(),
      .test_definition = list(),
      .insitu = NULL
  )
)
