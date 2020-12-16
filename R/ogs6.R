
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
    #'@param test_mode In test mode, ogs_bin_path will not be
    #' validated. Unless you're a dev, please don't touch.
    initialize = function(sim_name,
                          sim_id,
                          sim_path,
                          ogs_bin_path,
                          test_mode = FALSE) {

      # Basic validation
      assertthat::assert_that(assertthat::is.string(sim_name))
      assertthat::assert_that(assertthat::is.number(sim_id))

      self$sim_path <- sim_path

      if(!test_mode){
        if(!file.exists(paste0(ogs_bin_path, "generateStructuredMesh.exe"))) {
          stop(paste("Could not find executable file",
                     "generateStructuredMesh.exe at location",
                     ogs_bin_path), call. = FALSE)
        }
      }

        private$.sim_name <- sim_name
        private$.sim_id <- sim_id
        private$.ogs_bin_path <- validate_is_dir_path(ogs_bin_path)
    },


    #===== ADDING COMPONENTS =====


    #'@description
    #'Adds a r2ogs6_mesh object
    #'@param mesh r2ogs6_mesh
    add_mesh = function(mesh){
      self$meshes <- c(self$meshes,
                       list(mesh))
    },

    #'@description
    #'Adds a r2ogs6_gml object
    #'@param gml r2ogs6_gml
    add_gml = function(gml){
      assertthat::assert_that(class(gml) == "r2ogs6_gml")

      private$.gml <- gml
      private$.geometry <- paste0(gml$name, ".gml")
    },

    #'@description
    #'Adds a python script
    #'@param python_script string: File name of python script
    add_python_script = function(python_script){
      self$python_script <- python_script
    },

    #'@description
    #'Adds a r2ogs6_process object
    #'@param process r2ogs6_process
    add_process = function(process){
      self$processes <- c(self$processes,
                          list(process))

      names(self$processes)[[length(self$processes)]] <- process$name
    },

    #'@description
    #'Adds a r2ogs6_time_loop object
    #'@param time_loop r2ogs6_time_loop
    add_time_loop = function(time_loop){
      self$time_loop <- time_loop
    },

    #'@description
    #'Adds a r2ogs6_local_coordinate_system object
    #'@param local_coordinate_system r2ogs6_local_coordinate_system
    add_local_coordinate_system = function(local_coordinate_system){
      self$local_coordinate_system <- local_coordinate_system
    },

    #'@description
    #'Adds a r2ogs6_medium object
    #'@param medium r2ogs6_medium
    add_medium = function(medium){
      self$media <- c(self$media,
                      list(medium))
    },

    #'@description
    #'Adds a r2ogs6_parameter object
    #'@param parameter r2ogs6_parameter
    add_parameter = function(parameter){
      self$parameters <- c(self$parameters,
                       list(parameter))

      names(self$parameters)[[length(self$parameters)]] <- parameter$name
    },

    #'@description
    #'Adds a r2ogs6_curve object
    #'@param curve r2ogs6_curve
    add_curve = function(curve){
      self$curves <- c(self$curves,
                       list(curve))
    },

    #'@description
    #'Adds a r2ogs6_process_variable object
    #'@param process_variable r2ogs6_process_variable
    add_process_variable = function(process_variable){
      self$process_variables <- c(self$process_variables,
                                  list(process_variable))

      names(self$process_variables)[[length(self$process_variables)]] <-
        process_variable$name
    },

    #'@description
    #'Adds a r2ogs6_nonlinear_solver object
    #'@param nonlinear_solver r2ogs6_nonlinear_solver
    add_nonlinear_solver = function(nonlinear_solver){
      self$nonlinear_solvers <- c(self$nonlinear_solvers,
                                  list(nonlinear_solver))

      names(self$nonlinear_solvers)[[length(self$nonlinear_solvers)]] <-
        nonlinear_solver$name
    },

    #'@description
    #'Adds a r2ogs6_linear_solver object
    #'@param linear_solver r2ogs6_linear_solver
    add_linear_solver = function(linear_solver){
      self$linear_solvers <- c(self$linear_solvers,
                               list(linear_solver))

      names(self$linear_solvers)[[length(self$linear_solvers)]] <-
        linear_solver$name
    },

    #'@description
    #'Adds a r2ogs6_vtkdiff object
    #'@param vtkdiff r2ogs6_vtkdiff
    add_vtkdiff = function(vtkdiff){
      self$test_definition <- c(self$test_definition, list(vtkdiff))
    },

    #'@description
    #'Adds a r2ogs6_insitu object
    #'@param insitu r2ogs6_insitu
    add_insitu = function(insitu){
      self$insitu <- insitu
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
      #'Access to private parameter '.sim_path'
      sim_path = function(value) {
        if(missing(value)) {
          private$.sim_path
        }else{
          value <- validate_is_dir_path(value)
          private$.sim_path <- value
        }
      },

      #'@field ogs_bin_path
      #'Getter for OGS6 private parameter '.ogs_bin_path'
      ogs_bin_path = function() {
        private$.ogs_bin_path
      },

      #'@field geometry
      #'Getter for OGS6 private parameter '.geometry'
      geometry = function() {
        private$.geometry
      },

      #'@field gml
      #'Getter for OGS6 private parameter '.gml'
      gml = function() {
        private$.gml
      },

      #'@field meshes
      #'Access to private parameter '.meshes'
      meshes = function(value) {
        if(missing(value)) {
          private$.meshes
        }else{
          validate_wrapper_list(value,
                                get_implemented_classes()[["meshes"]])
          private$.meshes <- value
        }
      },

      #'@field python_script
      #'Access to private parameter '.python_script'
      python_script = function(value) {
        if(missing(value)) {
          private$.python_script
        }else{
          assertthat::assert_that(assertthat::is.string(value))
          private$.python_script <- value
        }
      },

      #'@field processes
      #'Access to private parameter '.processes'
      processes = function(value) {
        if(missing(value)) {
          private$.processes
        }else{
          validate_wrapper_list(value,
                                get_implemented_classes()[["processes"]])
          private$.processes <- value
        }
      },

      #'@field time_loop
      #'Access to private parameter '.time_loop'
      time_loop = function(value) {
        if(missing(value)) {
          private$.time_loop
        }else{
          assertthat::assert_that(
            get_implemented_classes()[["time_loop"]] %in%
              class(value))
          private$.time_loop <- value
        }
      },

      #'@field local_coordinate_system
      #'Access to private parameter '.local_coordinate_system'
      local_coordinate_system = function(value) {
        if(missing(value)) {
          private$.local_coordinate_system
        }else{
          assertthat::assert_that(
            get_implemented_classes()[["local_coordinate_system"]] %in%
              class(value))
          private$.local_coordinate_system <- value
        }
      },

      #'@field media
      #'Access to private parameter '.media'
      media = function(value) {
        if(missing(value)) {
          private$.media
        }else{
          validate_wrapper_list(value,
                                get_implemented_classes()[["media"]])
          private$.media <- value
        }
      },

      #'@field parameters
      #'Access to private parameter '.parameters'
      parameters = function(value) {
        if(missing(value)) {
          private$.parameters
        }else{
          validate_wrapper_list(value,
                                get_implemented_classes()[["parameters"]])
          private$.parameters <- value
        }
      },

      #'@field curves
      #'Access to private parameter '.curves'
      curves = function(value) {
        if(missing(value)) {
          private$.curves
        }else{
          validate_wrapper_list(value,
                                get_implemented_classes()[["curves"]])
          private$.curves <- value
        }
      },

      #'@field process_variables
      #'Access to private parameter '.process_variables'
      process_variables = function(value) {
        if(missing(value)) {
          private$.process_variables
        }else{
          validate_wrapper_list(
            value,
            get_implemented_classes()[["process_variables"]])
          private$.process_variables <- value
        }
      },

      #'@field nonlinear_solvers
      #'Access to private parameter '.nonlinear_solvers'
      nonlinear_solvers = function(value) {
        if(missing(value)) {
          private$.nonlinear_solvers
        }else{
          validate_wrapper_list(
            value,
            get_implemented_classes()[["nonlinear_solvers"]])
          private$.nonlinear_solvers <- value
        }
      },

      #'@field linear_solvers
      #'Access to private parameter '.linear_solvers'
      linear_solvers = function(value) {
        if(missing(value)) {
          private$.linear_solvers
        }else{
          validate_wrapper_list(value,
                                get_implemented_classes()[["linear_solvers"]])
          private$.linear_solvers <- value
        }
      },

      #'@field test_definition
      #'Access to private parameter '.test_definition'
      test_definition = function(value) {
        if(missing(value)) {
          private$.test_definition
        }else{
          validate_wrapper_list(value,
                                get_implemented_classes()[["test_definition"]])
          private$.test_definition <- value
        }
      },

      #'@field insitu
      #'Access to private parameter '.insitu'
      insitu = function(value) {
        if(missing(value)) {
          private$.insitu
        }else{
          assertthat::assert_that(
            get_implemented_classes()[["insitu"]] %in%
              class(value))
          private$.insitu <- value
        }
      }
  ),

  private = list(
    #general parameters
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
