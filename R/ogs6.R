
#===== OGS6 =====


#' OGS6
#' @description Constructor for the \code{OGS6} base class
#' @export
OGS6 <- R6::R6Class("OGS6",
  public = list(

    #' @description
    #' Creates new OGS6 object
    #' @param sim_name string: Simulation name
    #' @param sim_path string: Path where all files for the simulation will be
    #'   saved
    #' @examples
    #' ogs6_obj <- OGS6$new(sim_name = "my_sim", sim_path = "my/path")
    initialize = function(sim_name,
                          sim_path) {

      # Basic validation
      self$sim_name <- sim_name

      if(missing(sim_path)){
        sim_path <- unlist(options("r2ogs6.default_sim_path"))
      }
      self$sim_path <- sim_path

    },


    #===== Adding components =====


    #' @description
    #' Adds a .prj simulation component
    #' @param x An object of any .prj `prj` class
    #' @examples
    #' ogs6_obj <- OGS6$new(sim_name = "my_sim", sim_path = "my/path")
    #' ogs6_obj$add(prj_parameter(name = "foo", type = "bar"))
    add = function(x){

      # Assert that class name is in implemented .prj classes for OGS6
      ogs6_prj_classes <- ogs6_prj_top_level_classes()
      assertthat::assert_that(class(x) %in% ogs6_prj_classes)

      # Get name of corresponding OGS6 component
      component_name <- names(ogs6_prj_classes)[ogs6_prj_classes == class(x)]

      component_class <-
        eval(parse(text = paste0("class(self$", component_name, ")")))

      active_field_call <-
        ifelse(component_class == "list",
               paste0("self$", component_name,
                      " <- c(self$", component_name, ", list(x))"),
               paste0("self$", component_name, " <- x"))

      eval(parse(text = active_field_call))

      # If class has `name` variable, make it accessable by name
      if(component_class == "list" &&
         "name" %in% names(as.list(formals(class(x))))){

        name_call <- paste0("names(self$", component_name, ")[[length(self$",
                            component_name, ")]] <- x$name")

        eval(parse(text = name_call))
      }

      invisible(self)
    },

    #' @description
    #' Adds a reference to a file with ending .gml and optionally, a
    #' \code{OGS6_gml} object
    #' @param gml string | OGS6_gml: Either a path to a file with extension
    #' .gml or a OGS6_gml object.
    #' @examples
    #' ogs6_obj <- OGS6$new(sim_name = "my_sim", sim_path = "my/path")
    #' ogs6_obj$add_gml("this_works.gml")
    #' \dontrun{ogs6_obj$add_gml("this_doesnt.oops")}
    add_gml = function(gml){

      if(assertthat::is.string(gml)){
        assertthat::assert_that(grepl("\\.gml$", gml))
        private$.geometry <- gml
      }else{
        assertthat::assert_that(inherits(gml, "OGS6_gml"))
        private$.gml <- gml
        private$.geometry <- paste0(self$sim_name, ".gml")
      }

      invisible(self)
    },

    #' @description
    #' Adds a reference to a \code{.vtu} file and optionally, a \code{OGS6_vtu}
    #' object
    #' @param path string: A path
    #' @param axisym flag: Is the mesh axially symmetrical?
    #' @param read_in_vtu flag: Optional: Should \code{.vtu} file just be
    #' copied or read in too?
    #' @examples
    #' ogs6_obj <- OGS6$new(sim_name = "my_sim", sim_path = "my/path")
    #' ogs6_obj$add_vtu("this_works.vtu")
    #' \dontrun{ogs6_obj$add_vtu("this_doesnt.oops")}
    add_vtu = function(path,
                       axisym = FALSE,
                       read_in_vtu = FALSE){
      assertthat::assert_that(assertthat::is.string(path))
      assertthat::assert_that(grepl("\\.vtu$", path))
      assertthat::assert_that(assertthat::is.flag(axisym))
      assertthat::assert_that(assertthat::is.flag(read_in_vtu))

      self$meshes <- c(self$meshes,
                       list(mesh = list(path = path,
                                        axially_symmetric = axisym)))

      if(read_in_vtu){
        private$.vtus <- c(private$.vtus, list(OGS6_vtu$new(path)))
      }

      invisible(self)
    },

    #' @description
    #' Update a component of the \code{OGS6} object.
    #' @param cmpts list(sublist, length(sublist) == 2): The first element
    #' of a sublist is a character that calls an \code{OGS6} component, the
    #' second one is the corresponding value.
    #' @examples
    #' \dontrun{ogs6_obj$update_component(list(
    #'                    list("ogs6_obj$parameters[[1]]$value", 2.3),
    #'                    list("ogs6_obj$media[[1]]$properties[[2]]$value",
    #'                         1.0e-3)))}
    update_component = function(cmpts){#cmpts=list(list(call_str, value))
        assertthat::assert_that(is.list(cmpts))

        for (i in seq_along(cmpts)){
            # check sublists
            assertthat::assert_that(is.list(cmpts[[i]]))
            assertthat::assert_that(length(cmpts[[i]])==2)
            assertthat::assert_that(is.character(cmpts[[i]][[1]]))

            # update component via call
            call_str <- cmpts[[i]][[1]]
            value <- cmpts[[i]][[2]]
            call_str <- gsub("^[A-Za-z_0-9]*\\$", "self$", call_str)
            assertthat::assert_that(!is.null(eval(parse(text = call_str))),
                                    msg = paste(call_str,
                                                "not found in ogs6_obj",
                                                self$sim_name))
            set_call <- paste0(call_str, " <- ", value)
            eval(parse(text = set_call))
        }
        invisible(self)
    },


    #===== Utility =====


    #' @description
    #' Checks if the \code{OGS6} object has all necessary parameters for
    #' starting a simulation
    #' @param print_status flag: Should the status be printed to the console?
    #' @examples
    #' ogs6_obj <- OGS6$new(sim_name = "my_sim", sim_path = "my/path")
    #' ogs6_obj$get_status()
    get_status = function(print_status = TRUE){

      assertthat::assert_that(assertthat::is.flag(print_status))
      flag <- TRUE

      status_strs <- character()
      tag_names <- lapply(get_prj_top_level_tags(), `[[`, 1)
      required <- lapply(get_prj_top_level_tags(), `[[`, 2)

      for(i in seq_len(length(tag_names))){

        is_required <- required[[i]]

        prj_obj_call <- paste0("private$.", tag_names[[i]])
        prj_obj <- eval(parse(text = prj_obj_call))

        if(length(prj_obj) == 0){
          if(is_required){
            status_str <- crayon::red("\u2717 ")
            flag <- FALSE
          }else{
            status_str <- crayon::yellow("\u2717 ")
          }
        }else{
          status_str <- crayon::green("\u2713 ")
        }

        status_str <- paste0(status_str,
                             "'",
                             tag_names[[i]],
                             ifelse(!class(prj_obj) == "list",
                                    "' is defined",
                                    "' has at least one element"))

        status_strs <- c(status_strs, status_str)
      }

      status <- paste(status_strs, collapse = "\n")

      if(print_status){
        cat(status)

        if(flag){
          cat(paste0("Your OGS6 object has all necessary components.\n",
                     "You can try calling ogs6_run_simulation().",
                     "Note that this calls more validation functions, ",
                     "so you may not be done just yet.\n"))
        }
      }

      return(invisible(flag))
    },

    #' @description
    #' Overrides default printing behaviour
    print = function(){
      cat("OGS6\n")
      cat("simulation name:  ", self$sim_name, "\n", sep = "")
      cat("simulation path:  ", self$sim_path, "\n", sep = "")

      cat("\n----- geometry:  ", self$geometry, "\n", sep = "")
      cat("associated OGS6_gml:\n")
      print(self$gml)

      cat("\n----- meshes -----\n",
          paste(self$meshes, collapse = "\n"),
          "\n", sep = "")

      prj_tags <- lapply(get_prj_top_level_tags(), function(x){x[["tag_name"]]})
      prj_tags <- prj_tags[!prj_tags %in% c("geometry", "mesh", "meshes")]

      for(i in seq_len(length(prj_tags))){
        tag_name <- prj_tags[[i]]

        prj_param_call <- paste0("print(self$", tag_name, ")")

        cat("\n----- ", tag_name, " -----\n", sep = "")
        eval(parse(text = prj_param_call))
        cat("\n", sep = "")
      }

      invisible(self)
    },

    #' print_log
    #' @description Prints logfile to console (if it exists)
    print_log = function(){
      if(!is.null(self$logfile)){
        writeLines(readLines(self$logfile))
      }else{
        cat("There is no logfile associated with this OGS6 object.\n")
      }

      invisible(self)
    },

    #' @description
    #' Clears components from the \code{OGS6} object
    #' @param which character: The names of the components (all by default).
    #' If you want to delete only some components, run
    #' \code{names(ogs6_prj_top_level_classes())} for the available options.
    clear = function(which){

      if(missing(which)){
        which <- names(ogs6_prj_top_level_classes())
      }

      valid_input = names(ogs6_prj_top_level_classes())

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

      invisible(self)
    }

  ),


  #===== Active fields =====


  active = list(

      #' @field sim_name
      #' Simulation name. \code{value} must be string
      sim_name = function(value) {
        if(missing(value)) {
          private$.sim_name
        }else{
          assertthat::assert_that(assertthat::is.string(value))
          private$.sim_name <- value
        }
      },

      #' @field sim_path
      #' Simulation path. \code{value} must be string
      sim_path = function(value) {
        if(missing(value)) {
          private$.sim_path
        }else{
          private$.sim_path <- as_dir_path(value)
        }
      },

      #' @field logfile
      #' Logfile path. \code{value} must be string
      logfile = function(value) {
        if(missing(value)) {
          private$.logfile
        }else{
          assertthat::assert_that(assertthat::is.string(value))
          private$.logfile <- value
        }
      },

      #' @field gml
      #' \code{.gml}. read-only
      gml = function() {
        private$.gml
      },

      #' @field geometry
      #' \code{.prj} \code{geometry} tag. \code{value} must be string
      geometry = function(value) {
        if(missing(value)) {
          private$.geometry
        }else{
          assertthat::assert_that(is.string(value))
          private$.geometry <- value
        }
      },

      #' @field meshes
      #' \code{.prj} \code{meshes} tag. \code{value} must be list of strings
      meshes = function(value) {
        if(missing(value)) {
          private$.meshes
        }else{
          assertthat::assert_that(is.list(value))
          lapply(value, function(x){
            assertthat::assert_that(is.list(x), length(x) == 2)
            assertthat::assert_that(assertthat::is.string(x[[1]]))
            assertthat::assert_that(assertthat::is.flag(x[[2]]))
          })
          private$.meshes <- value
        }
      },


      #' @field vtus
      #' \code{.vtu}s. \code{value} must be list of \code{OGS6_vtu} objects
      vtus = function(value) {
        if(missing(value)) {
          private$.vtus
        }else{
          is_wrapper_list(value,
                          ogs6_prj_top_level_classes()[["vtus"]])
          private$.vtus <- value
        }
      },

      #' @field python_script
      #' \code{.prj} \code{python_script} tag. \code{value} must be string
      python_script = function(value) {
        if(missing(value)) {
          private$.python_script
        }else{
          assertthat::assert_that(assertthat::is.string(value))
          private$.python_script <- value
        }
      },

      #' @field search_length_algorithm
      #' \code{.prj} \code{search_length_algorithm} tag. \code{value} must be
      #' \code{prj_search_length_algorithm} object
      search_length_algorithm = function(value) {
        if(missing(value)) {
          private$.search_length_algorithm
        }else{
          assertthat::assert_that(
            ogs6_prj_top_level_classes()[["search_length_algorithm"]] %in%
              class(value))
          private$.search_length_algorithm <- value
        }
      },

      #' @field processes
      #' \code{.prj} \code{processes} tag. \code{value} must be list of
      #' \code{prj_process} objects
      processes = function(value) {
        if(missing(value)) {
          private$.processes
        }else{
          # If there already is a process element
          if(length(private$.processes) > 0){
            if(ogs6_prj_top_level_classes()[["processes"]] %in%
               class(private$.processes[[1]])){
                 is_wrapper_list(value,
                                 ogs6_prj_top_level_classes()[["processes"]])
            }else{
              assertthat::assert_that(assertthat::is.string(value))
              value <- list(include = c(file = value))
            }

          }else{
            if(assertthat::is.string(value)){
              value <- list(include = c(file = value))
            }else{
              is_wrapper_list(value,
                              ogs6_prj_top_level_classes()[["processes"]])
            }
          }

          private$.processes <- value
        }
      },

      #' @field time_loop
      #' \code{.prj} \code{time_loop} tag. \code{value} must be
      #' \code{prj_time_loop} object
      time_loop = function(value) {
        if(missing(value)) {
          private$.time_loop
        }else{
          assertthat::assert_that(
            ogs6_prj_top_level_classes()[["time_loop"]] %in%
              class(value))
          private$.time_loop <- value
        }
      },

      #' @field local_coordinate_system
      #' \code{.prj} \code{local_coordinate_system} tag. \code{value} must be
      #' \code{prj_local_coordinate_system} object
      local_coordinate_system = function(value) {
        if(missing(value)) {
          private$.local_coordinate_system
        }else{
          assertthat::assert_that(
            ogs6_prj_top_level_classes()[["local_coordinate_system"]] %in%
              class(value))
          private$.local_coordinate_system <- value
        }
      },

      #' @field media
      #' \code{.prj} \code{media} tag. \code{value} must be list of
      #' \code{prj_medium} objects
      media = function(value) {
        if(missing(value)) {
          private$.media
        }else{
          is_wrapper_list(value,
                          ogs6_prj_top_level_classes()[["media"]])
          private$.media <- value
        }
      },

      #' @field parameters
      #' \code{.prj} \code{parameters} tag. \code{value} must be list of
      #' \code{prj_parameter} objects
      parameters = function(value) {
        if(missing(value)) {
          private$.parameters
        }else{
          is_wrapper_list(value,
                          ogs6_prj_top_level_classes()[["parameters"]])
          private$.parameters <- value
        }
      },

      #' @field chemical_system
      #' \code{.prj} \code{chemical_system} tag. \code{value} must be
      #' \code{prj_chemical_system} object
      chemical_system = function(value) {
        if(missing(value)) {
          private$.chemical_system
        }else{
          assertthat::assert_that(
            ogs6_prj_top_level_classes()[["chemical_system"]] %in%
              class(value))
          private$.chemical_system <- value
        }
      },

      #' @field curves
      #' \code{.prj} \code{curves} tag. \code{value} must be list of
      #' \code{prj_curve} objects
      curves = function(value) {
        if(missing(value)) {
          private$.curves
        }else{
          is_wrapper_list(value,
                          ogs6_prj_top_level_classes()[["curves"]])
          private$.curves <- value
        }
      },

      #' @field process_variables
      #' \code{.prj} \code{process_variables} tag. \code{value} must be list of
      #' \code{prj_process_variable} objects
      process_variables = function(value) {
        if(missing(value)) {
          private$.process_variables
        }else{
          is_wrapper_list(
            value,
            ogs6_prj_top_level_classes()[["process_variables"]])
          private$.process_variables <- value
        }
      },

      #' @field nonlinear_solvers
      #' \code{.prj} \code{nonlinear_solvers} tag. \code{value} must be list of
      #' \code{prj_nonlinear_solver} objects
      nonlinear_solvers = function(value) {
        if(missing(value)) {
          private$.nonlinear_solvers
        }else{
          is_wrapper_list(
            value,
            ogs6_prj_top_level_classes()[["nonlinear_solvers"]])
          private$.nonlinear_solvers <- value
        }
      },

      #' @field linear_solvers
      #' \code{.prj} \code{linear_solvers} tag. \code{value} must be list of
      #' \code{prj_linear_solver} objects
      linear_solvers = function(value) {
        if(missing(value)) {
          private$.linear_solvers
        }else{
          is_wrapper_list(value,
                          ogs6_prj_top_level_classes()[["linear_solvers"]])
          private$.linear_solvers <- value
        }
      },

      #' @field test_definition
      #' \code{.prj} \code{test_definition} tag. \code{value} must be list of
      #' \code{prj_vtkdiff} objects
      test_definition = function(value) {
        if(missing(value)) {
          private$.test_definition
        }else{
          is_wrapper_list(value,
                          ogs6_prj_top_level_classes()[["test_definition"]])
          private$.test_definition <- value
        }
      },

      #' @field insitu
      #' \code{.prj} \code{insitu} tag. \code{value} must be
      #' \code{prj_insitu} object
      insitu = function(value) {
        if(missing(value)) {
          private$.insitu
        }else{
          assertthat::assert_that(
            ogs6_prj_top_level_classes()[["insitu"]] %in%
              class(value))
          private$.insitu <- value
        }
      },

      #' @field pvds
      #' \code{.pvd}s. \code{value} must be list of \code{OGS6_pvd} objects
      pvds = function(value) {
        if(missing(value)) {
          private$.pvds
        }else{
          is_wrapper_list(value, "OGS6_pvd")
          private$.pvds <- value
        }
      }
  ),

  #===== Private parameters =====

  private = list(
    # general parameters
      .sim_name = NULL,
      .sim_path = NULL,

      .logfile = NULL,

      # .gml object
      .gml = NULL,

      # .vtu objects
      .vtus = NULL,

      # .prj parameters

      # .gml reference
      .geometry = NULL,

      # .vtu reference(s)
      .meshes = list(),

      .python_script = NULL,
      .search_length_algorithm = NULL,
      .processes = list(),
      .time_loop = NULL,
      .local_coordinate_system = NULL,
      .media = list(),
      .parameters = list(),
      .chemical_system = NULL,
      .curves = list(),
      .process_variables = list(),
      .nonlinear_solvers = list(),
      .linear_solvers = list(),
      .test_definition = list(),
      .insitu = NULL,

      # .pvd objects (output)
      .pvds = NULL
  )
)


#===== ogs6_prj_top_level_classes =====


#' ogs6_prj_top_level_classes
#' @description Returns named character vector of \code{OGS6} top level
#' \code{.prj} tags (names) represented by \code{r2ogs6} classes along with
#' their class names (values).
#' @return character
#' @export
ogs6_prj_top_level_classes <- function(){

  xpaths_for_classes <- xpaths_for_classes

  flattened_xpaths <- unlist(xpaths_for_classes)
  names(flattened_xpaths) <- NULL

  prj_components <- character()
  seen <- character()

  for(i in seq_len(length(flattened_xpaths))){

    split_xpath <-
      unlist(strsplit(flattened_xpaths[[i]], "/", fixed = TRUE))

    if(!split_xpath[[1]] %in% seen){
      if(!is.null(get_class_from_xpath(split_xpath[[1]]))){
        prj_components <- c(prj_components,
                            get_class_from_xpath(split_xpath[[1]]))

      }else{
        xpath <- paste(split_xpath[[1]], split_xpath[[2]], sep = "/")
        prj_components <- c(prj_components,
                            get_class_from_xpath(xpath))
      }

      names(prj_components)[[length(prj_components)]] <- split_xpath[[1]]
      seen <- c(seen, split_xpath[[1]])
    }
  }

  return(prj_components)
}
