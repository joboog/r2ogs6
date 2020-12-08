#This script contains some useful methods for a developer.


#===== IMPLEMENTATION UTILITY =====


#'select_fitting_subclass
#'@description Utility function to differentiate which property class to pick
#' i.e. when dealing with r2ogs6 which has 3 subclasses with the tag name
#' 'property'
#'@param xpath_expr string: An XPath expression
#'@param subclasses_names character: A named character vector
select_fitting_subclass <- function(xpath_expr, subclasses_names){

  assertthat::assert_that(assertthat::is.string(xpath_expr))
  assertthat::assert_that(is.character(subclasses_names))

  split_path <- unlist(strsplit(xpath_expr, "/", fixed = TRUE))

  if(identical(sort(unique(names(subclasses_names))),
               sort(names(subclasses_names)))){
    tag_name <- split_path[[length(split_path)]]
    return(invisible(subclasses_names[[tag_name]]))
  }

  grandparent_path <- paste(utils::tail(split_path, 3), collapse = "/")

  subclass_name <- ""

  switch(
    grandparent_path,
    "medium/phases/phase" =
      {
        subclass_name <- "r2ogs6_phase"
      },
    "medium/properties/property" =
      {
        subclass_name <- "r2ogs6_pr_property"
      },
    "phase/properties/property" =
      {
        subclass_name <- "r2ogs6_ph_property"
      },

    "component/properties/property" =
      {
        subclass_name <- "r2ogs6_com_property"
      }
  )

  return(invisible(subclass_name))
}


#'get_subclass_names
#'@description Utility function, returns the names of the subclasses
#' of a r2ogs6 class
#'@param class_name string: The name of a r2ogs6 class
#'@return character: The names of the subclasses as a character vector
#' (empty if there are none)
get_subclass_names <- function(class_name) {

  assertthat::assert_that(assertthat::is.string(class_name))

  subclasses_names <- character()

  switch(class_name,

         r2ogs6_chemical_system = {
           subclasses_names <- c("r2ogs6_solution",
                                 "r2ogs6_phase_component",
                                 "r2ogs6_kinetic_reactant",
                                 "r2ogs6_rate")
         },
         r2ogs6_linear_solver = {
           subclasses_names <- c("r2ogs6_eigen")
         },
         r2ogs6_medium = {
           subclasses_names <- c("r2ogs6_phase",
                                 "r2ogs6_pr_property",
                                 "r2ogs6_ph_property",
                                 "r2ogs6_com_property")
         },
         r2ogs6_process_variable = {
           subclasses_names <- c("r2ogs6_boundary_condition",
                                 "r2ogs6_source_term",
                                 "r2ogs6_deactivated_subdomain")
         },
         r2ogs6_time_loop = {
           subclasses_names <- c("r2ogs6_tl_process",
                                 "r2ogs6_output",
                                 "r2ogs6_global_processes_coupling",
                                 "r2ogs6_convergence_criterion")
         }
  )

  return(invisible(subclasses_names))
}


#'get_class_tag_name
#'@description Utility function, returns the tag name of a r2ogs6 class
#'@param class_name string: The name of a r2ogs6 class
#'@return string: The tag name corresponding to class_name
get_class_tag_name <- function(class_name) {

  assertthat::assert_that(assertthat::is.string(class_name))

  tag_name <- ""

  if(class_name %in% names(get_nonstandard_tag_names())){
    tag_name <- get_nonstandard_tag_names()[[class_name]]
  }else{
    tag_name <- paste(utils::tail(unlist(strsplit(class_name,
                                                  "_",
                                                  fixed = TRUE)),
                           -1),
                      collapse = "_")
  }

  return(invisible(tag_name))
}


#'get_nonstandard_tag_names
#'@description Utility function, returns nonstandard tag names
#'@return character: The tag names of classes that are not named after the
#' convention r2ogs6_<tag name> because there already is a class with that name.
#' If you as a dev create new classes like that, just add them to the list :)
get_nonstandard_tag_names <- function(){

  tag_names <- c(r2ogs6_tl_process = "process",
                 r2ogs6_pr_property = "property",
                 r2ogs6_ph_property = "property",
                 r2ogs6_com_property = "property")

  return(invisible(tag_names))
}


#'get_implemented_classes
#'@description Utility function, returns the names of all classes implemented
#' so far. Change this if you implement new classes or delete old ones!
#' If you implement a new class, you add the following to the character vector:
#' <name_of_corresponding_OGS6_parameter> = <name_of_your_class>
get_implemented_classes <- function(){

  class_names <- c(meshes = "r2ogs6_mesh",
                   gml = "r2ogs6_gml",
                   search_length_algorithm = "r2ogs6_search_length_algorithm",
                   processes = "r2ogs6_process",
                   media = "r2ogs6_medium",
                   time_loop = "r2ogs6_time_loop",
                   local_coordinate_system = "r2ogs6_local_coordinate_system",
                   parameters = "r2ogs6_parameter",
                   curves = "r2ogs6_curve",
                   process_variables = "r2ogs6_process_variable",
                   nonlinear_solvers = "r2ogs6_nonlinear_solver",
                   linear_solvers = "r2ogs6_linear_solver",
                   test_definition = "r2ogs6_vtkdiff",
                   insitu = "r2ogs6_insitu")

  return(invisible(class_names))
}


#===== INFO UTILITY =====


#'get_list_status
#'@description Helper function for get_status() to check if a list has at least
#' one element.
#'@param flag Boolean flag to keep track of missing components
#'@param obj_list The specified list
#'@param element_type Optional: What kind of elements are in the list?
#'@param is_opt flag: Does the list need at least one element?
get_list_status <- function(flag,
                            obj_list,
                            element_type = "list element",
                            is_opt = FALSE){

  sim_ready <- flag

  if(length(obj_list) == 0){
    if(!is_opt){
      cat(crayon::red("\u2717"))
      sim_ready <- FALSE
    }else{
      cat(crayon::yellow("()"))
    }
  }else{
    cat(crayon::green("\u2713"))
  }

  cat(" At least one", element_type, "was defined", "\n")

  return(invisible(sim_ready))
}


#'obj_is_defined
#'@description Helper function for get_status() to check if an object was
#' defined
#'@param flag Boolean flag to keep track of missing components
#'@param obj The specified object
#'@param obj_type Optional: What kind of object is this?
#'@param is_opt flag: Is the element optional i.e. can it be NULL?
obj_is_defined <- function(flag,
                           obj,
                           obj_type = "",
                           is_opt = FALSE){
  is_defined <- flag

  if(is.null(obj)){
    cat(crayon::red("\u2717"))
    is_defined <- FALSE
  }else{
    cat(crayon::green("\u2713"))
  }

  cat(" ", obj_type, "object is not NULL", "\n")

  return(invisible(is_defined))
}


#===== COERCION UTILITY =====


#'coerce_string_to_numeric
#'@description If an object is of type string, coerces it to a numeric type:
#' A double if 'split' is FALSE as per default, a numeric vector otherwise.
#' If 'split' is set to true the string will be split at ' ' (whitespace)
#' characters.
#'@param obj An object to check
#'@param split flag: Should object be split at ' ' (whitespace) if it is a
#' string?
#'@return The object as a numeric type (if 'obj' was a string, else the
#' unchanged 'obj')
coerce_string_to_numeric <- function(obj, split = FALSE){

  if(assertthat::is.string(obj)){
    if(split){
      obj <- as.double(unlist(strsplit(obj, " ")))
    }else{
      obj <- as.double(obj)
    }
  }

  return(invisible(obj))
}


#===== VALIDATION UTILITY =====


#===== Validation helpers for required parameters =====


#'validate_is_string
#'@description Checks if an object is a number (helper to save
#' some typing when validating obligatory object parameters)
#'@param ... Ellipsis
validate_is_number <- function(...){

  objs <- list(...)

  for(i in seq_len(length(objects))){
    assertthat::assert_that(assertthat::is.number(objs[[i]]))
  }

  return(invisible(TRUE))
}

#'validate_is_string
#'@description Checks if an object is a string (helper to save
#' some typing when validating obligatory object parameters)
#'@param ... Ellipsis
validate_is_string <- function(...){

  objs <- list(...)

  for(i in seq_len(length(objects))){
      assertthat::assert_that(assertthat::is.string(objs[[i]]))
  }

  return(invisible(TRUE))
}


#'validate_is_string_flag
#'@description Checks if an object is a string reading either
#' "true" or "false"
#'@param ... Ellipsis
validate_is_string_flag <- function(...){

  objs <- list(...)

  for(i in seq_len(length(objs))){
    assertthat::assert_that(assertthat::is.string(objs[[i]]))
    assertthat::assert_that(objs[[i]] %in% c("true", "false"))
  }

  return(invisible(TRUE))
}


#'validate_param_list
#'@description Validator function for a parameter list or vector
#'@param param_list A list (or vector) of parameters
#'@param default_names How the list elements will be named as per default
validate_param_list <- function(param_list, default_names) {

  if(!is.list(param_list)  && !is.vector(param_list)){
    stop(paste("'param_list' parameter of function validate_param_list ",
               "must be a vector (it can be a list)."), call. = FALSE)
  }

  assertthat::assert_that(is.character(default_names))
  assertthat::assert_that(length(param_list) == length(default_names))

  sorted_param_names <- sort(names(param_list))
  sorted_default_names <- sort(default_names)

  if(is.null(names(param_list)) ||
     (!is.null(names(param_list)) &&
      any(sorted_param_names != sorted_default_names))){

    names(param_list) <- default_names

    message(paste0(
      "Renaming elements of ",
      deparse(quote(param_list)),
      " to fit their default names: '",
      paste(default_names, collapse = "', '")
    ))
  }

  return(invisible(param_list))
}


#'validate_wrapper_list
#'@description Helper function, checks if a lists consists only of elements of
#' a specific class
#'@param wrapper_list The list to check
#'@param expected_element_class The class each element of the wrapper list
#' should have
validate_wrapper_list <- function(wrapper_list, expected_element_class) {

  assertthat::assert_that(is.list(wrapper_list))

  lapply(wrapper_list, function(x){
    if(class(x) != expected_element_class){
      stop(paste("List has at least one element whose class is not",
                 expected_element_class),
           call. = FALSE)}
  })
}


#===== Validation helpers for optional parameters =====


#'validate_is_null_or_class_obj
#'@description Checks if an object is either null or a class object of class
#' 'class_name'
#'@param obj The object to check
#'@param class_name The name of the expected class
validate_is_null_or_class_obj <- function(obj, class_name){

  if(!is.null(obj)){
      assertthat::assert_that(class(obj) == class_name)
  }

  return(invisible(obj))
}


#'validate_is_null_or_numeric
#'@description Checks if an object is either null or numeric (helper to save
#' some typing when validating optional object parameters)
#'@param ... Ellipsis
validate_is_null_or_numeric <- function(...){

  objs <- list(...)

  for(i in seq_len(length(objects))){
    if(!is.null(objs[[i]])){
      assertthat::assert_that(is.numeric(objs[[i]]))
    }
  }

  return(invisible(TRUE))
}


#'validate_is_null_or_number
#'@description Checks if an object is either null or a number (helper to save
#' some typing when validating optional object parameters)
#'@param ... Ellipsis
validate_is_null_or_number <- function(...){

  objs <- list(...)

  for(i in seq_len(length(objects))){
    if(!is.null(objs[[i]])){
      assertthat::assert_that(assertthat::is.number(objs[[i]]))
    }
  }

  return(invisible(TRUE))
}


#'validate_is_null_or_string
#'@description Checks if an object is either null or a string (helper to save
#' some typing when validating optional object parameters)
#'@param ... Ellipsis
validate_is_null_or_string <- function(...){

  objs <- list(...)

  for(i in seq_len(length(objs))){
    if(!is.null(objs[[i]])){
      assertthat::assert_that(assertthat::is.string(objs[[i]]))
    }
  }

  return(invisible(TRUE))
}


#'validate_is_null_or_str_flag
#'@description Checks if an object is either null or a string reading either
#' "true" or "false"
#'@param ... Ellipsis
validate_is_null_or_str_flag <- function(...){

  objs <- list(...)

  for(i in seq_len(length(objs))){
    if(!is.null(objs[[i]])){
      validate_is_string_flag(objs[[i]])
    }
  }

  return(invisible(TRUE))
}


#'validate_param_list
#'@description Validator function for a parameter list or vector or NULL
#'@param obj A list (or vector) of parameters
#'@param default_names How the list elements will be named as per default
validate_is_null_or_param_list <- function(obj, default_names){

  if(!is.null(obj)){
    obj <- validate_param_list(obj, default_names)
  }

  return(invisible(obj))
}


#===== OTHERS =====


#Test if S3 object in R6 class inherits reference semantics

# A <- R6::R6Class("A",
#   public = list(
#
#       b_obj = NULL,
#
#       initialize = function(b_obj) {
#           self$b_obj <- b_obj
#         }
#   )
# )
#
# b <- function(x){
#     structure(x,
#               class = "b")
# }
#
# mod_func_a <- function(a_obj){
#     a_obj$b_obj$x <- 100
# }
#
# a_obj <- A$new(b(42))
#
# mod_func_a(a_obj)
#
# a_obj$b_obj$x
#
#
# mod_func_b <- function(b_obj){
#     b_obj$x <- 100
# }
#
# b_obj <- b(42)
#
# mod_func_b(b_obj)
#
# b_obj
