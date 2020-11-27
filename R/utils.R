#This script contains some useful methods for a developer.


#============================== INFO UTILITY ================================


#'get_list_status
#'@description Helper function for get_status() to check if a list has at least one element.
#'@param flag Boolean flag to keep track of missing components
#'@param obj_list The specified list
#'@param element_type Optional: What kind of elements are in the list?
#'@param is_opt Does the list need at least one element?
get_list_status <- function(flag, obj_list, element_type = "list element", is_opt = FALSE){

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
#'@description Helper function for get_status() to check if an object was defined.
#'@param flag Boolean flag to keep track of missing components
#'@param obj The specified object
#'@param obj_type Optional: What kind of object is this?
obj_is_defined <- function(flag, obj, obj_type = ""){
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



#============================== VALIDATION UTILITY ================================


#'validate_param_list
#'@description Validator function for a parameter list
#'@param param_list A list of parameters
#'@param expected_length The expected list length
#'@param possible_names How the list elements may be named (if the user DID name them)
validate_param_list <- function(param_list, expected_length, possible_names) {

  if(!is.list(param_list)){
    stop("Argument param_list passed to validate_param_list must be a list", call. = FALSE)
  }

  if(length(param_list) != expected_length){
    stop(paste(deparse(quote(param_list)), "must be a list of length", expected_length),
         call. = FALSE)
  }

  if(!is.null(names(param_list)) &&
     names(param_list) != possible_names){
    stop(paste0("If you do name the elements of ", deparse(quote(param_list)), ", stick to their default
              values to avoid confusion: '", paste(possible_names, collapse="', '"), "'"),
         call. = FALSE)
  }
}


#'validate_wrapper_list
#'@description Helper function, checks if a lists consists only of elements of a specific class
#'@param wrapper_list The list to check
#'@param expected_element_class The class each element of the wrapper list should have
validate_wrapper_list <- function(wrapper_list, expected_element_class) {

  assertthat::assert_that(is.list(wrapper_list))

  lapply(wrapper_list, function(x){
    if(class(x) != expected_element_class){
      stop(paste("List has at least one element whose class is not", expected_element_class),
         call. = FALSE)}
    })
}


#============================== XML UTILITY ================================


#'get_value_types
#'@description Gets the type of an XML value based on the documentation
#' (per default, XML values are read in as a string, but for many elements,
#' we want to coerce them to double)
#'@param xml_node An XML node (of class xml2::xml_node)
get_value_types <- function(xml_node) {

  #WIP! Could be a nice utility function.

  return(invisible("String"))
}


#============================== OTHERS ================================




#================================Test if S3 object in R6 class inherits reference semantics

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
