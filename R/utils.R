#This script contains some useful methods for a developer.


#============================== INFO UTILITY ================================


#'list_has_element
#'@description Helper function for get_status() to check if a list has at least one element.
#'@param obj_list The specified list
#'@param element_type Optional: What kind of elements are in the list?
list_has_element <- function(obj_list, element_type = "list element"){
  has_element <- FALSE

  if(length(obj_list) == 0){
    cat(crayon::red("\u2717"))
  }else{
    cat(crayon::green("\u2713"))
    has_element <- TRUE
  }

  cat(" At least one", element_type, "was defined", "\n")

  return(invisible(has_element))
}


#'obj_is_null
#'@description Helper function for get_status() to check if an object was defined.
#'@param obj The specified object
#'@param obj_type Optional: What kind of object is this?
obj_is_null <- function(obj, obj_type = ""){
  is_defined <- FALSE

  if(is.null(obj)){
    cat(crayon::red("\u2717"))
  }else{
    cat(crayon::green("\u2713"))
    is_defined <- TRUE
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
#'@param expected_class The class each element of the wrapper list should have
validate_wrapper_list <- function(wrapper_list, expected_element_class) {

  assertthat::assert_that(is.list(wrapper_list))

  for(i in seq_len(length(object_list))){
    if(class(object_list[[i]] != expected_class)){
      stop(paste("List ... has at least one element whose class is not", expected_element_class),
           call. = FALSE)
    }
  }
}


#============================== XML UTILITY ================================


#' export_xml_to_file
#' @description Export function
#' @param xml_data The data to be exported (already in XML friendly format)
#' @param file_name The name of the file to be written
# @examples
# export_xml_to_file(...)
export_xml_to_file <- function(xml_data, file_name) {
  doc <- xml2::as_xml_document(xml_data)
  xml2::write_xml(doc, file_name, options = "format", encoding="ISO-8859-1")
  invisible()
}


#' adopt_nodes
#' @description A helper function for creating parent nodes using the generic function as_node
#' @param parent_name The name of the new parent node
#' @param obj_list A list of class objects (class should have method for generic function as_node)
adopt_nodes <- function(parent_name, obj_list) {

  if(length(obj_list) == 0){
    return(invisible(NULL))
  }

  parent_node <- list(parent_name = list())

  for(i in seq_len(length(obj_list))) {
    parent_node <- c(parent_node[[1]], as_node(obj_list[[i]]))
  }

  return(invisible(parent_node))
}


#'add_attr
#'@description Adds an attribute to a node attribute list
#'@param node The node the attribute should be added to
#'@param obj_parameter The value of the attribute to be added
#'@param attr_name The name of the attribute to be added
add_attr <- function(node, obj_parameter, attr_name) {
  if(!is.null(obj_parameter)) {
    attributes(node[[1]])[[attr_name]] <- obj_parameter
  }

  return(invisible(node))
}


#'add_children
#'@description Adds one or more children to a node child list
#'@param node The node the children should be added to
#'@param children The children to be added (a partially named list)
add_children <- function(node, children) {

  assertthat::assert_that(is.list(children))

  value_added <- FALSE

  for(i in seq_len(length(node[[1]]))){
    if(names(node[[1]])[[i]] == ""){
      value_added <- TRUE
    }
  }

  for(i in seq_len(length(children))){
    child <- children[[i]]

    is_wrapper <- is.list(child)

    child_name <- names(children)[[i]]

    is_r2ogs6_obj <- any(grepl("r2ogs6", class(child)))

    if(is_r2ogs6_obj){
      child_name <- ""
    }

    if(!is.null(child)) {
      if(is_wrapper){
        node[[1]] <- c(node[[1]], child)
      }else if(!is.null(child_name) && child_name != "" && !value_added) {
        node[[1]] <- c(node[[1]], as_node(list(child_name = child)))
      }else if(!value_added && (length(node[[1]]) == 0 || is_r2ogs6_obj)){
        node[[1]] <- c(node[[1]], as_node(child))

        if(!is_r2ogs6_obj){
          value_added <- TRUE
        }

      }else{
        stop(paste("You're trying to add a value (an unnamed child node) to a node
                 which already has a value."), call. = FALSE)
      }
    }
  }

  return(invisible(node))
}




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
