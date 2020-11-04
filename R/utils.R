#============================== GENERAL UTILITY (copy playground) ================================

###Stub
# Stub <- R6::R6Class("Stub",
#   public = list(
#
#     initialize = function(argument) {
#       #stopifnot(is.character(name), length(name) == 1)
#       #...
#     }
#   ),
#
#   active = list(
#
#   ),
#
#   private = list(
#
#   )
# )


#============================== VALIDATION UTILITY ================================


#'validate_param_list
#'@description Validator function for a parameter list
#'@param param_list A list of parameters
#'@param expected_length The expected list length
#'@param possible_names How the list elements may be named (if the user DID name them)
validate_param_list <- function(param_list, expected_length, possible_names) {

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


#'validate_list_element_classes
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



#'check_for_input_of_name
#'@description Checks if a object with the given name was already defined for a ogs6 object and if not,
#' tells user to initialize one
#'@param ogs6_obj The ogs6 object to check
#'@param input_name The name of the input to check for
#'@param input_expected Should the input exist?
#'@param do_stop Should execution be halted or should a boolean be returned?
#'@param init_func_name Optional: The name of the function the object can be initialized with
check_for_input_of_name <- function(ogs6_obj, input_name, input_expected, do_stop, init_func_name = NULL) {

  if(!input_name %in% names(ogs6_obj$sim_input) && input_expected){
    if(do_stop){
      stop(paste0("There is no input named ", input_name," for your ogs6 object yet.\n
                  You can initialize one by calling ", init_func_name ,
                  ". (read up on the required parameters)"),
           call. = FALSE)
    }else{
      return(FALSE)
    }

  }else if(input_name %in% names(ogs6_obj$sim_input) && !input_expected) {
    if(do_stop){
      stop(paste0("There is already an input named ", input_name," for your ogs6 object.\n
                  You probably added it by calling ", init_func_name , "."),
           call. = FALSE)
    }else{
      return(FALSE)
    }
  }

  return(TRUE)
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
#' @param objs A list of class objects (class must have method for generic function as_node)
adopt_nodes <- function(parent_name, objs) {
  parent_node <- list(parent_name = list())

  for(i in 1:length(objs)) {
    parent_node <- c(parent_node[[1]], as_node(objs[[i]]))
  }

  return(parent_node)
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

  return(node)
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

  return(node)
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
