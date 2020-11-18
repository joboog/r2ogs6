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

#'validate_paths
#'@description Helper function to pull path validation out of already large class OGS6
#'@param sim_path The path where all relevant files for the simulation will be saved
#'@param ogs_bin_path Path to OpenGeoSys6 /bin directory
validate_paths <- function(sim_path, ogs_bin_path){
  if(!dir.exists(sim_path)){
    dir.create(sim_path)
  }else{
    if(length(dir(sim_path, all.files = TRUE)) != 0){
      warning(paste0("The sim_path directory you defined ('", sim_path,
                     "') already exists (that is ok). However, ",
                     "it is not empty. Files may be overwritten."), call. = FALSE)
    }
  }

  if(!file.exists(paste0(ogs_bin_path, "generateStructuredMesh.exe"))) {
    stop(paste("Could not find executable file generateStructuredMesh.exe at location",
               ogs_bin_path), call. = FALSE)
  }
}


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

  for(i in seq_len(length(wrapper_list))){
    if(class(wrapper_list[[i]]) != expected_element_class){
      stop(paste("List has at least one element whose class is not", expected_element_class),
           call. = FALSE)
    }
  }
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


#'simple_list_to_node
#'@description Helper to turn a simple vector into the corresponding node structure
#' with the vector elements as children. This works for lists too (as they are vectors).
#'@param parent_name The name of the parent node
#'@param simple_vector The vector to turn into the node structure
simple_vector_to_node <- function(parent_name, simple_vector){

  assertthat::assert_that(assertthat::is.string(parent_name))
  assertthat::assert_that(is.vector(simple_vector))

  for(i in seq_len(length(simple_vector))){
    if(length(simple_vector[[i]]) != 1){
      stop(paste("simple_vector_to_node 'simple_vector' parameter may only contain",
                 "atomic values!"), call. = FALSE)
    }
  }

  node <- list(structure(list()))
  names(node)[[1]] <- parent_name

  for(i in seq_len(length(simple_vector))){
    element_name <- names(simple_vector)[[i]]
    element_list <- list(list(simple_vector[[i]]))
    names(element_list)[[1]] <- element_name

    node[[1]] <- c(node[[1]], element_list)
  }

  return(invisible(node))
}


#'adopt_nodes
#'@description Takes a homogenous list of r2ogs6_* objects and creates a wrapper node
#' using the generic function as_node
#'@param parent_name The name of the new parent node
#'@param obj_list A list of class objects (class should have method for generic function as_node)
adopt_nodes <- function(parent_name, obj_list) {

  if(length(obj_list) == 0){
    return(invisible(NULL))
  }

  node <- list(list())
  names(node)[[1]] <- parent_name

  for(i in seq_len(length(obj_list))) {
    #cat(class(obj_list[[i]]), " ", obj_list[[i]], "\n")
    node[[1]] <- c(node[[1]], list(as_node(obj_list[[i]])))
  }

  return(invisible(node))
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

  assertthat::assert_that(is.list(node))
  assertthat::assert_that(is.list(children))

  for(i in seq_len(length(children))){

    child <- children[[i]]
    child_name <- names(children)[[i]]

    #If the child is a r2ogs6 class object, call as_node on it
    if(any(grepl("r2ogs6", class(child)))){
      node[[1]][[length(node[[1]]) + 1]] <- as_node(child)
      next
    }

    if(!is.null(child)) {
      #If the child is a wrapper, leave it alone
      if(is.list(child)){
        node[[1]][[length(node[[1]]) + 1]] <- child
      #If the child has a name
      }else if(!is.null(child_name) && child_name != "") {
        new_node <- as_node(child, child_name)
        node[[1]][[length(node[[1]]) + 1]] <- new_node
      }else{
        stop(paste("add_children: Trying to add an unnamed child which is not",
                   "already a node (list) or an r2ogs6_* class object"), call. = FALSE)
      }
    }
  }

  return(invisible(node))
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
