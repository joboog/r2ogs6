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

#'check_for_obj_of_name
#'@description Checks if a object with the given name was already defined for a ogs6 object and if not,
#' tells user to initialize one
#'@param ogs6_obj The ogs6 object to check
#'@param obj_name The name of the object to check for
check_for_obj_of_name <- function(ogs6_obj, obj_name) {

  if(!obj_name %in% names(ogs6_obj$sim_input)){
    stop(paste("There is no object named ", obj_name," for your ogs6 object yet.\n
               You can initialize one by calling input_add_", obj_name ,
               "() (read up on the required parameters)"),
         call. = FALSE)
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
#' @param objs A list of class objects (class must have method for generic function as_node)
adopt_nodes <- function(parent_name, objs) {
  parent_node <- list(parent_name = list())

  for(i in 1:length(objs)) {
    parent_node <- c(parent_node[[1]], as_node(objs[[i]]))
  }

  return(xml2::as_xml_document(parent_node))
}

#'add_opt_attr
#'@description Adds an optional attribute to a node attribute list
#'@param node The node the optional attribute should be added to
#'@param obj_parameter The value of the attribute to be added
#'@param attr_name The name of the attribute to be added
add_opt_attr <- function(node, obj_parameter, attr_name) {
  if(!is.null(obj_parameter)) {
    attributes(node[[1]])[[attr_name]] <- obj_parameter
  }

  return(node)
}


#'add_opt_child
#'@description Adds an optional child to a node child list
#'@param node The node the optional child should be added to
#'@param obj_parameter The value of the child to be added
#'@param child_name Optional: If it's a child node instead of just a value, the name of the child to be added
add_opt_child <- function(node, obj_parameter, child_name = NULL) {

  if(!is.null(obj_parameter)) {
    if(!is.null(child_name) && length(node[[1]]) == length(names(node[[1]]))) {
      node[[1]] <- c(node[[1]], list(child_name = obj_parameter))
    }else if(length(node[[1]]) == 0) {
      node[[1]] <- list(obj_parameter)
    }else{
      stop(paste("You're trying to add a value (an unnamed child node) to a node
                 which already has a value."), call. = FALSE)
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
