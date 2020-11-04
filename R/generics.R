


#============================== as_node ================================


#'as_node
#'@description S3 generic function, use to turn class data into XML friendly format
#'@param obj A class object (must have implementation for as_node)
as_node <- function(obj) {
    UseMethod("as_node")
}


#'as_node.default
#'@description Default implementation for S3 generic function as_node, just returns the object
#' passed to it (this may seem senseless, but is actually important for add_children
#' to work when as_node is called on values which are not r2ogs6 classes)
#'@param obj Any object
as_node.default <- function(obj){
    return(obj)
}


#============================== input_add ================================

#'S3 generic function, used to add input of different kinds to a OGS6 class object
#'@param obj A class object (must have implementation for input_add)
#'@param ogs6_obj The OGS6 class object the input should be added to
input_add <- function(obj, ogs6_obj) {

    assertthat::assert_that(inherits(ogs6_obj, "OGS6"))
    UseMethod("input_add")
}
