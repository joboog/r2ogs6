


#'S3 generic function, use to turn class data into XML friendly format
#'@param obj A class object (must have implementation for as_node)
as_node <- function(obj) {
    UseMethod("as_node")
}