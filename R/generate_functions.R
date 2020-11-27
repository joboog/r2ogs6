#This is a script containing experimental functions which generate other functions.
#These will not be exported, but in the best case developers can use them to make their workflow more efficient.

#Experimental stuff
generate_from_element <- function(element_name, attrs, children) {


}



generate_class_from_element <- function(element, export = TRUE) {

    children <- xml2::xml_children(element)
    attrs <- xml2::xml_attrs(element)
    element_name <- xml2::xml_name(element)

    #Generate name of new class from element name if class_name is not specified
    if(is.null(class_name)){
        class_name <- snakecase::to_any_case(element_name, "snake")
    }


    #Start of description
    class_str <- paste0("#'r2ogs6_", element_name, "\n",
                        "#'@description r2ogs6 class representing a ", element_name, " element \n")

    #Add parameter documentation tag(s)
    parameters <- list()

    for(i in seq_len(length(children))){
        parameters <- c(parameters, xml2::xml_name(children[[i]]))

        class_str <- paste0(class_str, "#'@param ", xml2::xml_name(children[[i]]), " (Auto generated)\n")
    }

    #Add export documentation tag if class should be exported
    if(export){
        class_str <- paste0(class_str, "#'@export\n")
    }

    #Start of declaration
    class_str <- paste0(class_str, "r2ogs6_", element_name, " <- function(\n")

    #ADD PARAMETERS HERE

    class_str <- paste0(class_str, ") {\n")

    #ADD FUNCTION CONTENTS

    class_str <- paste0(class_str, "}\n")


    generated_class <- structure(list(), class = element_name)

    return(generated_class)
}

generate_validator_from_element <- function() {

}


#============================== READ_IN FUNCTION GENERATOR ================================

#'generate_simple_read_in
#'@description Assuming function read_in gets good enough results, this could save code later.
#'@param element_name The name of the .prj element to be read from (wrapper element, e.g. 'processes')
#'@param child_name The name of the element children (e.g. 'process')
#'@param has_name_tag Do the child elements have a child element with the name 'name'?
#'@param subclasses_names Optional: A character vector containing the names of r2ogs6_*
#' subclasses (r2ogs6_* classes without a method for input_add)
generate_simple_read_in <- function(element_name, child_name,
                                    has_name_tag = TRUE, subclasses_names = NULL){

    assertthat::assert_that(assertthat::is.string(element_name))
    assertthat::assert_that(assertthat::is.string(child_name))
    assertthat::assert_that(assertthat::is.flag(has_name_tag))

    if(!is.null(subclasses_names)){
        assertthat::assert_that(is.character(subclasses_names))
    }


    func_str <- paste0("#'read_in_", element_name, "\n",
                       "#'@description Reads in ", child_name, " elements from a .prj file\n",
                       "#'@param ogs6_obj A OGS6 class object\n",
                       "#'@param prj_path The path to the project file the ", child_name,
                       " elements should be read from\n")

    if(has_name_tag){
        func_str <- paste0(func_str, "#'@param ", child_name, "_names Optional: The names of the ",
                           child_name, " elements to be read in\n")
    }else{
        func_str <- paste0(func_str, "#'@param ", child_name, "_indices Optional: The indices of the ",
                           child_name, " elements to be read in\n")
    }

    func_str <- paste0(func_str, "#'@export\n",
                       "read_in_", element_name, " <- function(ogs6_obj, prj_path, ")

    if(has_name_tag){
        func_str <- paste0(func_str, child_name, "_names = NULL) {\n")
    }else{
        func_str <- paste0(func_str, child_name, "_indices = NULL) {\n")
    }

    func_str <- paste0(func_str, "read_in(ogs6_obj, prj_path, \"", element_name,
                       "\", \"", child_name, "\", selection_vector = ")

    if(has_name_tag){
        func_str <- paste0(func_str, child_name, "_names")
    }else{
        func_str <- paste0(func_str, child_name, "_indices")
    }

    if(!is.null(subclasses_names)){
        subclasses_str <- utils::capture.output(invisible(dput(subclasses_names)))
        subclasses_str <- paste(subclasses_str, collapse = "")
        func_str <- paste0(func_str, ", subclasses_names = ", subclasses_str)
    }

    func_str <- paste0(func_str,
                       ")\n",
                       "}\n")

    cat(func_str)
    return(invisible(func_str))
}