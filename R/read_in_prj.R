#Functions to read in data from a .prj file to an OGS6 object


#'read_in_prj
#'@description Wrapper function to read in a whole .prj file
#'@param ogs6_obj A OGS6 class object
#'@param prj_path The path to the project file that should be read in
#'@export
read_in_prj <- function(ogs6_obj, prj_path){

    assertthat::assert_that("OGS6" %in% class(ogs6_obj))
    xml_doc <- validate_read_in_xml(prj_path)

    from_other_path <- (dirname(ogs6_obj$sim_path) != dirname(prj_path))

    #Geometry reference
    gml_ref_node <- xml2::xml_find_first(xml_doc, "//geometry")

    #Meshes references
    vtu_ref_nodes <- NULL

    if(class(gml_ref_node) != "xml_missing"){
        gml_path <- paste0(dirname(prj_path), "/", xml2::xml_text(gml_ref_node))
        read_in_gml(ogs6_obj, gml_path)
        vtu_ref_nodes <- xml2::xml_find_all(xml_doc, "//mesh")
    }else{
        vtu_ref_nodes <- xml2::xml_find_all(xml_doc, "//meshes/*")
    }

    for(i in seq_along(vtu_ref_nodes)){
        vtu_ref <- xml2::xml_text(vtu_ref_nodes[[i]])
        ogs6_obj$add_mesh(r2ogs6_mesh(vtu_ref))

        if(from_other_path){
            #Copy file into ogs6_obj$sim_path folder
            file.copy(paste0(dirname(prj_path), "/", vtu_ref), ogs6_obj$sim_path)
        }
    }

    read_in_processes(ogs6_obj, prj_path)
    read_in_media(ogs6_obj, prj_path)
    read_in_time_loop(ogs6_obj, prj_path)
    read_in_parameters(ogs6_obj, prj_path)
    read_in_curves(ogs6_obj, prj_path)
    read_in_process_variables(ogs6_obj, prj_path)
    read_in_nonlinear_solvers(ogs6_obj, prj_path)
    read_in_linear_solvers(ogs6_obj, prj_path)
    read_in_test_definition(ogs6_obj, prj_path)
}


#============================== AUTO GENERATED STUBS ================================


#'read_in_processes
#'@description Reads in process elements from a .prj file
#'@param ogs6_obj A OGS6 class object
#'@param prj_path The path to the project file the process elements should be read from
#'@param process_names Optional: The names of the process elements to be read in
#'@export
read_in_processes <- function(ogs6_obj, prj_path, process_names = NULL) {
    read_in(ogs6_obj, prj_path, "processes", "process", selection_vector = process_names)
}


#'read_in_media
#'@description Reads in medium elements from a .prj file
#'@param ogs6_obj A OGS6 class object
#'@param prj_path The path to the project file the medium elements should be read from
#'@param medium_indices Optional: The indices of the medium elements to be read in
#'@export
read_in_media <- function(ogs6_obj, prj_path, medium_indices = NULL) {
    read_in(ogs6_obj, prj_path, "media", "medium", selection_vector = medium_indices,
            subclasses_names = c(phase = "r2ogs6_medium_phase", property = "r2ogs6_medium_property"))
}


#'read_in_time_loop
#'@description Reads in time_loop element from a .prj file
#'@param ogs6_obj A OGS6 class object
#'@param prj_path The path to the project file the time_loop element should be read from
#'@export
read_in_time_loop <- function(ogs6_obj, prj_path) {
    read_in(ogs6_obj, prj_path, "time_loop", "time_loop",
            selection_vector = NULL,
            subclasses_names = c(process = "r2ogs6_tl_process",
                                 output = "r2ogs6_tl_output",
                                 global_processes_coupling = "r2ogs6_global_processes_coupling"))
}


#'read_in_parameters
#'@description Reads in parameter elements from a .prj file
#'@param ogs6_obj A OGS6 class object
#'@param prj_path The path to the project file the parameter elements should be read from
#'@param parameter_names Optional: The names of the parameter elements to be read in
#'@export
read_in_parameters <- function(ogs6_obj, prj_path, parameter_names = NULL) {
    read_in(ogs6_obj, prj_path, "parameters", "parameter", selection_vector = parameter_names)
}


#'read_in_curves
#'@description Reads in curve elements from a .prj file
#'@param ogs6_obj A OGS6 class object
#'@param prj_path The path to the project file the curve elements should be read from
#'@param curve_names Optional: The names of the curve elements to be read in
#'@export
read_in_curves <- function(ogs6_obj, prj_path, curve_names = NULL) {
    read_in(ogs6_obj, prj_path, "curves", "curve",
            selection_vector = curve_names)
}


#'read_in_process_variables
#'@description Reads in process_variable elements from a .prj file
#'@param ogs6_obj A OGS6 class object
#'@param prj_path The path to the project file the process_variable elements should be read from
#'@param process_variable_names Optional: The names of the process_variable elements to be read in
#'@export
read_in_process_variables <- function(ogs6_obj, prj_path, process_variable_names = NULL) {
    read_in(ogs6_obj, prj_path, "process_variables", "process_variable",
            selection_vector = process_variable_names,
            subclasses_names = c(boundary_condition = "r2ogs6_boundary_condition"))
}


#'read_in_nonlinear_solvers
#'@description Reads in nonlinear_solver elements from a .prj file
#'@param ogs6_obj A OGS6 class object
#'@param prj_path The path to the project file the nonlinear_solver elements should be read from
#'@param nonlinear_solver_names Optional: The names of the nonlinear_solver elements to be read in
#'@export
read_in_nonlinear_solvers <- function(ogs6_obj, prj_path, nonlinear_solver_names = NULL) {
    read_in(ogs6_obj, prj_path, "nonlinear_solvers", "nonlinear_solver",
            selection_vector = nonlinear_solver_names)
}


#'read_in_linear_solvers
#'@description Reads in linear_solver elements from a .prj file
#'@param ogs6_obj A OGS6 class object
#'@param prj_path The path to the project file the linear_solver elements should be read from
#'@param linear_solver_names Optional: The names of the linear_solver elements to be read in
#'@export
read_in_linear_solvers <- function(ogs6_obj, prj_path, linear_solver_names = NULL) {
    read_in(ogs6_obj, prj_path, "linear_solvers", "linear_solver",
            selection_vector = linear_solver_names)
}


#'read_in_test_definition
#'@description Reads in vtkdiff elements from a .prj file
#'@param ogs6_obj A OGS6 class object
#'@param prj_path The path to the project file the vtkdiff elements should be read from
#'@param vtkdiff_indices Optional: The indices of the vtkdiff elements to be read in
#'@export
read_in_test_definition <- function(ogs6_obj, prj_path, vtkdiff_indices = NULL) {
    read_in(ogs6_obj, prj_path, "test_definition", "vtkdiff",
            selection_vector = vtkdiff_indices)
}


#============================== SPECIAL CASES ================================
#This part is for read_in functions dealing with elements that need special
#handling because they have a structure that guess_structure from read_in_utils.R
#doesn't know how to deal with

#'read_in_timesteps_node
#'@description Reads in a timesteps node
#'@param timesteps_node The timesteps node
read_in_timesteps_node <- function(timesteps_node){

    timesteps_list <- list()

    for(i in seq_along(xml2::xml_children(timesteps_node))){
        pair_node <- xml2::xml_children(timesteps_node)[[i]]

        val_1 <- xml2::xml_double(xml2::xml_children(pair_node)[[1]])
        val_2 <- xml2::xml_double(xml2::xml_children(pair_node)[[2]])

        timesteps_list <- c(timesteps_list, list(pair = c(val_1, val_2)))
    }

    return(invisible(timesteps_list))
}

