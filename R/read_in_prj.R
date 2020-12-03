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
        gml_path <- paste0(dirname(prj_path), "/",
                           xml2::xml_text(gml_ref_node))
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
            file.copy(paste0(dirname(prj_path), "/",
                             vtu_ref), ogs6_obj$sim_path)
        }
    }

    read_in(ogs6_obj, prj_path, "processes", "process")

    read_in(ogs6_obj, prj_path, "search_length_algorithm",
            "search_length_algorithm")

    read_in(ogs6_obj, prj_path, "media", "medium",
            subclasses_names = c(phase = "r2ogs6_medium_phase",
                                 property = "r2ogs6_medium_property"))

    read_in(ogs6_obj, prj_path, "time_loop", "time_loop",
            subclasses_names =
                c(process = "r2ogs6_tl_process",
                  output = "r2ogs6_tl_output",
                  global_processes_coupling =
                      "r2ogs6_global_processes_coupling"))

    read_in(ogs6_obj, prj_path, "local_coordinate_system",
            "local_coordinate_system")

    read_in(ogs6_obj, prj_path, "parameters", "parameter")

    read_in(ogs6_obj, prj_path, "curves", "curve")

    read_in(ogs6_obj, prj_path, "process_variables", "process_variable",
            subclasses_names = c(boundary_condition =
                                     "r2ogs6_boundary_condition"))

    read_in(ogs6_obj, prj_path, "nonlinear_solvers", "nonlinear_solver")

    read_in(ogs6_obj, prj_path, "linear_solvers", "linear_solver")

    read_in(ogs6_obj, prj_path, "test_definition", "vtkdiff")

    read_in(ogs6_obj, prj_path, "insitu", "insitu")
}


#===== SPECIAL CASES =====

#This part is for read_in functions dealing with elements that need special
#handling because they have a structure that guess_structure
#from read_in_utils.R doesn't know how to deal with

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

