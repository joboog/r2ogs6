# !!!!! READ the developer guide before updateing r2ogs6 !!!!!!
# https://gitlab.opengeosys.org/ogs/tools/r2ogs6/-/blob/master/vignettes/dev_workflow_vignette.Rmd

library(dplyr)
devtools::load_all(".") # load current r2ogs6 version

temppath <- "tmp/update_652"


# top level tags -----------------------------------------------------------
# Both the old and new BM folders are supposed to be stored in the directory
# now check for new top level tags
old <- analyse_xml(path = paste0(temppath,"/Data_644"),
                   pattern = "\\.prj$",
                   xpath = "*",
                   print_findings = TRUE)

new <- analyse_xml(path = paste0(temppath,"/Data_652"),
                    pattern = "\\.prj$",
                    xpath = "*",
                    print_findings = TRUE)

ind <- names(new$both_sorted) %in% names(old$both_sorted)
names(new$both_sorted)[!ind]
# output: "outputs" "use_high_precision" "raster"
# "raster" is a new top-level tag

# "use_high_precision" is a child tag of chemical_system, already implemented
# "outputs" is a child tag ot "time_loop" and a parent wrapper tag of
# "time_loop/output"




# add top-level tag "rasters"
analyse_xml(path = paste0(temppath,"/Data_652"),
                    pattern = "\\.prj$",
                    xpath = "//rasters",
                    print_findings = TRUE)

raster <- analyse_xml(path = paste0(temppath,"/Data_652"),
                    pattern = "\\.prj$",
                    xpath = "/OpenGeoSysProject/rasters/raster",
                    print_findings = TRUE)

generate_constructor(raster, print_result = TRUE)
generate_helper(raster, print_result = TRUE)

# new class prj_raster added

# execute data_raw/xpaths_for_classes.R
xpaths_for_classes[["prj_raster"]]

# add test with bm Parabolic/LiquidFlow/RasterParameter/GroundwaterModelWithRasterBC.prj









# top-level/child tags-------------------------------------------------------
keywords_lvl1 <- names(new$both_sorted)
change_df <- tibble::tibble(parent = character(), child = character(), new = logical())

# walk over every old level1 tag and check which child elements are found for
# old and new benchmarks
for (i in seq_along(keywords_lvl1)) {
    old <- analyse_xml(path = paste0(temppath,"/Data_644"),
                        pattern = "\\.prj$",
                        xpath = paste0("//", keywords_lvl1[i]),
                        print_findings = FALSE)$both_sorted %>% names

    new <- analyse_xml(path = paste0(temppath,"/Data_652"),
                         pattern = "\\.prj$",
                         xpath = paste0("//", keywords_lvl1[i]),
                         print_findings = FALSE)$both_sorted %>% names

    if (any(!new %in% old)) {
        change_df <- rbind(change_df,
                           tibble::tibble(parent = keywords_lvl1[i],
                                          child = new[!new %in% old],
                                          new = TRUE))
        cat("new child: ")
        cat(new[!new %in% old])
        cat("\nof tag: ")
        cat(keywords_lvl1[i])
        cat("\n")
        cat(rep("#", 10))
        cat("\n")
    }
    if (any(!old %in% new)) {
        change_df <- rbind(change_df,
                           tibble::tibble(parent = keywords_lvl1[i],
                                          child = old[!old %in% new],
                                          new = FALSE))
        cat("\n\n old child (not in new)\n")
        cat(old[!old %in% new])
        cat("\n of tag: ")
        cat(keywords_lvl1[i])
        cat("\n")
        cat(rep("#", 10))
        cat("\n")
    }
    rm(old, new)
}

# output:
# new child: output_extrapolation_residuals
# of tag: output
# # # # # # # # # # #
# new child: ice_constitutive_relation use_b_bar
# of tag: process
# # # # # # # # # # #


#  old child (not in new)
# dimension coupling_scheme_parameter
#  of tag: process
# # # # # # # # # # #
# new child: material_ids
# of tag: mesh
# # # # # # # # # # #
# new child: implicit
# of tag: basis_vector_0
# # # # # # # # # # #
# new child: output
# of tag: outputs
# # # # # # # # # # #
# new child: file variable dimension
# of tag: raster
# # # # # # # # # # #
save(change_df, file = "tmp/update6.5.2_summary.rda")
print(change_df)

# > print(change_df)
# # A tibble: 11 × 3
#    parent         child                          new  
#    <chr>          <chr>                          <lgl>
#  1 output         output_extrapolation_residuals TRUE 
#  2 process        ice_constitutive_relation      TRUE 
#  3 process        use_b_bar                      TRUE 
#  4 process        dimension                      FALSE
#  5 process        coupling_scheme_parameter      FALSE
#  6 mesh           material_ids                   TRUE 
#  7 basis_vector_0 implicit                       TRUE 
#  8 outputs        output                         TRUE 
#  9 raster         file                           TRUE 
# 10 raster         variable                       TRUE 
# 11 raster         dimension                      TRUE


# work through list tag ----------------------------------------------------
# insert every tag/child into analyse_xml, look at xml files and implement
# changes in r2ogs6. At best, test if bm is read in correctly



# new child: output_extrapolation_residuals
# of tag: output -----------------------------------------------------------

output <- analyse_xml(path = paste0(temppath,"/Data_652"),
            pattern = "\\.prj$",
            xpath = "//OpenGeoSysProject/time_loop/output",
            print_findings = TRUE)

analyse_xml(path = paste0(temppath,"/Data_652"),
            pattern = "\\.prj$",
            xpath = paste0("//OpenGeoSysProject/time_loop/output/",
                           "output_extrapolation_residuals"),
            print_findings = TRUE)

# child element was added to class prj_output


# test with bm Parabolic/ComponentTransport/ReactiveTransport/DecayChain/1d_decay_chain_OS.prj
# rm(test)
# test <- OGS6$new("test", sim_path = paste0(temppath, "/test"))
# bm <- paste0("Parabolic/LiquidFlow/SimpleSynthetics/PrimaryVariableConstraintDirichletBC/cuboid_1x1x1_hex_1000_Dirichlet_Dirichlet_1.prj")
# read_in_prj(test, paste0(temppath,"/Data_644/", bm))
# test$processes$LiquidFlow$linear
# # read of tag works!
# dir_make_overwrite(test$sim_path)
# run_benchmark(prj_path = paste0(temppath,"/Data_644/", bm),
#               sim_path = test$sim_path)





# new child: ice_constitutive_relation 
# of tag: process ---------------------------------------------------------

ice <- analyse_xml(path = paste0(temppath,"/Data_652"),
            pattern = "\\.prj$",
            xpath = "//OpenGeoSysProject/processes/process",
            print_findings = TRUE)

analyse_xml(path = paste0(temppath,"/Data_652"),
            pattern = "\\.prj$",
            xpath = paste0("//OpenGeoSysProject/processes/process/",
                           "ice_constitutive_relation"),
            print_findings = TRUE)

# class added to prj_process

# test with ThermoHydroMechanics/1D_freezing_column_Stefan/Stefan_problem.prj


# new child:  use_b_bar
# of tag: process ---------------------------------------------------------

use_b_bar <- analyse_xml(path = paste0(temppath,"/Data_652"),
            pattern = "\\.prj$",
            xpath = "//OpenGeoSysProject/processes/process",
            print_findings = TRUE)

analyse_xml(path = paste0(temppath,"/Data_652"),
            pattern = "\\.prj$",
            xpath = paste0("//OpenGeoSysProject/processes/process/",
                           "use_b_bar"),
            print_findings = TRUE)

# added as child to prj_process

# test with Mechanics/CooksMembrane/CooksMembrane.prj


# new child: material_ids
# of tag: mesh------------------------------------------------------------
analyse_xml(path = paste0(temppath,"/Data_652"),
            pattern = "\\.prj$",
            xpath = "//mesh",
            print_findings = TRUE)

analyse_xml(path = paste0(temppath,"/Data_652"),
            pattern = "\\.prj$",
            xpath = paste0("//OpenGeoSysProject/meshes/mesh/",
                           "material_ids"),
            print_findings = TRUE)

# could not find the correct xpath, 
# There is only match with /mesh/material_ids
# skip this one!


# new attribute: implicit
# of tag: basis_vector_0 ------------------------------------------------
analyse_xml(path = paste0(temppath,"/Data_652"),
            pattern = "\\.prj$",
            xpath = "//basis_vector_0",
            print_findings = TRUE)

implicit <- analyse_xml(path = paste0(temppath,"/Data_652"),
            pattern = "\\.prj$",
            xpath = paste0("//OpenGeoSysProject/local_coordinate_system/basis_vector_0"),
            print_findings = TRUE)

generate_constructor(implicit, print_result = TRUE)
generate_helper(implicit, print_result = TRUE)

# TODO: implement


# new child: output
# of tag: outputs
analyse_xml(path = paste0(temppath,"/Data_652"),
            pattern = "\\.prj$",
            xpath = "//outputs",
            print_findings = TRUE)

outputs <- analyse_xml(path = paste0(temppath,"/Data_652"),
            pattern = "\\.prj$",
            xpath = paste0("//OpenGeoSysProject/time_loop/outputs"),
            print_findings = TRUE)

# added as child to prj_timeloop
# added as potential parent to prj_output

# Need to test with Parabolic/HT/SimpleSynthetics/IsothermalFluidFlow.prj



# Now try to run all OGS v6.5.2 benchmarks!