# !!!!! READ the developer guide before updateing r2ogs6 !!!!!!
# https://gitlab.opengeosys.org/ogs/tools/r2ogs6/-/blob/master/vignettes/dev_workflow_vignette.Rmd
library(dplyr)
devtools::load_all(".") # load current r2ogs6 version

temppath <- "tmp/update_v6.4.4"


# top level tags -----------------------------------------------------------
# Both the old and new BM folders are supposed to be stored in the directory
# now check for new top level tags
old <- analyse_xml(path = paste0(temppath,"/Data_643"),
                   pattern = "\\.prj$",
                   xpath = "*",
                   print_findings = TRUE)

new <- analyse_xml(path = paste0(temppath,"/Data_644"),
                    pattern = "\\.prj$",
                    xpath = "*",
                    print_findings = TRUE)

ind <- names(new$both_sorted) %in% names(old$both_sorted)
names(new$both_sorted)[!ind]
# output: "submesh_residuum_output"
# there are no new top-level tag

# "submesh_residuum_output" is child tags of
# time_loop



# top-level/child tags-------------------------------------------------------
keywords_lvl1 <- names(new$both_sorted)
change_df <- tibble::tibble(parent = character(), child = character(), new = logical())

# walk over every old level1 tag and check which child elements are found for
# old and new benchmarks (v6.4.3 and v6.4.4)
for (i in seq_along(keywords_lvl1)) {
    old <- analyse_xml(path = paste0(temppath,"/Data_643"),
                        pattern = "\\.prj$",
                        xpath = paste0("//", keywords_lvl1[i]),
                        print_findings = FALSE)$both_sorted %>% names

    new <- analyse_xml(path = paste0(temppath,"/Data_644"),
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

# new child: geometrical_sets
# of tag: output
# # # # # # # # # # #
# new child: linear pressurized_crack_scheme subtype apply_body_force_for_deformation
# of tag: process
# # # # # # # # # # #
# new child: type timesteps variables meshes prefix suffix
# of tag: submesh_residuum_output
# # # # # # # # # # #
save(change_df, file = "tmp/update6.4.4_summary.rda")
print(change_df)

# > print(change_df)
# # A tibble: 11 × 3
#    parent                  child                            new
#    <chr>                   <chr>                            <lgl>
#  1 output                  geometrical_sets                 TRUE
#  2 process                 linear                           TRUE
#  3 process                 pressurized_crack_scheme         TRUE
#  4 process                 subtype                          TRUE
#  5 process                 apply_body_force_for_deformation TRUE
#  6 submesh_residuum_output type                             TRUE
#  7 submesh_residuum_output timesteps                        TRUE
#  8 submesh_residuum_output variables                        TRUE
#  9 submesh_residuum_output meshes                           TRUE
# 10 submesh_residuum_output prefix                           TRUE
# 11 submesh_residuum_output suffix                           TRUE


# work through list tag ----------------------------------------------------
# insert every tag/child into analyse_xml, look at xml files and implement
# changes in r2ogs6. At best, test if bm is read in correctly


# of tag: process -----------------------------------------------------
lvl2_process <- analyse_xml(path = paste0(temppath,"/Data_644"),
            pattern = "\\.prj$",
            xpath = "//OpenGeoSysProject/processes/process",
            print_findings = TRUE)

# new child: linear
analyse_xml(path = paste0(temppath,"/Data_644"),
            pattern = "\\.prj$",
            xpath = paste0("//OpenGeoSysProject/processes/process/",
                           "linear"),
            print_findings = TRUE)

# child element was added to class prj_process
# test
rm(test)
test <- OGS6$new("test", sim_path = paste0(temppath, "/test"))
bm <- paste0("Parabolic/LiquidFlow/SimpleSynthetics/PrimaryVariableConstraintDirichletBC/cuboid_1x1x1_hex_1000_Dirichlet_Dirichlet_1.prj")
read_in_prj(test, paste0(temppath,"/Data_644/", bm))
test$processes$LiquidFlow$linear
# read of tag works!
dir_make_overwrite(test$sim_path)
run_benchmark(prj_path = paste0(temppath,"/Data_644/", bm),
              sim_path = test$sim_path)
# bm works!


# new child: pressurized_crack_scheme
analyse_xml(path = paste0(temppath,"/Data_644"),
            pattern = "\\.prj$",
            xpath = paste0("//OpenGeoSysProject/processes/process/",
                           "pressurized_crack_scheme"),
            print_findings = TRUE)

# child element was added to class prj_process
# test
rm(test)
test <- OGS6$new("test", sim_path = paste0(temppath, "/test"))
bm <- paste0("PhaseField/k_regime_HF/2D_bm_0p01.prj")
read_in_prj(test, paste0(temppath,"/Data_644/", bm))
test$processes$PhaseField$pressurized_crack_scheme
# read of tag works!
dir_make_overwrite(test$sim_path)
run_benchmark(prj_path = paste0(temppath,"/Data_644/", bm),
              sim_path = test$sim_path)
# bm does not work due to problems with petsc solver on my machine


# new child: subtype
analyse_xml(path = paste0(temppath,"/Data_644"),
            pattern = "\\.prj$",
            xpath = paste0("//OpenGeoSysProject/processes/process/",
                           "subtype"),
            print_findings = TRUE)

# child element was added to class prj_process
# test
rm(test)
test <- OGS6$new("test", sim_path = paste0(temppath, "/test"))
bm <- paste0("ThermoRichardsMechanics/LinearMechanics/mechanics_linear.prj")
read_in_prj(test, paste0(temppath,"/Data_644/", bm))
test$processes$RM$subtype
# read of tag works!
dir_make_overwrite(test$sim_path)
run_benchmark(prj_path = paste0(temppath,"/Data_644/", bm),
              sim_path = test$sim_path)
# bm works


# new child: apply_body_force_for_deformation
analyse_xml(path = paste0(temppath,"/Data_644"),
            pattern = "\\.prj$",
            xpath = paste0("//OpenGeoSysProject/processes/process/",
                           "apply_body_force_for_deformation"),
            print_findings = TRUE)

# child element was added to class prj_process
# test
rm(test)
test <- OGS6$new("test", sim_path = paste0(temppath, "/test"))
bm <- paste0("ThermoRichardsMechanics/BodyForce/square.prj")
read_in_prj(test, paste0(temppath,"/Data_644/", bm))
test$processes$BodyForceTest$apply_body_force_for_deformation
# read of tag works!
dir_make_overwrite(test$sim_path)
run_benchmark(prj_path = paste0(temppath,"/Data_644/", bm),
              sim_path = test$sim_path)
# bm works


# of tag: output -----------------------------------------------------
lvl2_output <- analyse_xml(path = paste0(temppath,"/Data_644"),
            pattern = "\\.prj$",
            xpath = "//OpenGeoSysProject/time_loop/output",
            print_findings = TRUE)

# new child: apply_body_force_for_deformation
analyse_xml(path = paste0(temppath,"/Data_644"),
            pattern = "\\.prj$",
            xpath = paste0("//OpenGeoSysProject/time_loop/output/",
                           "geometrical_sets"),
            print_findings = TRUE)
# generate constructure
geometrical_set_res <- analyse_xml(path = paste0(temppath,"/Data_644"),
            pattern = "\\.prj$",
            xpath = paste0("//OpenGeoSysProject/time_loop/output/",
                           "geometrical_sets/geometrical_set"),
            print_findings = TRUE)
generate_constructor(params = geometrical_set_res,
                     print_result = TRUE)

generate_helper(params = geometrical_set_res,
                print_result = TRUE)

# child element was added to class prj_geometrical_set

# test
rm(test)
test <- OGS6$new("test", sim_path = paste0(temppath, "/test"))
bm <- paste0("Elliptic/cube_1x1x1_SteadyStateDiffusion/cube_1e0_processed.prj")
read_in_prj(test, paste0(temppath,"/Data_644/", bm))
test$time_loop$output$geometrical_sets %>% length()
# read of tag works!
dir_make_overwrite(test$sim_path)
run_benchmark(prj_path = paste0(temppath,"/Data_644/", bm),
              sim_path = test$sim_path)
# bm works


# of tag: time_loop -----------------------------------------------------
lvl2_submesh_residuum_output <- analyse_xml(
                            path = paste0(temppath,"/Data_644"),
                           pattern = "\\.prj$",
                           xpath = "//OpenGeoSysProject/time_loop/submesh_residuum_output",
                           print_findings = TRUE)

# submesh_residuum_output is very similar to time_loop/output
# https://doxygen.opengeosys.org/v6.4.4/d1/d94/ogs_file_param__prj__time_loop__submesh_residuum_output.html
# thats why prj_submesh_residuum_output was set up similar to prj_output

# test
rm(test)
test <- OGS6$new("test", sim_path = paste0(temppath, "/test"))
bm <- paste0("ThermoRichardsMechanics/PointHeatSource/point_heat_source_2D.prj")
read_in_prj(test, paste0(temppath,"/Data_644/", bm))
test$time_loop$submesh_residuum_output
# read of tag works!
dir_make_overwrite(test$sim_path)
run_benchmark(prj_path = paste0(temppath,"/Data_644/", bm),
              sim_path = test$sim_path)
# bm works


# Now try to run all OGS v6.4.4 benchmarks!