packrat::packrat_mode(on=T)
library(dplyr)
devtools::load_all(".") # load current r2ogs6 version

temppath <- "tmp/update_v6.4.2"

# top level tags -----------------------------------------------------------
# Both the old and new BM folders are supposed to be stored in the directory
# now check for new top level tags
old <- analyse_xml(path = paste0(temppath,"/Data_641"),
                   pattern = "\\.prj$",
                   xpath = "*",
                   print_findings = TRUE)

new <- analyse_xml(path = paste0(temppath,"/Data_642"),
                    pattern = "\\.prj$",
                    xpath = "*",
                    print_findings = TRUE)

ind <- names(new$both_sorted) %in% names(old$both_sorted)
names(new$both_sorted)[!ind]
# there are no new top-level tag, however, the include tag now appears at
# top level with file as attribute
# !!! A function to handle top-level includes have been introduced and tested


# top-level/child tags-------------------------------------------------------
keywords_lvl1 <- names(old$both_sorted)
change_df <- tibble::tibble(parent = character(), child = character(), new = logical())

# walk over every old level1 tag and check which child elements are found for
# old and new benchmarks (v6.4.1 and v6.4.2)
for (i in seq_along(keywords_lvl1)) {
    old <- analyse_xml(path = paste0(temppath,"/Data_641"),
                        pattern = "\\.prj$",
                        xpath = paste0("//", keywords_lvl1[i]),
                        print_findings = FALSE)$both_sorted %>% names

    new <- analyse_xml(path = paste0(temppath,"/Data_642"),
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
# new child: compensate_non_equilibrium_initial_residuum
# of tag: process_variable
# # # # # # # # # # #
# new child: energy_split_model softening_curve characteristic_length
# coupling_scheme_parameter
# of tag: process
# # # # # # # # # # #
#
#
# old child (not in new)
# compensate_non_equilibrium_initial_residuum density_solid
# latent_heat_evaporation
# of tag: process
# # # # # # # # # # #
# new child: fixing_pe
# of tag: solution
# # # # # # # # # # #
# new child: site_unit
# of tag: surface
# # # # # # # # # # #

save(change_df, file = "tmp/update6.4.1_summary.rda")
#load("update6.4.1_summary.rda")
#print(change_df, n = 26)


# work through list tag ----------------------------------------------------
# insert every tag/child into analyse_xml, look at xml files and implement
# changes in r2ogs6. At best, test if bm is read in correctly

# of tag: process_variable -------------------------------------------------
# new child: compensate_non_equilibrium_initial_residuum
lvl2_process_var <- analyse_xml(path = paste0(temppath,"/Data_642"),
            pattern = "\\.prj$",
            xpath = "//OpenGeoSysProject/process_variables/process_variable",
            print_findings = TRUE)

analyse_xml(path = paste0(temppath,"/Data_642"),
            pattern = "\\.prj$",
            xpath = paste("//OpenGeoSysProject/process_variables/",
                          "process_variable/",
                          "compensate_non_equilibrium_initial_residuum"),
            print_findings = TRUE)
# child element was added to class prj_process_variable

# test
test <- OGS6$new("test", sim_path = paste0(temppath, "/test"))
bm <- "ThermoRichardsMechanics/TaskCDECOVALEX2023/Decovalex-0.prj"
read_in_prj(test, paste0(temppath,"/Data_642/", bm))
test$process_variables$displacement$compensate_non_equilibrium_initial_residuum
# read of tag works!
dir_make_overwrite(test$sim_path)
run_benchmark(prj_path = paste0(temppath,"/Data_642/", bm),
              sim_path = test$sim_path)
# execution of bm returned zero exit status!

# of tag: process -----------------------------------------------------
# new child: energy_split_model
lvl2_process <- analyse_xml(path = paste0(temppath,"/Data_642"),
            pattern = "\\.prj$",
            xpath = "//OpenGeoSysProject/processes/process",
            print_findings = TRUE)

analyse_xml(path = paste0(temppath,"/Data_642"),
            pattern = "\\.prj$",
            xpath = "//OpenGeoSysProject/processes/process/energy_split_model",
            print_findings = TRUE)
# child element was added to class prj_process
# test
rm(test)
test <- OGS6$new("test", sim_path = paste0(temppath, "/test"))
bm <- "PhaseField/beam/AT1_iso_tensile.prj"
read_in_prj(test, paste0(temppath,"/Data_642/", bm))
test$processes$PhaseField$energy_split_model
# read of tag works!
dir_make_overwrite(test$sim_path)
run_benchmark(prj_path = paste0(temppath,"/Data_642/", bm),
              sim_path = test$sim_path)
# bm does fail. OGS error message:
# critical: /usr/local/ogs/src/MathLib/LinAlg/Eigen/
# LinearSolverOptionsParser.h:49 parseNameAndOptions()
# error: OGS was compiled with Eigen but the config section in the project file
# seems to be invalid
# This is an error in the ogs-singularity version and will not be handled here


# child: softening_curve
# of tag: process
analyse_xml(path = paste0(temppath,"/Data_642"),
            pattern = "\\.prj$",
            xpath = "//OpenGeoSysProject/processes/process/softening_curve",
            print_findings = TRUE)
# child element was added to class prj_process
# test
rm(test)
test <- OGS6$new("test", sim_path = paste0(temppath, "/test"))
bm <- "PhaseField/beam/bar_COHESIVE_exponential.prj"
read_in_prj(test, paste0(temppath,"/Data_642/", bm))
test$processes$PhaseField$softening_curve
# read of tag works!
dir_make_overwrite(test$sim_path)
run_benchmark(prj_path = paste0(temppath,"/Data_642/", bm),
              sim_path = test$sim_path)
# bm does fail. OGS error message:
# critical: /usr/local/ogs/src/MathLib/LinAlg/Eigen/
# LinearSolverOptionsParser.h:49 parseNameAndOptions()
# error: OGS was compiled with Eigen but the config section in the project file
# seems to be invalid
# This is an error in the ogs-singularity version and will not be handled here



# child: characteristic_length
# of tag: process
analyse_xml(path = paste0(temppath,"/Data_642"),
        pattern = "\\.prj$",
        xpath = "//OpenGeoSysProject/processes/process/characteristic_length",
        print_findings = TRUE)
# child element was added to class prj_process
# test
rm(test)
test <- OGS6$new("test", sim_path = paste0(temppath, "/test"))
bm <- "PhaseField/beam/bar_COHESIVE_exponential.prj"
read_in_prj(test, paste0(temppath,"/Data_642/", bm))
test$processes$PhaseField$characteristic_length
# read of tag works!
dir_make_overwrite(test$sim_path)
run_benchmark(prj_path = paste0(temppath,"/Data_642/", bm),
              sim_path = test$sim_path)
# bm does fail. OGS error message:
# critical: /usr/local/ogs/src/MathLib/LinAlg/Eigen/
# LinearSolverOptionsParser.h:49 parseNameAndOptions()
# error: OGS was compiled with Eigen but the config section in the project file
# seems to be invalid
# This is an error in the ogs-singularity version and will not be handled here


#: child coupling_scheme_parameter
# of tag: process
analyse_xml(path = paste0(temppath,"/Data_642"),
    pattern = "\\.prj$",
    xpath = "//OpenGeoSysProject/processes/process/coupling_scheme_parameter",
    print_findings = TRUE)
# child element was added to class prj_process
# test
rm(test)
test <- OGS6$new("test", sim_path = paste0(temppath, "/test"))
bm <- "HydroMechanics/StaggeredScheme/MandelCryer/MandelCryerStaggered.prj"
read_in_prj(test, paste0(temppath,"/Data_642/", bm))
test$processes$MandelCryer$coupling_scheme_parameter
# read of tag works!
dir_make_overwrite(test$sim_path)
run_benchmark(prj_path = paste0(temppath,"/Data_642/", bm),
              sim_path = test$sim_path)


# old child (not in new)
# compensate_non_equilibrium_initial_residuum
# density_solid
# latent_heat_evaporation
# of tag: process
analyse_xml(path = paste0(temppath,"/Data_642"),
    pattern = "\\.prj$",
    xpath = "//OpenGeoSysProject/processes/process/latent_heat_evaporation",
    print_findings = TRUE)


# of tag: chemical system/solution---------------------------------------------
# new child: fixing_pe
analyse_xml(path = paste0(temppath,"/Data_642"),
            pattern = "\\.prj$",
            xpath = "//chemical_system/solution",
            print_findings = TRUE)
# test
rm(test)
test <- OGS6$new("test", sim_path = paste0(temppath, "/test"))
bm <- "Parabolic/ComponentTransport/ReactiveTransport/SurfaceComplexation/RadionuclideSorption_fixed_pe.prj"
read_in_prj(test, paste0(temppath,"/Data_642/", bm))
test$chemical_system$solution$fixing_pe
# read of tag works!
dir_make_overwrite(test$sim_path)
run_benchmark(prj_path = paste0(temppath,"/Data_642/", bm),
              sim_path = test$sim_path)


# new child: site_unit
chem_srf <- analyse_xml(path = paste0(temppath,"/Data_642"),
            pattern = "\\.prj$",
            xpath = "//chemical_system/surface",
            print_findings = TRUE)
# !!! Need to create class for surface tag
generate_constructor(chem_srf, print_result = TRUE)
generate_helper(chem_srf, print_result = TRUE)
# execute data_raw/xpaths_for_classes.R
xpaths_for_classes[["prj_surface"]]
# class created
# test
rm(test)
test <- OGS6$new("test", sim_path = paste0(temppath, "/test"))
bm <- "Parabolic/ComponentTransport/ReactiveTransport/SurfaceComplexation/RadionuclideSorption_fixed_pe.prj"
read_in_prj(test, paste0(temppath,"/Data_642/", bm))
test$chemical_system$surface
run_benchmark(prj_path = paste0(temppath,"/Data_642/", bm),
              sim_path = test$sim_path)
# works!