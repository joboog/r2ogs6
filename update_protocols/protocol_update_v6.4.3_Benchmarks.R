packrat::packrat_mode(on=T)
library(dplyr)
devtools::load_all(".") # load current r2ogs6 version

temppath <- "tmp/update_v6.4.3"


# top level tags -----------------------------------------------------------
# Both the old and new BM folders are supposed to be stored in the directory
# now check for new top level tags
old <- analyse_xml(path = paste0(temppath,"/Data_642"),
                   pattern = "\\.prj$",
                   xpath = "*",
                   print_findings = TRUE)

new <- analyse_xml(path = paste0(temppath,"/Data_643"),
                    pattern = "\\.prj$",
                    xpath = "*",
                    print_findings = TRUE)

ind <- names(new$both_sorted) %in% names(old$both_sorted)
names(new$both_sorted)[!ind]
# output: "number_of_components" "chemical_reactions"
# there are no new top-level tag
# "number_of_components" "chemical_reactions"  are now child tags of
# chemical_system



# top-level/child tags-------------------------------------------------------
keywords_lvl1 <- names(new$both_sorted)
change_df <- tibble::tibble(parent = character(), child = character(), new = logical())

# walk over every old level1 tag and check which child elements are found for
# old and new benchmarks (v6.4.1 and v6.4.2)
for (i in seq_along(keywords_lvl1)) {
    old <- analyse_xml(path = paste0(temppath,"/Data_642"),
                        pattern = "\\.prj$",
                        xpath = paste0("//", keywords_lvl1[i]),
                        print_findings = FALSE)$both_sorted %>% names

    new <- analyse_xml(path = paste0(temppath,"/Data_643"),
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

#new child: numerical_stabilization aperture_size
#of tag: process
## # # # # # # # # #
#new child: chemical_reaction
#of tag: chemical_reactions
## # # # # # # # # #
save(change_df, file = "tmp/update6.4.3_summary.rda")
print(change_df)


# work through list tag ----------------------------------------------------
# insert every tag/child into analyse_xml, look at xml files and implement
# changes in r2ogs6. At best, test if bm is read in correctly


# of tag: process -----------------------------------------------------
# new child: numerical_stabilization
lvl2_process <- analyse_xml(path = paste0(temppath,"/Data_643"),
            pattern = "\\.prj$",
            xpath = "//OpenGeoSysProject/processes/process",
            print_findings = TRUE)

analyse_xml(path = paste0(temppath,"/Data_643"),
            pattern = "\\.prj$",
            xpath = paste0("//OpenGeoSysProject/processes/process/",
                           "numerical_stabilization"),
            print_findings = TRUE)
# child element was added to class prj_process
# test
rm(test)
test <- OGS6$new("test", sim_path = paste0(temppath, "/test"))
bm <- paste0("Tests/Data/Parabolic/ComponentTransport/",
             "ClassicalTransportExample/classical_transport_example.prj")
read_in_prj(test, paste0(temppath,"/Data_643/", bm))
test$processes$HC$numerical_stabilization
# read of tag works!
dir_make_overwrite(test$sim_path)
run_benchmark(prj_path = paste0(temppath,"/Data_643/", bm),
              sim_path = test$sim_path)
# bm works!


# new child: aperture_size
analyse_xml(path = paste0(temppath,"/Data_643"),
            pattern = "\\.prj$",
            xpath = paste0("//OpenGeoSysProject/processes/process/",
                           "aperture_size"),
            print_findings = TRUE)
# child element was added to class prj_process
# test
rm(test)
test <- OGS6$new("test", sim_path = paste0(temppath, "/test"))
bm <- paste0("Tests/Data/Parabolic/LiquidFlow/InclinedMeshElements/1Din3D/",
             "line_fractures_in_3D.prj")
read_in_prj(test, paste0(temppath,"/Data_643/", bm))
test$processes$LiquidFlow$aperture_size
# read of tag works!
dir_make_overwrite(test$sim_path)
run_benchmark(prj_path = paste0(temppath,"/Data_643/", bm),
              sim_path = test$sim_path)
# bm works!



# of tag: chemical system ---------------------------------------------------
# new child: chemical_reactions
analyse_xml(path = paste0(temppath,"/Data_643"),
            pattern = "\\.prj$",
            xpath = "//chemical_system",
            print_findings = TRUE)

chem_reac <- analyse_xml(path = paste0(temppath,"/Data_643"),
            pattern = "\\.prj$",
            xpath = "//chemical_system/chemical_reactions/chemical_reaction",
            print_findings = TRUE)

generate_constructor(chem_reac, print_result = TRUE)
generate_helper(chem_reac, print_result = TRUE)
# execute data_raw/xpaths_for_classes.R
xpaths_for_classes[["prj_chemical_reaction"]]

# test
rm(test)
test <- OGS6$new("test", sim_path = paste0(temppath, "/test"))
bm <- paste0("Tests/Data/Parabolic/ComponentTransport/ReactiveTransport/",
             "DecayChain/GlobalImplicitApproach/1d_decay_chain_GIA.prj")
read_in_prj(test, paste0(temppath,"/Data_643/", bm))
test$chemical_system$chemical_reactions
test$chemical_system$chemical_reactions[[1]]
test$chemical_system$number_of_components
# read of tag works!
dir_make_overwrite(test$sim_path)
run_benchmark(prj_path = paste0(temppath,"/Data_643/", bm),
              sim_path = test$sim_path)
# works!

# Now try to run all OGS v6.4.3 benchmarks!