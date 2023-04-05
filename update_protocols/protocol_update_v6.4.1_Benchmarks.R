
library(dplyr)
devtools::load_all(".") # load current r2ogs6 version

# level1 ------------------------------------------------------------------
# Both the old and new BM folders are supposed to be stored in the directory
# above the current wd

old <- analyse_xml(path = "../ogs-6.4.0-Tests-Data",
                   pattern = "\\.prj$",
                   xpath = "*",
                   print_findings = TRUE)

new <- analyse_xml(path = "../ogs-6.4.1-Tests-Data",
                    pattern = "\\.prj$",
                    xpath = "*",
                    print_findings = TRUE)

ind <- names(new$both_sorted) %in% names(old$both_sorted)
names(new$both_sorted)[!ind] # new keyword!
lvl1 <- analyse_xml(path = "../ogs-6.4.1-Tests-Data",
            pattern = "\\.prj$",
            xpath = "//chemical_system/exchangers",
            print_findings = TRUE)
# actually, this was included as a new subclass of "chemical_system" later
# level2 ------------------------------------------------------------------

keywords_lvl1 <- names(old$both_sorted)
change_df <- tibble::tibble(parent = character(), child = character(), new = logical())

# walk over every old level1 keyword and check which child elements are found for
# old and new benchmarks (v6.4.0 and v6.4.1)
for (i in seq_along(keywords_lvl1)) {
    old <- analyse_xml(path = "../ogs-6.4.0-Tests-Data",
                       pattern = "\\.prj$",
                       xpath = paste0("//", keywords_lvl1[i]),
                       print_findings = FALSE)$both_sorted %>% names

    new <- analyse_xml(path = "../ogs-6.4.1-Tests-Data",
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
        cat("\nof keyword: ")
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
        cat("\n of keyword: ")
        cat(keywords_lvl1[i])
        cat("\n")
        cat(rep("#", 10))
        cat("\n")
    }
}
save(change_df, file = "tmp/update6.4.1_summary.rda")
#load("update6.4.1_summary.rda")
print(change_df, n = 26)


# work through list -------------------------------------------------------
# insert every keyword/child into analyse_xml, look at xml files and implement
# changes in r2ogs6. At best, test if bm is read in correctly
lvl2_hdf <- analyse_xml(path = "../ogs-6.4.1-Tests-Data",
                    pattern = "\\.prj$",
                    xpath = "//time_loop/output/hdf",
                    print_findings = TRUE)
# inserted as argument in prj_time_loop.R
test <- OGS6$new("test", sim_path = "tmp")
read_in_prj(test, "../ogs-6.4.1-Tests-Data/Tests/Data/EllipticPETSc/cube_1e3_XDMF_np3_2files.prj")
test$time_loop$output$hdf

analyse_xml(path = "../ogs-6.4.1-Tests-Data",
            pattern = "\\.prj$",
            xpath = "//parameter/range",
            print_findings = TRUE)
analyse_xml(path = "../ogs-6.4.1-Tests-Data",
            pattern = "\\.prj$",
            xpath = "//parameter/seed",
            print_findings = TRUE)
# inserted in prj_parameter.R
test <- OGS6$new("test", sim_path = "tmp")
read_in_prj(test, "../ogs-6.4.1-Tests-Data/Tests/Data/HydroMechanics/OrthotropicEmbeddedFracturePermeability/disc_with_hole_quasiisotropic.prj")
test$parameters[[1]]$range
test$parameters[[1]]$seed


lvl2_process <- analyse_xml(path = "../ogs-6.4.1-Tests-Data",
            pattern = "\\.prj$",
            xpath = "//OpenGeoSysProject/processes/process",
            print_findings = TRUE)
# simlified_elasticity
analyse_xml(path = "../ogs-6.4.1-Tests-Data",
            pattern = "\\.prj$",
            xpath = "//OpenGeoSysProject/processes/process/simplified_elasticity",
            print_findings = TRUE)

# test:
test <- OGS6$new("test", sim_path = "tmp")
read_in_prj(test, "../ogs-6.4.1-Tests-Data/Tests/Data/ThermoRichardsFlow/SimplifiedMechanics/TRhyd_saturated.prj")
test$processes[[1]]$simplified_elasticity
# check!

# chemically_induced_porosity_change
analyse_xml(path = "../ogs-6.4.1-Tests-Data",
            pattern = "\\.prj$",
            xpath = "//OpenGeoSysProject/processes/process/chemically_induced_porosity_change",
            print_findings = TRUE)

analyse_xml(path = "../ogs-6.4.1-Tests-Data",
            pattern = "\\.prj$",
            xpath = "//chemical_system/linear_solver",
            print_findings = TRUE)
# added liner solver in prj_chemical_system.R

# test:
test <- OGS6$new("test", sim_path = "tmp")
read_in_prj(test, "../ogs-6.4.1-Tests-Data/Tests/Data/Parabolic/ComponentTransport/ReactiveTransport/EquilibriumPhase/calcitePorosityChange.prj")
read_in_prj(test, "../ogs-6.4.1-Tests-Data/Tests/Data/Parabolic/ComponentTransport/ReactiveTransport/CationExchange/exchange.prj")
test$chemical_system$linear_solver
test$processes[[1]]$chemically_induced_porosity_change

chem_ex <- analyse_xml(path = "../ogs-6.4.1-Tests-Data",
            pattern = "\\.prj$",
            xpath = "//chemical_system/exchangers",
            print_findings = TRUE)
generate_constructor(chem_ex, print_result = TRUE)
generate_helper(chem_ex, print_result = TRUE)
#added prj_exchange class
# run script in data-raw
# devtools::document()
xpaths_for_classes[["prj_exchangers"]]
xpaths_for_classes[["prj_solution"]]

analyse_xml(path = "../ogs-6.4.1-Tests-Data",
            pattern = "\\.prj$",
            xpath = "//process/use_server_communication",
            print_findings = TRUE)
read_in_prj(test, "../ogs-6.4.1-Tests-Data/Tests/Data/Parabolic/T/3D_3BHEs_array_SimX/3bhes_1U.prj")
read_in_prj(test, "../ogs-6.4.1-Tests-Data/Tests/Data/Parabolic/T/3D_Beier_sandbox_SimX/beier_sandbox.prj")
# added argument to prj_process.R

print(change_df, n = 26)
analyse_xml(path = "../ogs-6.4.1-Tests-Data",
            pattern = "\\.prj$",
            xpath = "//process/phasefield_model",
            print_findings = TRUE)

test <- OGS6$new("test", sim_path = "tmp")
read_in_prj(test, "../ogs-6.4.1-Tests-Data/Tests/Data/PhaseField/beam/AT1_iso_tensile.prj")
test$processes[[1]]$phasefield_model

analyse_xml(path = "../ogs-6.4.1-Tests-Data",
            pattern = "\\.prj$",
            xpath = "//process/irreversible_threshold",
            print_findings = TRUE)
test$processes[[1]]$irreversible_threshold
# added as argument

analyse_xml(path = "../ogs-6.4.1-Tests-Data",
            pattern = "\\.prj$",
            xpath = "//process/tabular_file",
            print_findings = TRUE)
test <- OGS6$new("test", sim_path = "tmp")
read_in_prj(test, "../ogs-6.4.1-Tests-Data/Tests/Data/Parabolic/ComponentTransport/ReactiveTransport/SurfaceComplexation/LookupTable/RadionuclideSorption.prj")
test$processes[[1]]$tabular_file


analyse_xml(path = "../ogs-6.4.1-Tests-Data",
            pattern = "\\.prj$",
            xpath = "//process/temperature_field",
            print_findings = TRUE)
test <- OGS6$new("test", sim_path = "tmp")
read_in_prj(test, "../ogs-6.4.1-Tests-Data/Tests/Data/Parabolic/ComponentTransport/ThermalDiffusion/TemperatureField_transport.prj")
test$media[[1]]$phases$phase$components[[1]]$properties[[1]]$reference_temperature
test$media[[1]]$phases$phase$components[[1]]$properties[[1]]$activation_energy

analyse_xml(path = "../ogs-6.4.1-Tests-Data",
            pattern = "\\.prj$",
            xpath = "//process/use_stokes_brinkman_form",
            print_findings = TRUE)
test <- OGS6$new("test", sim_path = "tmp")
read_in_prj(test, "../ogs-6.4.1-Tests-Data/Tests/Data/StokesFlow/2DPlanarPermeableFracture/2DPlanarPermeableFracture.prj")
test$processes[[1]]$use_stokes_brinkman_form
# added as argument

analyse_xml(path = "../ogs-6.4.1-Tests-Data",
            pattern = "\\.prj$",
            xpath = "//value/expression",
            print_findings = TRUE)
test <- OGS6$new("test", sim_path = "tmp")
read_in_prj(test, "../ogs-6.4.1-Tests-Data/Tests/Data/Parabolic/ComponentTransport/SimpleSynthetics/DiffusionAndStorageAndAdvection.prj")
test$media[[1]]$phases$phase$properties$property$value


# run benchmarks ----------------------------------------------------------
options("r2ogs6.default_ogs6_bin_path" = "/usr/local/bin/ogs6.4.1.sif")
options("r2ogs6.default_ogs6_processlib_path" = "../ogs-6.4.1-ProcessLib/ProcessLib")

run_all_benchmarks("/home/phit0/Public/ogs-6.4.1-Tests-Data/Tests/Data/",
                   sim_path = "tmp/"
                  )


# debug errors from benchmarks --------------------------------------------
run_benchmark("/home/phit0/Public/ogs-6.4.1-Tests-Data/Tests/Data/Parabolic/T/t1_1Dsource/t1_1Dsource.prj",
              sim_path = "tmp")
# some benchmarks with *. mesh instead of vtu
run_benchmark("/home/phit0/Public/ogs-6.4.1-Tests-Data/Tests/Data/HydroMechanics/OrthotropicEmbeddedFracturePermeability/x_strain_y_flow.prj",
              sim_path = "tmp")
validate_read_in_xml("/home/phit0/Public/ogs-6.4.1-Tests-Data/Tests/Data/HydroMechanics/OrthotropicEmbeddedFracturePermeability/x_strain_y_flow.prj")

run_benchmark("/home/phit0/Public/ogs-6.4.1-Tests-Data/Tests/Data/Elliptic/cube_1x1x1_SteadyStateDiffusion/cube_1e0.xml",
              sim_path = "tmp")
run_benchmark("/home/phit0/Public/ogs-6.4.1-Tests-Data/Tests/Data/Elliptic/cube_1x1x1_SteadyStateDiffusion/cube_1e0.prj",
              sim_path = "tmp")
# wrong file ending *.xml from Tests.cmake (fixed)

run_benchmark("/home/phit0/Public/ogs-6.4.1-Tests-Data/Tests/Data/TH2M/TH_unsaturated/heatpipe_radial_static_gas/heatpipe_radial_static_gas.prj",
              sim_path = "tmp/")
# triple_temperature
# triple_pressure
# critical_temperature
# critical_pressure
# reference_pressure

## all inserted as arguments

file.exists(
"/home/phit0/Public/ogs-6.4.1-Tests-Data/Tests/Data/Elliptic/line_1_SteadyStateDiffusion/line_1e0.prj",
"/home/phit0/Public/ogs-6.4.1-Tests-Data/Tests/Data/Elliptic/line_1_SteadyStateDiffusion/line_1e0_neumann.prj",
"/home/phit0/Public/ogs-6.4.1-Tests-Data/Tests/Data/Elliptic/line_1_SteadyStateDiffusion/line_1e0_robin_right_picard.prj",
"/home/phit0/Public/ogs-6.4.1-Tests-Data/Tests/Data/Elliptic/line_1_SteadyStateDiffusion/line_1e0_robin_left_picard.prj",
"/home/phit0/Public/ogs-6.4.1-Tests-Data/Tests/Data/Elliptic/line_1_SteadyStateDiffusion/line_1e0_time_dep_dirichlet.prj",
"/home/phit0/Public/ogs-6.4.1-Tests-Data/Tests/Data/Elliptic/line_1_SteadyStateDiffusion/line_1e0_time_dep_neumann.prj"
) # k then

run_benchmark("/home/phit0/Public/ogs-6.4.1-Tests-Data/Tests/Data/Parabolic/T/t1_1Dsource/t1_1Dsource.prj",
                sim_path = "tmp")

run_benchmark("/home/phit0/Public/ogs-6.4.1-Tests-Data/Tests/Data/Parabolic/T/t1_1Dsteady/t1_1Dsteady.prj",
              sim_path = "tmp")
run_benchmark("/home/phit0/Public/ogs-6.4.1-Tests-Data/Tests/Data/Parabolic/T/t2_1D1bt/t2_1D1bt.prj",
              sim_path = "tmp")
run_benchmark("/home/phit0/Public/ogs-6.4.1-Tests-Data/Tests/Data/Parabolic/T/t2_1D2bt/t2_1D2bt.prj",
              sim_path = "tmp")
run_benchmark("/home/phit0/Public/ogs-6.4.1-Tests-Data/Tests/Data/HydroMechanics/OrthotropicEmbeddedFracturePermeability/x_strain_y_flow.prj",
              sim_path = "tmp")

run_benchmark("/home/phit0/Public/ogs-6.4.1-Tests-Data/Tests/Data/HydroMechanics/OrthotropicEmbeddedFracturePermeability/y_strain_z_flow.prj",
              sim_path = "tmp")
run_benchmark("/home/phit0/Public/ogs-6.4.1-Tests-Data/Tests/Data/HydroMechanics/OrthotropicEmbeddedFracturePermeability/z_strain_x_flow.prj",
              sim_path = "tmp")
run_benchmark("/home/phit0/Public/ogs-6.4.1-Tests-Data/Tests/Data/HydroMechanics/OrthotropicEmbeddedFracturePermeability/disc_with_hole_anisotropic.prj",
              sim_path = "tmp")
run_benchmark("/home/phit0/Public/ogs-6.4.1-Tests-Data/Tests/Data/HydroMechanics/OrthotropicEmbeddedFracturePermeability/disc_with_hole_anisotropic_rotated.prj",
              sim_path = "tmp")
run_benchmark("/home/phit0/Public/ogs-6.4.1-Tests-Data/Tests/Data/HydroMechanics/OrthotropicEmbeddedFracturePermeability/disc_with_hole_quasiisotropic.prj",
              sim_path = "tmp")
run_benchmark("/home/phit0/Public/ogs-6.4.1-Tests-Data/Tests/Data/Parabolic/LiquidFlow/Verification/h1_1Dsource/h1_1Dsource.prj",
              sim_path = "tmp")
run_benchmark("/home/phit0/Public/ogs-6.4.1-Tests-Data/Tests/Data/Parabolic/LiquidFlow/Verification/h1_1Dsteady/h1_1Dsteady.prj",
              sim_path = "tmp")
run_benchmark("/home/phit0/Public/ogs-6.4.1-Tests-Data/Tests/Data/Parabolic/LiquidFlow/Verification/h1_3Dhydstat/h1_3Dhydstat.prj",
              sim_path = "tmp")
run_benchmark("/home/phit0/Public/ogs-6.4.1-Tests-Data/Tests/Data/Parabolic/LiquidFlow/Verification/h2_1D1bt/h2_1D1bt.prj",
              sim_path = "tmp")
run_benchmark("/home/phit0/Public/ogs-6.4.1-Tests-Data/Tests/Data/Parabolic/LiquidFlow/Verification/h2_1D2bt/h2_1D2bt.prj",
              sim_path = "tmp")
run_benchmark("/home/phit0/Public/ogs-6.4.1-Tests-Data/Tests/Data/Elliptic/cube_1x1x1_SteadyStateDiffusion/cube_1e0.xml",
              sim_path = "tmp")
run_benchmark("/home/phit0/Public/ogs-6.4.1-Tests-Data/Tests/Data/EllipticPETSc/quad_20x10_GroundWaterFlow.prj",
              sim_path = "tmp")
run_benchmark("/home/phit0/Public/ogs-6.4.1-Tests-Data/Tests/Data/Parabolic/TES/1D/tes-1D-zeolite-discharge-small.prj",
              sim_path = "tmp")
run_benchmark("/home/phit0/Public/ogs-6.4.1-Tests-Data/Tests/Data/Parabolic/TES/1D/tes-1D-zeolite-discharge-large.prj",
              sim_path = "tmp")
run_benchmark("/home/phit0/Public/ogs-6.4.1-Tests-Data/Tests/Data/Parabolic/TES/1D/tes-1D-zeolite-discharge-small-newton.prj",
              sim_path = "tmp")

run_benchmark("/home/phit0/Public/ogs-6.4.1-Tests-Data/Tests/Data/TH2M/THM/slab/THM_1d_dirichlet.prj",
              sim_path = "tmp")
# source terms field of prj file is empty

run_benchmark("/home/phit0/Public/ogs-6.4.1-Tests-Data/Tests/Data/TH2M/THM/slab/THM_1d_dirichlet_newton.xml",
              sim_path = "tmp")

run_benchmark("/home/phit0/Public/ogs-6.4.1-Tests-Data/Tests/Data/TH2M/TH_unsaturated/heatpipe_radial_static_gas/heatpipe_radial_static_gas.prj",
              sim_path = "tmp")
run_benchmark("/home/phit0/Public/ogs-6.4.1-Tests-Data/Tests/Data/TH2M/TH_unsaturated/heatpipe_slab_static_gas/heatpipe_slab_static_gas.prj",
              sim_path = "tmp")
run_benchmark("/home/phit0/Public/ogs-6.4.1-Tests-Data/Tests/Data/TH2M/TH2/heatpipe_radial/heatpipe_radial.prj",
              sim_path = "tmp")
run_benchmark("/home/phit0/Public/ogs-6.4.1-Tests-Data/Tests/Data/TH2M/TH2/heatpipe_slab/heatpipe_slab.prj",
              sim_path = "tmp")
run_benchmark("/home/phit0/Public/ogs-6.4.1-Tests-Data/Tests/Data/ThermoRichardsFlow/SimplifiedMechanics/TaskCDECOVALEX2023/Decovalex-0-TRF.prj",
              sim_path = "tmp")






