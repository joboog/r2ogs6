# !!!!! READ the developer guide before updating r2ogs6 !!!!!!
# ./vignettes/dev_workflow_vignette.Rmd

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

# execute code in data_raw/xpaths_for_classes.R
xpaths_for_classes[["prj_raster"]] should show /OpenGeoSys/rasters/raster

# add test with bm Parabolic/LiquidFlow/RasterParameter/GroundwaterModelWithRasterBC.prj
# this bm does not work as the references raster file is not shipped with
test <- OGS6$new("test", sim_path = paste0(temppath, "/test"))
bm <- paste0("Parabolic/LiquidFlow/RasterParameter/GroundwaterModelWithRasterBC.prj")
read_in_prj(test, paste0(temppath,"/Data_652/", bm), read_includes=TRUE)
# read of tag works!



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
rm(test)
test <- OGS6$new("test", sim_path = paste0(temppath, "/test"))
bm <- paste0("Parabolic/ComponentTransport/ReactiveTransport/DecayChain/1d_decay_chain_OS.prj")
read_in_prj(test, paste0(temppath,"/Data_652/", bm))
test$time_loop$output$output_extrapolation_residuals
# # read of tag works!

dir_make_overwrite(test$sim_path)
run_benchmark(prj_path = paste0(temppath,"/Data_644/", bm),
              sim_path = test$sim_path)
# bm works!





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
# added "steepness" and "characteristic_temperature" to class prj_pr_property

# test with ThermoHydroMechanics/1D_freezing_column_Stefan/Stefan_problem.prj
# rm(test)
test <- OGS6$new("test", sim_path = paste0(temppath, "/test"))
bm <- "ThermoHydroMechanics/1D_freezing_column_Stefan/Stefan_problem.prj"
read_in_prj(test, paste0(temppath,"/Data_652/", bm))
test$processes[[1]]$ice_constitutive_relation
# # read of tag works!

# added "steepness" and "characteristic_temperature" to class prj_pr_property
dir_make_overwrite(test$sim_path)
run_benchmark(prj_path = paste0(temppath,"/Data_652/", bm),
              sim_path = test$sim_path)
# bm works

new child:  use_b_bar
of tag: process ---------------------------------------------------------

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
# rm(test)
test <- OGS6$new("test", sim_path = paste0(temppath, "/test"))
bm <- "Mechanics/CooksMembrane/CooksMembrane.prj"
read_in_prj(test, paste0(temppath,"/Data_652/", bm))
test$processes[[1]]$use_b_bar
# read of tag works!

dir_make_overwrite(test$sim_path)
run_benchmark(prj_path = paste0(temppath,"/Data_652/", bm),
              sim_path = test$sim_path)
# bm works


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

# test with Elliptic/cube_1x1x1_SteadyStateDiffusion/cube_1e2_processed.prj
# rm(test)
test <- OGS6$new("test", sim_path = paste0(temppath, "/test"))
bm <- "Elliptic/cube_1x1x1_SteadyStateDiffusion/cube_1e2_processed.prj"
read_in_prj(test, paste0(temppath,"/Data_652/", bm))
test$time_loop$output$meshes
# read of tag works!

dir_make_overwrite(test$sim_path)
ogs6_export_sim_files(ogs6_obj = test,
                      overwrite = TRUE,
                      copy_ext_files = FALSE,
                      test_mode = FALSE)
run_benchmark(prj_path = paste0(temppath,"/Data_652/", bm),
              sim_path = test$sim_path)
# bm works


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

# new class prj_basis_vector_0 (1,2) added

# execute code in data_raw/xpaths_for_classes.R
xpaths_for_classes[["prj_basis_vector_1"]] # should show local_coordinate_system/basis_vector_1

test <- OGS6$new("test", sim_path = paste0(temppath, "/test"))
bm <- "Mechanics/TransverseElasticModel/m_e_transiso_2D.prj"
read_in_prj(test, paste0(temppath,"/Data_652/", bm))
test$local_coordinate_system$basis_vector_0

dir_make_overwrite(test$sim_path)
ogs6_export_sim_files(ogs6_obj = test,
                      overwrite = TRUE,
                      copy_ext_files = FALSE,
                      test_mode = FALSE)

# run_benchmark(prj_path = paste0(temppath,"/Data_652/", bm),
#               sim_path = test$sim_path)

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
test <- OGS6$new("test", sim_path = paste0(temppath, "/test"))
bm <- "Parabolic/HT/SimpleSynthetics/IsothermalFluidFlow.prj"
read_in_prj(test, paste0(temppath,"/Data_652/", bm))
test$local_coordinate_system$basis_vector_0

dir_make_overwrite(test$sim_path)
ogs6_export_sim_files(ogs6_obj = test,
                      overwrite = TRUE,
                      copy_ext_files = FALSE,
                      test_mode = FALSE)

run_benchmark(prj_path = paste0(temppath,"/Data_652/", bm),
              sim_path = test$sim_path)

# Now try to run all OGS v6.5.2 benchmarks!
run_all_benchmarks(
    path = paste0(temppath,"/Data_652/"),
    ogs6_processlib_path = "/home/boogjoha/work/ogs/ProcessLib",
    sim_path = paste0(temppath, "/test_all_bm")
)

# The following benchmarks returned zero exit status
# isotropic_thermal_expansion_aniso_expansion_x45
# HydroMechanics_single_fracture_3compartments_flow_linear_aperture0_e
# line_source_term_x_0.5_line_source_term_x_0.5
# Square_sealed_homogeneous_square_1e0
# m1_3Dload_m1_3Dload
# Mechanics_single_joint_3D
# cube_1x1x1_SteadyStateDiffusion_cube_2e3_prism_surfaceflux_top_bottom
# Inclined2DMesh_transient_flow_in_inclined_2D_plane
# Mechanics_two_cracks_branch_pull
# OrthotropicEmbeddedFracturePermeability_disc_with_hole_anisotropic_rotated
# 2D_square_1.8e1_calculatesurfaceflux
# t2_1D2bt_t2_1D2bt
# 1D_neumann_newton_masslumping
# MFront_cube_1e0_dp
# EllipticPETSc_cube_1e3
# Unconfined_Compression_early_square_1e2_UC_early
# ThermoMechanicalPhaseField_beam3d
# SimpleSynthetics_ConcentrationDiffusionAndStorage
# cube_1x1x1_SteadyStateDiffusion_cube_1e0_neumann
# 1D_neumann_picard_masslumping
# RotatedAroundVerticalAxis_flow_gravity
# 1D_dirichlet_source-term_line_1_line_1e2_source_term
# SimpleSynthetics_open_boundary_component-transport_cube_1e3
# t2_1D1bt_t2_1D1bt
# 3D_BHE_GW_advection_BHE_GW_advection
# StaggeredScheme_ConcentrationDiffusionAndStorage
# cube_1x1x1_SteadyStateDiffusion_cube_1e3_calculatesurfaceflux
# SimpleSynthetics_calculatesurfaceflux_ht_cube_1e4
# TaskCDECOVALEX2023_Decovalex-0-TRF
# h1_3Dhydstat_h1_3Dhydstat
# nonuniform_bc_SteadyStateDiffusion_neumann_nonuniform
# PointHeatSource_point_heat_source_2D_gml
# Mechanics_single_joint_displacement_controlled
# anisotropic_thermal_expansion_aniso_expansion
# quarter_disc_quarter_disc_neumann
# Beam_sealed_bimaterial_square_1e2
# OrthotropicEmbeddedFracturePermeability_disc_with_hole_anisotropic
# HydroMechanics_single_fracture_3compartments_flow
# GravityDriven_gravity_driven_XZ
# 3D_line_source_term_middle_restricted_line_source_term_x_0.5_y_0.5_restricted
# SimpleSynthetics_ConcentrationDiffusionOnly
# t1_1Dsteady_t1_1Dsteady
# Richards_RichardsFlow_2d_compare_ogs5
# ThermoOsmosis_Column
# HydroMechanics_single_fracture_3compartments_flow_linear_aperture0
# HeatPipe_Twophase_HeatPipe_quad_curve_large
# anisotropic_thermal_expansivity_cube_ortho_phi0.183_petsc
# CreepAfterExcavation_CreepAfterExcavation
# RichardsFlow2D_RichardsFlow_2d_small
# Pyramid_cuboid_1x1x1_pyramid_6000_calculatesurfaceflux
# sphere_point_heatsource
# PressureBCatCornerOfAnisotropicSquare_pressureBC_at_corner_of_anisotropic_square
# cube_1x1x1_SteadyStateDiffusion_cube_1e0
# FaultedCube_Ra_795_fault_bcgs_jacobi
# GravityDriven_gravity_driven
# EllipticPETSc_cube_1e3_XDMF_np2
# McWhorter_TwoPhase_mcwt_line
# ClassicalTransportExample_classical_transport_example_full_upwind
# tm1_1Dfixb_tm1_1Dfixb
# ThermoMechanics_iglu_axisymmetric_plane_strain_quad
# OrthotropicEmbeddedFracturePermeability_disc_with_hole_quasiisotropic
# ThermoMechanicalPhaseField_cube_1e0
# TH_ClassicalTransportExample_classical_transport_example_full_upwind
# square_1x1_SteadyStateDiffusion_square_1e2_GMRES
# PointHeatSource_point_heat_source_2D
# TracerSimulation_TracerSimulation
# flow_no_strain_flow_no_strain
# Confined_Compression_square_1e2_quad9
# m1_3Dsquare_m1_3Dsquare
# square_1x1_SteadyStateDiffusion_square_1e6_with_nodal_sources
# XDMF_compression_off_FunctionParameterTest_XDMF
# FailureIndexDependentPermeability_quad_with_half_hole
# HeatPipe_Twophase_HeatPipe_quad_curve_small
# Tube_tube
# Flux_cube_1e3_calculatesurfaceflux
# ThermoMechanics_iglu_axisymmetric_plane_strain
# GravityDriven3D_isotropic_gravity_driven3D
# tm2_1D1bt_tm2_1D1bt
# CreepWithHeterogeneousReferenceTemperature_arehs-salt-M_gravity_only_element_refT
# Beam_unsealed_bimaterial_square_1e2
# Richards_RichardsFlow_2d_large
# Verification_hm2_1D2bt
# ADecovalexTHMCBasedHTExample_th_decovalex
# SimpleSynthetics_PressureDiffusionTemperatureDiffusionStaggered
# 3D_2U_BHE_3D_2U_BHE
# CTF1_CTF1
# cube_1x1x1_SteadyStateDiffusion_cube_1e3_top_neumann_newton
# square_1x1_SteadyStateDiffusion_wedge_1e2_axi_ang_0.02
# anisotropic_thermal_expansion_aniso_expansion_y45
# nonuniform_bc_SteadyStateDiffusion_dirichlet_nonuniform
# GroundEquilibrium_simHM_ground_quadBCu
# NodalSourceTerm_nodal_source_test
# m1_3Dgravity_m1_3Dgravity
# square_1x1_SteadyStateDiffusion_Python_square_1e5_poisson_sin_x_sin_y
# cube_1x1x1_SteadyStateDiffusion_cube_1e3_neumann_calculatesurfaceflux
# SimpleSynthetics_calculatesurfaceflux_ht_cube_1e3
# cube_1x1x1_SteadyStateDiffusion_cube_2e3_prism_surfaceflux_front_back
# quarter_circle_quarter_circle_neumann
# TimeDependentHeterogeneousBoundaryConditions_TimeDependentHeterogeneousBoundaryConditions
# elder_elder
# m1_3Dtopload_m1_3Dtopload
# anisotropic_thermal_expansion_aniso_expansion_z45
# h1_1Dsource_h1_1Dsource
# RichardsMechanics_confined_compression_fully_saturated_restart
# SimplifiedMechanics_TRhyd_unsaturated
# .._cube_1e3_XDMF_np3_3files
# SimpleSynthetics_PressureDiffusionTemperatureDiffusion
# LiakopoulosHM_liakopoulos
# HeatingHomogeneousDomain_hex_THM
# SimpleSynthetics_IsothermalFluidFlowStaggered
# 1D_line_source_term_tests_line_source_term
# 3D_line_source_term_middle_line_source_term_x_0.5_y_0.5
# slab_THM_1d_dirichlet
# SimpleSynthetics_ConcentrationDiffusionOnly_3Components
# .._cube_1e3_XDMF_np3_2files
# disc_with_hole_disc_with_hole
# 1D_neumann_newton
# circle_radius_1_circle_1e2_axi
# XDMF_CoupledPressureParabolicTemperatureParabolicStaggered
# SimpleSynthetics_IsothermalFluidFlow
# Wetland_Wetland_1d
# circle_radius_1_circle_1e4_axi
# Mechanics_single_joint_inside
# SimpleSynthetics_DiffusionAndStorageAndAdvectionAndDecay
# SimplifiedMechanics_TRhyd_unsaturated_bishopstest
# 2D_tes-inert-wedge
# SimplifiedMechanics_TRcustom_unsaturated
# HydroMechanics_single_fracture_3compartments_flow_CHZ
# StrainDependentPermeability_gas_loading
# Storage_cube_incompressible_fluid
# SimplifiedMechanics_TRuni_unsaturated
# OrthotropicEmbeddedFracturePermeability_unconfined_biot
# square_100x100_ComponentTransport_square_1e4_heterogeneity
# GMSH2OGS_quadratic_mesh_assembly_test
# h2_1D1bt_h2_1D1bt
# square_1x1_SteadyStateDiffusion_square_1e2_GMRES_GML_output_xdmf-hdf5
# square_1x1_SteadyStateDiffusion_Python_square_1e3_poisson_sin_x_sin_y
# Theis_theis
# HydroMechanics_single_fracture_3D
# Inclined2DMesh_hydrostatic_flow_in_inclined_2D_plane
# circle_radius_1_circle_1e5_axi
# ThermoMechanicalPhaseField_slab_5
# FractureIn3D_fractures_in_3D
# ClassicalTransportExample_classical_transport_example_full_upwind_staggered
# Storage_cube_isochoric_heat-up
# Liakopoulos_TwoPhase_Lia_quad_short
# Richards_RichardsFlow_2d_small_PID_adaptive_dt
# square_1x1_SteadyStateDiffusion_square_1e2_axi
# ogs5_H_3D_ogs5_H_3d
# Confined_Compression_cube_1e3
# 3D_deep_BHE_3D_deep_BHE_CXA
# 1D_dirichlet_line_60_heat
# Verification_hm2_1D1bt
# OrthotropicEmbeddedFracturePermeability_y_strain_z_flow
# SimpleSynthetics_IsothermalFluidFlowWithGravity
# flow_pressure_boundary_flow_pressure_boundary
# circle_radius_1_circle_1e6_axi
# TCEDiffusion_Twophase_TCE_diffusion_1D_small
# cube_1x1x1_SteadyStateDiffusion_cube_1e3_top_neumann
# WithPicardNonLinearSolverAndPETSc_Decovalex-0-TRF
# OrthotropicEmbeddedFracturePermeability_z_strain_x_flow
# anisotropic_thermal_expansivity_square_ortho_phi0.0
# 49k_prisms_line_source_term_in_cylinder
# 1D_tes-1D-zeolite-discharge-large
# square_1x1_SteadyStateDiffusion_square_1e2_volumetricsourceterm
# EllipticPETSc_square_1e1_neumann
# ThermoMechanics_iglu_quarter_plane_strain
# RichardsMechanics_RichardsFlow_2d_richardsflow
# SimpleSynthetics_CoupledPressureParabolicTemperatureParabolicStaggered
# Verification_hm2_1Dcolumn1
# TH_ClassicalTransportExample_classical_transport_example
# MassConservation_mass_conservation
# Point_injection_pointheatsource_quadratic-mesh
# tm1_3Dcube_tm1_3Dcube
# SimpleSynthetics_CoupledPressureParabolicTemperatureParabolic
# 286k_prisms_line_source_term_in_cylinder
# SimpleSynthetics_PressureParabolicTemperatureParabolic
# Verification_hm2_1Dbiot
# SimpleSynthetics_DiffusionAndStorageAndAdvection
# square_1x1_SteadyStateDiffusion_wedge_1e4_axi_ang_0.02
# GroundEquilibrium_simHM_ground_quadBCu_python
# ConstViscosity_square_5500x5500
# square_1x1_SteadyStateDiffusion_square_1e4_axi
# 3D_deep_BHE_3D_deep_BHE_CXC
# line_source_term_x_0.5_restricted_to_middle_line_source_term_x_0.5
# 2D_axially_symmetric_square_1e2_axi
# BHE_1P_BHE_1P_newton
# ThermalDiffusion_TemperatureField
# Liakopoulos_TwoPhase_Lia_quad_large
# PrincipalStress_sphere
# SimpleSynthetics_DiffusionAndStorageAndAdvectionAndDispersion
# MultiMesh_drainage
# Mechanics_two_joints
# SimpleSynthetics_open_boundary_component-transport_cube_1e3_advective_form
# 1Din3D_line_fractures_in_3D
# m1_3Dbottom_m1_3Dbottom
# BuildupTest_buildup_test
# goswami_goswami_input
# InjectionProduction1D_InjectionProduction1DMono
# GravityDriven3D_anisotropic_gravity_driven3D
# 2D_BHE_array_bhe2d
# ConTracer_ConTracer_2d
# elder_elder-python
# EmbeddedFracturePermeability_cube
# SimpleAxisymmetricCreep_SimpleAxisymmetricCreepWithAnalyticSolution
# SimpleSynthetics_PressureParabolicTemperatureParabolicStaggered
# flow_pressure_boundary_flow_pressure_boundary_python
# line_source_term_left_source_term_left
# flow_free_expansion_flow_free_expansion
# h2_1D2bt_h2_1D2bt
# ThermoMechanicalPhaseField_tes_hx3_iglu
# tm1_2Dbeam_tm1_2Dbeam
# HydroMechanics_single_fracture_3compartments_flow_CHZ_sigma0
# OrthotropicEmbeddedFracturePermeability_x_strain_y_flow
# SimpleAxisymmetricCreep_SimpleAxisymmetricCreep
# Hollow_Sphere_sphere
# tm1_3Dsquare_tm1_3Dsquare
# square_1x1_SteadyStateDiffusion_Python_square_1e3_laplace_eq
# BGRaCreepAndInitialStressAtIP_AREHS_arehs-salt-THM01_0
# BDT_cube_1e0_bdt
# ogs5_H_2D_ogs5_H_2d
# circle_radius_1_circle_1e1_axi
# 1D_neumann_picard
# Mechanics_single_joint_negative_aperture
# tm1_2Dsquare_tm1_2Dsquare
# SimpleSynthetics_DiffusionAndStorageAndAdvectionAndDispersionHalf
# 2D_tes-inert-axi
# Verification_hm2_2Dmandel
# PETSc_heat_pipe_strict
# SimpleSynthetics_IsothermalFluidFlowWithGravityStaggered
# m1_1Dload_m1_1Dload
# BodyForce_square
# SimpleSynthetics_surfaceflux_component-transport_cube_1e3
# Gravity_flow_gravity
# 2D_square_1e1_calculatesurfaceflux
# GroundEquilibrium_simHM_ground
# EllipticPETSc_cube_1e3_XDMF_np3
# tmp/update_652/./test
# RichardsFlow2D_RichardsFlow_2d_small_Picard
# StaggeredScheme_ConcentrationDiffusionOnly
# AxiSymTheis_axisym_theis
# tm1_1Dbeam_tm1_1Dbeam
# 1D_tes-1D-zeolite-discharge-small
# HeatTransportInStationaryFlow_HeatTransportInStationaryFlow
# square_1x1_SteadyStateDiffusion_square_1e3_volumetricsourceterm
# TimeIntervalDirichletBC_TimeIntervalDirichletBC
# tm2_1Dfixc_tm2_1Dfixc
# Point_injection_pointheatsource_linear-mesh
# line_source_term_left_source_term_left_r
# Simple3DThermoMechanicsFromTM_cube_1e3
# SingleMesh_drainage
# cube_1x1x1_SteadyStateDiffusion_cube_1e0_quadratic_hex
# SimplifiedMechanics_TRuni_unsaturated_bishopstest
# t1_1Dsource_t1_1Dsource
# HydroMechanics_single_fracture
# HydroMechanics_TaskB
# m1_1Dlozenge_m1_1Dlozenge
# LiakopoulosPETSc_liakopoulos_mixElem_mumps
# XDMF_FunctionParameterTest_XDMF
# RichardsFlow2D_RichardsFlow_2d_compare_ogs5
# Richards_RichardsFlow_2d_small
# ThermoMechanics_cube_1e3
# TimeDependentHeterogeneousSourceTerm_TimeDependentHeterogeneousSourceTerm
# StrainDependentPermeability_Strain_Dependent_Permeability_Test
# h1_1Dsteady_h1_1Dsteady
# EmbeddedFracturePermeability_square
# BHE_1P_BHE_1P
# Verification_hm2_1Dcolumn2
# axisymm_ring_ring_plane_strain
# SimplifiedMechanics_TRuni_saturated
# Unconfined_Compression_early_square_1e2_UC_early_python
# cube_1x1x1_SteadyStateDiffusion_cube_1e3_bottom_neumann_newton
# cube_1x1x1_SteadyStateDiffusion_cube_1e3_bottom_neumann
# BC_BC_TestSet_01
# Unconfined_Compression_late_square_1e2_UC_late
# anisotropic_thermal_expansivity_square_ortho_phi0.183
# LinearMFront_cube_1e0_lin
# ClassicalTransportExample_classical_transport_example
# cube_1x1x1_SteadyStateDiffusion_cube_2e3_prism_surfaceflux_left_right
# tm1_3Dgravity_tm1_3Dgravity
# BC_BC_RECHARGE_TestSet_01
# Mechanics_sfrac.q
# square_1x1_SteadyStateDiffusion_square_1e3_volumetricsourcetermdataarray
# ThermoMechanics_iglu_quarter_plane_strain_quad
# tm1_1Dfixa_tm1_1Dfixa
# 2D_axially_symmetric_wedge_1e2_axi_ang_0.02
# nonuniform_bc_SteadyStateDiffusion_inhomogeneous_permeability
# anisotropic_thermal_expansivity_cube_ortho_phi0.0
# SimpleAxisymmetricCreep_SimpleAxisymmetricCreepWithAnalyticSolutionMFront
# TransverseElasticModel_m_e_transiso_2D
# Mechanics_single_joint
# SimpleSynthetics_DiffusionAndStorageAndGravityAndDispersionHalf
# InjectionProduction1D_InjectionProduction1DMono_python
# TaskCDECOVALEX2023_Decovalex-0
# wedge_1x1x1_SteadyStateDiffusion_wedge_1e3_prism_surfaceflux_diagonal
# BC_BC_STORAGE_TestSet_01
# T_1d_dirichlet_T_1d_dirichlet
# m1_2Dload_m1_2Dload
# VariableNeumannBoundary_vdbc_input
# tm1_3Dorigin_tm1_3Dorigin
# Mechanics_two_cracks_junction_pull
# GroundEquilibrium_simHM_ground_python
# anisotropic_thermal_expansivity_cube_ortho_phi0.183
# SimplifiedMechanics_TRhyd_saturated
# M_2d_neumann_M_2d_neumann
# StaggeredScheme_surfaceflux_component-transport_cube_1e3
# Confined_Compression_square_1e2_tri
# 3D_Beier_sandbox_fixed_power_constant_flow
# circle_radius_1_circle_1e3_axi
# LineDirichletNeumannBC_line_dirichlet_neumannBC
# BC_BC_RECHARGE2_TestSet_01