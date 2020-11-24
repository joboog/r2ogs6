detach("package:r2ogs6", unload=TRUE)
library(r2ogs6)


#This is where the user defines their data.

#============================== SET UP SIMULATION OBJECT ================================

#If you want to test this, don't forget to alter the ogs_bin_path to fit your installation location!

#First, we need to create a simulation object.

ogs6_obj <- OGS6$new(sim_name = "my_sim",
                     sim_id = 1,
                     sim_path = "D:\\OGS_Sim\\",
                     ogs_bin_path = "D:\\Programme\\OpenGeoSys\\ogs-6.3.2-Windows-10.0.14393-x64-python-3.7.2-de-utils\\bin\\")

#============================== READ IN .prj FILE ================================

#This will only work if the working directory is r2ogs6, but is fine for testing purposes.
prj_path <- paste0("inst/extdata/flow_free_expansion/flow_free_expansion.prj")

#prj_path = paste0("D:\\Programme\\OpenGeoSys\\ogs-master-Tests-Data-HydroMechanics\\",
#                                        "Tests\\Data\\HydroMechanics\\IdealGas\\flow_no_strain\\flow_no_strain.prj")

read_in_prj(ogs6_obj, prj_path)

#============================== Execution ================================

#Calls ogs6 object validator functions, exports all necessary files and starts OpenGeoSys6

run_simulation(ogs6_obj)
