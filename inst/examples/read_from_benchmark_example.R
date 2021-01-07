detach("package:r2ogs6", unload=TRUE)
library(r2ogs6)


#This is where the user defines their data.


#===== SET UP SIMULATION OBJECT =====


# Set this option on your system:

ogs_bin_path <- paste0("D:/Programme/OpenGeoSys/",
                       "ogs-6.3.2-Windows-10.0.14393-x64-python-3.7.2-de-utils",
                       "/bin/")

#First, we need to create a simulation object.

ogs6_obj <- OGS6$new(sim_name = "my_sim",
                     sim_id = 1,
                     sim_path = "D:/OGS_Sim/",
                     ogs_bin_path = ogs_bin_path)


#===== READ IN .prj FILE =====

# (system dependent)
prj_path = paste0("D:\\Programme\\OpenGeoSys\\",
                  "ogs-master-Tests-Data\\",
                  "Tests\\Data\\HydroMechanics\\IdealGas\\",
                  "flow_no_strain\\flow_no_strain.prj")

read_in_prj(ogs6_obj, prj_path)


#===== Execution =====


#Calls OGS6 validators, exports all necessary files and starts OpenGeoSys6

e <- run_simulation(ogs6_obj)
