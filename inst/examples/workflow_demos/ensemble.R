
library(r2ogs6)


#===== Set up simulation object =====


# First make sure you're ready to go by setting r2ogs6.default_ogs_bin_path.
# You can do this by commenting out the line below and modifying the path to
# fit your system.

# options("r2ogs6.default_ogs_bin_path" = "your_path_here")


# Then we can create a simulation object.

ogs6_obj <- OGS6$new(sim_name = "my_sim",
                     sim_id = 1,
                     sim_path = "D:/OGS_Sim/")


#===== Read in benchmark file =====


# Modify the prj_path depending on where you saved the benchmark file.
prj_path <- "inst/extdata/flow_free_expansion/flow_free_expansion.prj"

# Read in the benchmark into our simulation object
read_in_prj(ogs6_obj, prj_path)


#===== Create ensemble from simulation object =====


ogs6_ens <- OGS6_Ensemble$new(
    ogs6_obj = ogs6_obj,
    parameters = list(list(ogs6_obj$parameters[[2]]$value, c(0.2, 0.3, 0.4)))
)


#===== Run simulation =====


# Should work on machines with more RAM
ogs6_ens$run_simulation(parallel = TRUE)

