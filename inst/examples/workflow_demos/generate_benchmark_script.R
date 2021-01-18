
library(r2ogs6)


# Modify the prj_path depending on where you saved the benchmark file.
prj_path <- "inst/extdata/flow_free_expansion/flow_free_expansion.prj"


# You can either define where to save the script explicitly...

script_path <- "some_directory/"

generate_benchmark_script(prj_path, script_path)


# ... or you can leave the path out, then it will be saved in
# r2ogs6.default_script_path. You can set this option by commenting out the
# line below and modifying the path to fit your system.

# options("r2ogs6.default_script_path" = "your_path_here")

generate_benchmark_script(prj_path)

