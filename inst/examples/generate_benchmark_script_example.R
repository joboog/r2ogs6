detach("package:r2ogs6", unload=TRUE)
library(r2ogs6)

# As a user you will want to alter the paths since this will only work if the working directory
# is r2ogs6, which is ok for developers but not for you (sorry!)

#Define .prj path
prj_path <- paste0("inst/extdata/flow_free_expansion/flow_free_expansion.prj")

dest_dir <- paste0(dirname(prj_path), "/")

generate_benchmark_script(prj_path, dest_dir)
