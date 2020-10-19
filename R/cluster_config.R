#This script only contains some tests for parallel computing so far

#System vars

#Check for number of available cores / logical cores (not always reliable on Unix-like systems)
parallel::detectCores()
parallel::detectCores(logical = FALSE)

# output_file_name <- "output.txt"
#
# socket_cluster <- parallel::makeCluster(4, outfile = output_file_name)
#
# parallel::clusterExport(socket_cluster, list("print_pid"))
#
# #parallel::clusterCall(socket_cluster, "Sys.getpid")
#
# parallel::clusterCall(socket_cluster, "print_pid")
#
# parallel::stopCluster(socket_cluster)
#
# read_output(output_file_name, TRUE)

# print_pid <- function(...) {
#   #Don't move s construction in cat(), else the file will get all mingled up!
#   s <- paste("My process ID is ", Sys.getpid(), "\n")
#   cat(s)
# }
#
# read_output <- function(file_name, delete_after_read = FALSE) {
#   cat(paste0(readLines(file_name), collapse="\n"))
#   if(delete_after_read) {
#     if (file.exists(file_name))
#       file.remove(file_name)
#   }
# }

