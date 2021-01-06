
#===== Getters to speed up tests, change these to fit your system! =====


#'get_default_sim_path
#'@description Utility function for testing
get_default_sim_path <- function(){

    default_sim_path <-
        "D:/OGS_sims/"

    return(default_sim_path)
}


#'get_default_script_path
#'@description Utility function for testing
get_default_script_path <- function(){

    default_script_path <-
        "D:/OGS_scripts/"

    return(default_script_path)
}


#'get_default_benchmark_path
#'@description Utility function for testing
get_default_benchmark_path <- function(){

    default_benchmark_path <-
        "D:/Programme/OpenGeoSys/ogs-master-Tests-Data/Tests/Data/"

    return(default_benchmark_path)
}


#'get_default_ogs_bin_path
#'@description Utility function for testing
get_default_ogs_bin_path <- function(){

    default_ogs_bin_path <-
        paste0("D:/Programme/OpenGeoSys/",
               "ogs-6.3.2-Windows-10.0.14393-x64-python-3.7.2-de-utils",
               "/bin/")

    return(default_ogs_bin_path)
}
