
#===== OGS6_Ensemble =====


#'OGS6_Ensemble
#'@description Constructor for the OGS6_Ensemble base class
#'@export
OGS6_Ensemble <- R6::R6Class(
    "OGS6_Ensemble",

    #'@description
    #'Creates new OGS6_Ensemble object
    #'@param ogs6_obj OGS6: A simulation object.
    #'@param parameters list(sublist, length(sublist) == 2): The first element
    #' of a sublist references an OGS6 parameter, the second one is a list or
    #' vector of values. Note that the second elements of the sublists must
    #' have the same length.
    public = list(
        initialize = function(ogs6_obj,
                              parameters) {

            assertthat::assert_that(inherits(ogs6_obj, "OGS6"))

            ens_path_split <-
                unlist(strsplit(ogs6_obj$sim_path, "/",
                                fixed = TRUE))

            ens_dir_name <-
                paste0(ogs6_obj$sim_name, "_ensemble")

            if(length(ens_path_split) == 1){
                ens_path_split <- paste0(ens_dir_name, ens_path_split)
            }else{
                ens_path_split <- append(ens_path_split,
                                         ens_dir_name,
                                         after = length(ens_path_split - 1))
            }

            private$.ens_path <-
                paste0(paste(ens_path_split,
                             collapse = "/"),
                       "/")

            private$.ensemble <- list(ogs6_obj)

            # Add all parameters to corresponding list
            assertthat::assert_that(is.list(parameters))

            ogs6_obj_ref <- deparse(substitute(ogs6_obj))

            for(i in seq_len(length(parameters))){
                dp_param <- deparse(substitute(parameters[[i]]))
                private$add_parameter(parameters[[i]],
                                      dp_param,
                                      ogs6_obj_ref)
            }

            private$make_ensemble()
        },

        #'@description
        #'Runs the simulation. This calls r2ogs6::run_simulation() internally.
        #' For ensembles, output will be written to logfiles.
        #'@param parallel flag: Should the function be run in parallel?
        #' This is implementented via the 'parallel' package.
        run_simulation = function(parallel = FALSE){

            assertthat::assert_that(assertthat::is.flag(parallel))

            if(parallel){

                # Forking is not possible in Windows
                use_socket <- (Sys.info()["sysname"] == "Windows")

                n_cores <- parallel::detectCores()

                cat("Detected ", n_cores, " cores.\n",
                    "Detected ", parallel::detectCores(logical = FALSE),
                    " logical cores.\n", sep = "")

                if(use_socket){
                    log_path <- paste0(self$ens_path, "cluster_log.txt")

                    socket_cl <- parallel::makeCluster(n_cores,
                                                       outfile = log_path)

                    # Load required libraries for each process
                    parallel::clusterEvalQ(socket_cl, {
                        library(xml2)
                        library(stringr)
                    })

                    # Export all functions from our package to the other nodes
                    all_r2ogs6_functions <- lsf.str("package:r2ogs6")
                    parallel::clusterExport(socket_cl, all_r2ogs6_functions)

                    # Computation
                    parallel::parLapply(socket_cl,
                                        self$ensemble,
                                        run_simulation,
                                        write_logfile = TRUE)

                    # Cleanup
                    parallel::stopCluster(socket_cluster)

                }else{

                    # For OSs other than Windows where forking is possible, we
                    # utilize it with mclapply

                    parallel::mclapply(self$ensemble,
                                       run_simulation,
                                       write_logfile = TRUE,
                                       mc.cores = n_cores)
                }
            }else{

                # For serial ensembles, we can use lapply
                lapply(self$ensemble,
                       run_simulation,
                       write_logfile = TRUE)
            }
        }
    ),

    active = list(

        #'@field parameters
        #'Getter for private parameter '.parameters'
        parameters = function() {
            private$.parameters
        },

        #'@field ens_path
        #'Getter for private parameter '.ens_path'
        ens_path = function(value) {
            private$.ens_path
        },

        #'@field ensemble
        #'Getter for private parameter '.ensemble'
        ensemble = function(value) {
            private$.ensemble
        }
    ),

    private = list(

        #@description
        #Adds a parameter.
        #@param parameter list, length == 2: The first element references an
        # OGS6 parameter, the second one is a list or vector of values.
        # To find out how many values you need to supply, call ensemble_size
        # on this object.
        #@param dp_param string: Deparsed paramter
        add_parameter = function(parameter,
                                 dp_param,
                                 ogs6_obj_ref) {

            assertthat::assert_that(assertthat::is.string(dp_param))
            assertthat::assert_that(is.list(parameter))

            assertthat::assert_that(length(parameter) == 2)

            # The parameter must have been defined previously!
            assertthat::assert_that(length(parameter[[1]]) != 0)

            if(length(self$parameters) != 0){
                assertthat::assert_that(
                    length(parameter[[2]]) ==
                        length(self$parameters[[1]][[2]]))
            }

            # To validate the original reference, deparse the parameter
            dp_param <- unlist(strsplit(dp_param,
                                        "list(list(",
                                        fixed = TRUE))[[2]]

            dp_param <- unlist(strsplit(dp_param,
                                        ",",
                                        fixed = TRUE))[[1]]

            com_str <- unlist(strsplit(dp_param,
                                       "$",
                                       fixed = TRUE))[[1]]

            if(ogs6_obj_ref != com_str){
                stop(paste("Added parameters must belong to the OGS6 object",
                           "the ensemble is based on!"),
                     call. = FALSE)
            }

            # After validating the original reference, replace it for
            # internal use
            dp_param_split <- unlist(strsplit(dp_param,
                                              "$",
                                              fixed = TRUE))

            dp_param_split[[1]] <- "ogs6_obj"
            dp_param <- paste(dp_param_split,
                              collapse = "$")

            parameter[[1]] <- dp_param

            private$.parameters <- c(private$.parameters, list(parameter))
        },

        #@description
        #Creates the actual ensemble.
        make_ensemble = function() {

            ogs6_obj <- self$ensemble[[1]]
            parameters <- private$.parameters

            # n iterations in first loop == n objects to create
            for (i in seq_len(length(parameters[[1]][[2]]))) {
                # Clone object, update parameter of clone
                ogs6_obj <- ogs6_obj$clone()

                ogs6_obj$sim_name <-
                    paste0(self$sim_name,
                           "_", (i + 1))

                ogs6_obj$sim_path <-
                    paste0(self$ens_path,
                           ogs6_obj$sim_name,
                           "/")

                for (j in seq_len(length(parameters))) {
                    set_param_call <-
                        paste0(parameters[[j]][[1]],
                               " <- parameters[[j]][[2]][[i]]")
                    eval(parse(text = set_param_call))
                }

                # Add clone to list of simulation objects
                private$.ensemble <- c(private$.ensemble, list(ogs6_obj))
            }
        },

        .ens_path = NULL,
        .ensemble = list(),
        .parameters = list()
    )
)
