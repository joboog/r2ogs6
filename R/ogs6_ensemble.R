
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
    #'@param sequential_mode flag: Defaults to `FALSE`
    #'@param percentages_mode flag: Defaults to `TRUE`
    #'@importFrom foreach %dopar%
    public = list(
        initialize = function(ogs6_obj,
                              parameters,
                              sequential_mode = FALSE,
                              percentages_mode = TRUE) {

            assertthat::assert_that(inherits(ogs6_obj, "OGS6"))
            assertthat::assert_that(assertthat::is.flag(sequential_mode))
            assertthat::assert_that(assertthat::is.flag(percentages_mode))

            private$.ens_path <- ogs6_obj$sim_path
            ogs6_obj$sim_path <- paste0(ogs6_obj$sim_path, ogs6_obj$sim_name)

            dp_str <- paste(deparse(substitute(parameters)), collapse = "\n")

            dp_str <- stringr::str_remove_all(
                dp_str,
                "(^list\\()|(\\)$)|[:space:]|([A-Za-z_]*\\s*=\\s*)")

            dp_strs <- unlist(strsplit(dp_str, "list\\("))
            dp_strs <- dp_strs[dp_strs != ""]

            private$.dp_parameters <- lapply(dp_strs, function(x){
                unlist(strsplit(x, ","))[[1]]
            })

            assertthat::assert_that(is.list(parameters))

            # If not in sequential mode, value vectors must have same length
            if(!sequential_mode){
                lapply(parameters, function(x){
                    assertthat::assert_that(length(x[[2]]) ==
                                                length(parameters[[1]][[2]]))
                })
            }else{
                assertthat::assert_that(!is.null(names(parameters)))
                assertthat::assert_that(!any(names(parameters) == ""))
            }

            second_elements <- lapply(parameters, function(x){x[[2]]})

            if(percentages_mode){
                private$.parameter_percs <- second_elements
                private$calc_values_by_percs(ogs6_obj)
                names(private$.parameter_values) <- names(parameters)
            }else{
                private$.parameter_values <- second_elements
            }

            private$make_ensemble(ogs6_obj$clone(deep = TRUE),
                                  sequential_mode)
        },

        #'@description
        #'Overrides default printing behaviour
        print = function(){
            cat("OGS6_Ensemble\n")
            cat("ensemble size:  ", length(self$ensemble), "\n", sep = "")
            cat("sequential_mode:  ",
                !is.null(private$.ranges),
                "\n", sep = "")
            cat("percentages_mode:  ",
                !is.null(self$parameter_percs),
                "\n", sep = "")
            cat("\nmodified parameters:\n",
                paste(self$dp_parameters, collapse = "\n"),
                "\n", sep = "")
            cat("\nparameter values:\n")
            print(self$parameter_values)

            invisible(self)
        },

        #'@description
        #'Runs the simulation. This calls r2ogs6::ogs_run_simulation()
        #' internally. For ensembles, output will always be written to logfiles.
        #'@param parallel flag: Should the function be run in parallel?
        #' This is implementented via the 'parallel' package.
        #'@param verbose flag
        run_simulation = function(parallel = FALSE,
                                  verbose = F){

            assertthat::assert_that(assertthat::is.flag(parallel))
            assertthat::assert_that(assertthat::is.flag(verbose))

            # Create ensemble directory
            assertthat::assert_that(!dir.exists(self$ens_path))
            dir.create(self$ens_path)

            if(parallel){

                # Forking is not possible in Windows
                use_socket <- (Sys.info()["sysname"] == "Windows")

                n_cores <- parallel::detectCores()
                n_logical_cores <- parallel::detectCores(logical = FALSE)

                cat("Detected ", n_cores, " cores.\n",
                    "Detected ", n_logical_cores, " logical cores.\n", sep = "")

                if(use_socket){
                    log_path <- paste0(self$ens_path, "cluster_log.txt")

                    socket_cl <- parallel::makeCluster((n_cores - 1),
                                                       outfile = log_path)

                    doParallel::registerDoParallel(socket_cl)

                    parallel::clusterEvalQ(socket_cl, {
                        library(r2ogs6)
                    })

                    ensemble <- self$ensemble

                    foreach::foreach(i = seq_along(ensemble)) %dopar% {
                        ogs6_obj <- ensemble[[i]]
                        r2ogs6::ogs_run_simulation(ogs6_obj,
                                               write_logfile = TRUE,
                                               verbose = verbose)
                    }

                    # Cleanup
                    parallel::stopCluster(socket_cluster)

                }else{

                    # For OSs other than Windows where forking is possible, we
                    # utilize it with mclapply

                    parallel::mclapply(self$ensemble,
                                       ogs_run_simulation,
                                       write_logfile = TRUE,
                                       verbose = verbose,
                                       mc.cores = n_cores)
                }
            }else{

                # For serial ensembles, we can use lapply
                exit_codes <- lapply(self$ensemble,
                                     ogs_run_simulation,
                                     write_logfile = TRUE,
                                     verbose = verbose)
                return(exit_codes)
            }
        },

        #'@description
        #'If the ensemble was created in sequential_mode, this will get the
        #' name of the value vector that was being iterated over at the given
        #' `index` during ensemble creation. I. e. if the ensemble was created
        #' with the value vectors `a = c(1, 2, 3)` and `b = c("foo", "bar")`,
        #' an `index` of 4 would return `"b"`
        #'@param index number: Index
        relevant_parameter_at = function(index){

            assertthat::assert_that(assertthat::is.number(index))

            if(is.null(private$.ranges)){
                warning(paste("This ensemble wasn't set up in sequential mode",
                              call. = FALSE))
                return(NULL)
            }

            for(i in seq_len(length(private$.ranges))){
                if(index %in% private$.ranges[[i]]){
                    return(names(private$.ranges)[[i]])
                }
            }

            warning(paste("Could not find range for given index", index,
                          call. = FALSE))

            return(NULL)
        }
    ),

    active = list(

        #'@field dp_parameters
        #'Getter for private parameter '.dp_parameters'
        dp_parameters = function() {
            private$.dp_parameters
        },

        #'@field parameter_percs
        #'Getter for private parameter '.parameter_percs'
        parameter_percs = function() {
            private$.parameter_percs
        },

        #'@field parameter_values
        #'Getter for private parameter '.parameter_values'
        parameter_values = function() {
            private$.parameter_values
        },

        #'@field ens_path
        #'Getter for private parameter '.ens_path'
        ens_path = function() {
            private$.ens_path
        },

        #'@field ensemble
        #'Getter for private parameter '.ensemble'
        ensemble = function(value) {
            private$.ensemble
        }
    ),

    private = list(

        # Calculates values based on given percentages
        calc_values_by_percs = function(ogs6_obj){

            for(i in seq_len(length(private$.parameter_percs))){
                val <- eval(parse(text = self$dp_parameters[[i]]))

                val_vec <- vapply(private$.parameter_percs[[i]], function(x){
                    val + (val * (x / 100))
                    },
                    FUN.VALUE = numeric(length(val)))

                private$.parameter_values <- c(self$parameter_values,
                                               list(val_vec))
            }
        },

        #@description
        #Creates the actual ensemble.
        make_ensemble = function(ogs6_obj,
                                 sequential_mode,
                                 percentages_mode) {

            orig_sim_name <- ogs6_obj$sim_name
            orig_sim_path <- ogs6_obj$sim_path

            parameter_values <- self$parameter_values

            # If sequential, put parameters behind each other
            if(sequential_mode){

                sim_id <- 1

                for(i in seq_len(length(parameter_values))){

                    shift_index_by <-
                        sum(unlist(lapply(seq_len(i - 1), function(x){
                            length(parameter_values[[x]])
                        })))

                    for(j in seq_len(length(parameter_values[[i]]))){

                        # Modify parameter reference of original object
                        set_param_call <-
                            paste0(self$dp_parameters[[i]],
                                   " <- parameter_values[[i]][[j]]")
                        eval(parse(text = set_param_call))

                        # Clone object
                        new_ogs6_obj <- private$copy_and_modify(ogs6_obj,
                                                                orig_sim_name,
                                                                sim_id,
                                                                orig_sim_path)

                        private$.ensemble <-
                            c(private$.ensemble, list(new_ogs6_obj))

                        sim_id <- sim_id + 1
                    }

                    range <- (1 + shift_index_by):
                        (shift_index_by + length(parameter_values[[i]]))

                    private$.ranges <- c(private$.ranges,
                                         list(range))
                    names(private$.ranges)[[length(private$.ranges)]] <-
                        names(parameter_values)[[i]]
                }

            }else{
                # n iterations in first loop == n objects to create
                for (i in seq_len(length(parameter_values[[1]]))) {

                    # Modify parameter reference of original object
                    for (j in seq_len(length(self$dp_parameters))) {
                        set_param_call <-
                            paste0(self$dp_parameters[[j]],
                                   " <- parameter_values[[j]][[i]]")
                        eval(parse(text = set_param_call))
                    }

                    # Clone object
                    new_ogs6_obj <- private$copy_and_modify(ogs6_obj,
                                                            orig_sim_name,
                                                            (i + 1),
                                                            orig_sim_path)

                    # Add clone to list of simulation objects
                    private$.ensemble <- c(private$.ensemble,
                                           list(new_ogs6_obj))
                }
            }
        },

        copy_and_modify = function(ogs6_obj,
                                   sim_name,
                                   sim_id,
                                   sim_path){
            # Clone object
            new_ogs6_obj <- ogs6_obj$clone(deep = TRUE)
            new_ogs6_obj$sim_name <- paste0(sim_name, "_", sim_id)
            new_path <- substr(sim_path,
                               start = 1,
                               stop = nchar(sim_path) - 1)
            new_ogs6_obj$sim_path <- paste0(new_path, "_", sim_id)
            return(invisible(new_ogs6_obj))
        },

        .ranges = NULL,
        .ens_path = NULL,
        .ensemble = list(),
        .dp_parameters = list(),
        .parameter_percs = NULL,
        .parameter_values = list()
    )
)


#'ogs_get_combinations
#'@description Gets possible combinations from supplied vectors
#'@param ... vector:
#'@export
ogs_get_combinations <- function(...){

    vec_list <- list(...)
    long_vec_list <- list()

    # Multiply lengths of vectors to get total length
    total_len <- prod(unlist(lapply(vec_list, length)))

    # Produce one long vector for each vector in vec_list
    for(i in seq_len(length(vec_list))){

        div <- total_len

        for(j in seq_len((i - 1))){
            div <- div / length(vec_list[[j]])
        }

        long_vec <- rep(unlist(lapply(vec_list[[i]],
                                             function(x){
            rep(x, (div / length(vec_list[[i]])))
        })), (total_len / div))

        long_vec_list <- c(long_vec_list,
                           list(long_vec))
    }

    return(invisible(long_vec_list))
}
