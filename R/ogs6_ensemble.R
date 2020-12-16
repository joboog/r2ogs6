
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

            private$.ogs6_obj_ref <-
                deparse(substitute(ogs6_obj))

            private$.sim_name <- ogs6_obj$sim_name

            ens_path_split <-
                unlist(strsplit(ogs6_obj$sim_path, "/",
                                fixed == TRUE))

            ens_dir_name <-
                paste0(ogs6_obj$sim_name, "_ensemble")

            append(ens_path_split,
                   ens_dir_name,
                   after = length(ens_path_split - 1))

            private$.ens_path <-
                paste0(paste(ens_path_split,
                             collapse = "/"),
                       "/")

            private$.ensemble <- list(ogs6_obj)

            self$parameters <- parameters
            private$make_ensemble()
        },

        #'@description
        #'Adds a parameter.
        #'@param parameter list, length == 2: The first element references an
        #' OGS6 parameter, the second one is a list or vector of values.
        #' To find out how many values you need to supply, call ensemble_size
        #' on this object.
        add_parameter = function(parameter) {
            self$parameters <- c(self$parameters, list(parameter))
        },

        #'@description
        #'Runs the simulation. This calls run_simulation() internally. For
        #' ensembles, logfiles will never be written to console. This function
        #' should be suited for parallelization.
        run_simulation = function(){
            for(i in seq_len(length(self$ensemble))){
                run_simulation(self$ensemble[[i]])
            }
        }
    ),

    active = list(

        #'@field parameters
        #'Access to private parameter '.parameters'
        parameters = function(value) {
            if (missing(value)) {
                private$.parameters
            } else{
                validate_ensemble_parameters(value, private$.ogs6_obj_ref)
                private$.parameters <-
                    value
                private$.ensemble <-
                    private$.make_ensemble()
            }
        },

        #'@field sim_name
        #'Getter for private parameter '.sim_name'
        sim_name = function(value) {
            private$.sim_name
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
        make_ensemble = function() {

            ogs6_obj <- self$ensemble[[1]]

            parameters <-
                private$.parameters

            # n iterations in first loop == n objects to create
            for (i in seq_len(length(parameters[[1]][[2]]))) {
                # Clone object, update parameter of clone
                ogs6_obj <-
                    ogs6_obj$clone()

                ogs6_obj$sim_name <-
                    paste0(self$sim_name,
                           "_", (i + 1))

                ogs6_obj$sim_path <-
                    paste0(self$ens_path,
                           ogs6_obj$sim_name,
                           "/")

                for (j in seq_len(length(parameters))) {
                    # Deparse call
                    param_ref <-
                        deparse(substitute(parameters[[j]][[1]]))
                    set_param_call <-
                        paste0(param_ref,
                               " <- parameters[[j]][[2]][[i]]")

                    eval(parse(text = set_param_call))
                }

                # Add clone to list of simulation objects
                append(private$.ensemble, list(ogs6_obj))
            }
        },

        .ogs6_obj_ref = NULL,
        .sim_name = NULL,
        .ens_path = NULL,
        .ensemble = NULL,
        .parameters = NULL
    )
)


#'validate_ensemble_parameters
#'@description Validates the parameters given to an OGS6_Ensemble object
#'@param parameters list: The specified parameters
#'@param ogs6_obj_ref string: Deparsed argument, for checking if the parameters
#' came from the same OGS6 object that the OGS6_Ensemble is based on
validate_ensemble_parameters <- function(parameters, ogs6_obj_ref){

    for(i in seq_len(length(parameters))){

        assertthat::assert_that(is.list(parameters[[i]]))
        assertthat::assert_that(length(parameters[[i]]) == 2)

        assertthat::assert_that(length(parameters[[i]][[2]]) ==
                                    length(parameters[[1]][[2]]))

        #Check if referenced parameters belong to ogs6_obj

        comp_str <- unlist(strsplit(parameters[[i]][[1]], "$",
                                    fixed = TRUE))[[1]]

        if(ogs6_obj_ref != comp_str){
            stop(paste0("The parameters referenced in parameters[[i]][[1]]",
                        "must belong to the ogs6_obj you are referencing!"),
                 call. = FALSE)
        }
    }
}
