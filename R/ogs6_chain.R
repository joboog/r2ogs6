
#===== OGS6_Chain =====


#'OGS6_Chain
#'@description Constructor for the OGS6_Chain base class
#'@export
#' @importFrom R6 R6Class
OGS6_Chain <- R6::R6Class(
    "OGS6_Chain",

    public = list(

        #'@description
        #'Creates new OGS6_Chain object
        #'@param ogs6_obj OGS6: A simulation object.
        #'@param iter_n number: Number of iterations (length of the chain)
        initialize = function(ogs6_obj,
                              iter_n){

            assertthat::assert_that(inherits(ogs6_obj, "OGS6"))
            assertthat::assert_that(assertthat::is.number(iter_n))

            private$.chain <- list(ogs6_obj)

        },

        #'@description
        #'Runs the simulation. This calls r2ogs6::ogs6_run_simulation() internally.
        #' For chains, output will be written to logfiles.
        ogs6_run_simulation = function(){

            # WIP

            for(i in seq_len(iter_n)){

                # ogs6_run_simulation returns the exit code of ogs6.exe, so
                # we can use it to catch errors and stop the chain

                exit_code <- ogs6_run_simulation(self$chain[[i]])

                # If simulation ran successfully, initialize new OGS6 object
                # and add it to the chain.
                if(exit_code == 0){

                    # Clone the previous object
                    new_obj <- self$chain[[i]]$clone()

                    # Alter relevant parameters
                    private$set_chain_parameters(new_obj)

                    # Add to chain
                    private$.chain <- c(private$.chain, list(new_obj))

                }else{
                    stop(paste("OpenGeoSys6 exited with error,",
                               "so your chain broke. Call print_log()",
                               "for more info"), call. = FALSE)
                }
            }
        },

        #'@description
        #'Reads in logfile of simulation and prints it to the console.
        #'This calls print_log internally. If no index is specified, this will
        #'print the logfile of the last object in the chain.
        #'@param index number: chain index of a OGS6 object
        print_log = function(index){

            if(missing(index)){
                index <- length(self$chain)
            }

            assertthat::assert_that(assertthat::is.number(index))
            assertthat::assert_that(index <= length(self$chain))

            self$chain[[index]]$print_log()
        }
    ),

    active = list(),

    private = list(

        set_chain_parameters = function(ogs6_obj){

        },

        .chain_path = NULL,
        .chain = list(),
        .parameters = list()
    )
)
