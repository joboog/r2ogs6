#' OGS6_msh
#' @description Small class to support \code{MSH} mesh files. Note that
#' MSH files are a legacy format from OGS5. It is recommended to switch to VTU
#'  files.
#' @export
OGS6_msh <- R6::R6Class("OGS6_msh",
    public = list(
        #' @description reates new OGS6_vtu object..
        #' @param msh_path path to \code{*.msh} file.
        initialize = function(msh_path) {

            assertthat::is.string(msh_path)
            assertthat::assert_that(file.exists(msh_path))
            private$.msh_path <- msh_path
        },
        #' @description Overrides the default print method
        print = function(){
            cat("OGS6_msh\n")
            cat("msh path:\n")
            cat(private$.msh_path, "\n\n")
            invisible(self)
        }
    ),
    # === active fields

    active = list(
        #' @field msh_path
        #' Getter/setter for private parameter `.msh_path`
        msh_path = function(value) {
            if (missing(value)) {
                private$.msh_path
            } else {
                assertthat::assert_that(assertthat::is.string(value))
                private$.msh_path <- value
            }
        }
    ),
    private = list(
        .msh_path = NULL
    )
)