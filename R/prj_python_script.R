
# python_script -----------------------------------------------------------

#' prj_python_script
#' @description tag: python_script
#' @param file_path string: Path or name of python script file (\code{*.py}).
#' @export
#'
#' @examples
#' prj_python_script(scripts = "bcs_laplace_eq.py")
#'
prj_python_script <- function(file_path) {

    new_prj_python_script(file_path)
}


new_prj_python_script <- function(file_path) {

    assertthat::assert_that(assertthat::is.string(file_path))
    assertthat::assert_that(isTRUE(stringr::str_detect(file_path, ".py")),
                            msg = "The script has to be a python script!")

    structure(list(file_path,
                   xpath = "python_script",
                   attr_names = character(),
                   flatten_on_exp = character()),
    class = "prj_python_script")
}