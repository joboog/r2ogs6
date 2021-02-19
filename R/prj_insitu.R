
#===== prj_insitu =====


#' prj_insitu
#' @description tag: insitu
#' @param scripts character: Script names
#' @example man/examples/ex_prj_insitu.R
#' @export
prj_insitu <- function(scripts) {

    #Make this more user friendly
    #...

    if(is.list(scripts)){
        scripts <- unlist(scripts)
    }

    new_prj_insitu(scripts)
}


new_prj_insitu <- function(scripts) {

    assertthat::assert_that(is.character(scripts))
    names(scripts) <- rep("script", length(scripts))

    structure(
        list(
            scripts = scripts,
            xpath = "insitu",
            attr_names = character(),
            flatten_on_exp = character()
        ),
        class = "prj_insitu"
    )
}
