
#===== r2ogs6_insitu =====


#'r2ogs6_insitu
#'@description tag: insitu
#'@param scripts character: Script names
#'@export
r2ogs6_insitu <- function(scripts) {

    #Make this more user friendly
    #...

    if(is.list(scripts)){
        scripts <- unlist(scripts)
    }

    new_r2ogs6_insitu(scripts)
}


new_r2ogs6_insitu <- function(scripts) {

    assertthat::assert_that(is.character(scripts))
    names(scripts) <- rep("script", length(scripts))

    structure(
        list(
            scripts = scripts,
            tag_name = "insitu",
            is_subclass = FALSE,
            attr_names = character(),
            flatten_on_exp = character()
        ),
        class = "r2ogs6_insitu"
    )
}
