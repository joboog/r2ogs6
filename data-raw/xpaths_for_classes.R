
# While having r2ogs6 loaded:

#'get_xpaths_for_classes
#'@description Creates a list of all `xpath` arguments of `r2ogs6` classes.
#' This is for efficiency as getting arguments from non-instantiated S3 classes
#' requires string parsing.
get_xpaths_for_classes <- function(){

    ns_exports <- getNamespaceExports("r2ogs6")
    r2ogs6_class_constructor_names <-
        sort(ns_exports[grepl("^new_r2ogs6", ns_exports)])
    r2ogs6_class_helper_names <-
        sort(ns_exports[grepl("^r2ogs6", ns_exports)])

    xfc_list <- list()

    for(i in seq_len(length(r2ogs6_class_constructor_names))){

        cc <- r2ogs6_class_constructor_names[[i]]
        cc_str <-
            paste(utils::capture.output(dput(eval(parse(text = cc)))),
                  collapse="\n")

        if(grepl("xpath[ ]*=[ \r\n]*\"[A-Za-z\\_\\/]*\"", cc_str)){
            xpath <-
                stringr::str_extract(cc_str,
                                     "xpath[ ]*=[ \r\n]*\"[A-Za-z\\_\\/]*\"")
            xpath <- unlist(strsplit(xpath, "[ ]*=[ \r\n]*"))[[2]]
            xpath <- stringr::str_remove_all(xpath, "\"")

        }else{

            # If xpath was concatenated, parse
            regexp <- paste0("xpath[:space:]*=[^=]*")

            xpath_call <- stringr::str_extract(cc_str, regexp)
            xpath_call <- stringr::str_remove(xpath_call,
                                              ",[:space:]*attr_name.*")

            xpath_call <- unlist(strsplit(xpath_call, "[ ]*=[ \r\n]*"))[[2]]
            xpath <- eval(parse(text = xpath_call))
        }

        xfc_list <- c(xfc_list,
                      list(xpath))

        names(xfc_list)[[length(xfc_list)]] <-
            r2ogs6_class_helper_names[[i]]
    }

    return(invisible(xfc_list))
}

xpaths_for_classes <- get_xpaths_for_classes()

usethis::use_data(xpaths_for_classes,
                  overwrite = TRUE)
