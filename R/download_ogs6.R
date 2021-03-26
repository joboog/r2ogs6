

#' Download OpenGeoSys 6
#'
#' Downloads latest prebuilt OpenGeoSys 6 version
#'
#' @param path String: Download path. Defaults to ...
#' @param os String: Operating System.
#' @param set_as_default_bin_path flag: Defaults to `TRUE`
#' @export
ogs6_download_ogs6 <- function(path,
                               os,
                               set_as_default_bin_path = TRUE){

    default_installation_paths <-
        list(
            Windows = c("C:/Program Files/",
                        "D:/Program Files/"),
            Darwin = c("/Applications/"),
            Linux = c("~/software/",
                      "~/opt/")
        )

    if(missing(os)){
        os <- Sys.info()["sysname"]
    }

    assertthat::assert_that(assertthat::is.string(os))
    assertthat::assert_that(os %in% names(default_installation_paths))
    assertthat::assert_that(assertthat::is.flag(set_as_default_bin_path))

    if(missing(path)){

        path <- NULL

        for(i in default_installation_paths[[os]]){
            if(dir.exists(i)){
                path <- i
                break
            }
        }

        if(is.null(path)){
            stop(
                paste0(
                    "Did not find any default path for OpenGeoSys 6 ",
                    "on your System. I checked the following paths:\n",
                    paste(default_installation_paths[[os]], collapse = "\n"),
                    "\nPlease specify a path manually instead."
                ),
                call. = FALSE
            )
        }
    }

    path <- as_dir_path(path)

    if(os == "Windows"){

        url_path <-
            paste0(
                "https://ogsstorage.blob.core.windows.net/binaries/ogs6/",
                "6.3.3/ogs-6.3.3-Windows-10.0.14393-python-3.7.2-utils.zip"
            )

        ogs_zip_folder <- paste0(path, basename(url_path))
        utils::download.file(url_path, ogs_zip_folder)

        ogs_folder <- gsub("\\.zip$", "", ogs_zip_folder)
        utils::unzip(ogs_zip_folder, exdir = ogs_folder)
        unlink(ogs_zip_folder)

    }else{
        url_path <-
            paste0(
                "https://ogsstorage.blob.core.windows.net/binaries/ogs6/",
                "6.3.3/ogs-6.3.3-Windows-10.0.14393-python-3.7.2-utils.zip"
            )

        ogs_folder <- "/scif/apps/ogs"

        sif_path <- paste0(path, "ogs-6.3.3-serial.sif")
        utils::download.file(url_path, sif_path)
    }

    bin_path <- paste0(ogs_folder, "/bin")

    if(set_as_default_bin_path){
        options(r2ogs6.default_ogs6_bin_path = bin_path)
    }

    return(invisible())
}
