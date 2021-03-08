

#' Download OpenGeoSys 6
#'
#' Downloads latest prebuilt OpenGeoSys 6 version
#'
#' @param path String: Download path. Defaults to ...
#' @param os String: Operating System.
#' @export
ogs6_download_ogs6 <- function(path, os){

    if(missing(path)){
        path <- dirname(unlist(options("r2ogs6.default_ogs6_bin_path")))
    }

    if(missing(os)){
        os <- Sys.info()["sysname"]
    }

    assertthat::assert_that(assertthat::is.string(path))
    assertthat::assert_that(assertthat::is.string(os))

    if(os == "Windows"){

        windows_dl_path <-
            paste0(
                "https://ogsstorage.blob.core.windows.net/binaries/ogs6/",
                "6.3.3/ogs-6.3.3-Windows-10.0.14393-python-3.7.2-utils.zip"
            )

        zip_path <- paste0(path, ".zip")

        download.file(windows_dl_path, zip_path)
        unzip(zip_path, exdir = dirname(path))
        unlink(zip_path)
    }else{

    }

    return(invisible)
}
