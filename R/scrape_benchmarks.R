
#===== download_benchmark =====


#'download_benchmark
#'@description Downloads a single benchmark (consisting of one .prj file and
#' either one .gml and one .vtu file or multiple .vtu files)
#'@param prj_url string: URL of a .prj file
#'@param path string: Path the benchmark files should be saved to
download_benchmark <- function(prj_url, path) {

    assertthat::assert_that(assertthat::is.string(prj_url))

    path <- as_dir_path(path)

    prj_dest_file <- paste0(path, basename(prj_url))
    file.create(prj_dest_file)

    prj_dir <- paste0(dirname(prj_url), "/")

    #Download the .prj file
    tmp <- tempfile()
    curl::curl_download(prj_url, tmp)
    prj_doc <- validate_read_in_xml(tmp)
    xml2::write_xml(prj_doc, prj_dest_file)

    gml_ref_node <- xml2::xml_find_first(prj_doc, "//geometry")

    #Get name of geometry file (if any)
    if (class(gml_ref_node) != "xml_missing") {
        geo_url <- paste0(prj_dir, xml2::xml_text(gml_ref_node))

        gml_dest_file <- paste0(path, basename(geo_url))
        file.create(gml_dest_file)
        curl::curl_download(geo_url, gml_dest_file)

        vtu_ref_node <- xml2::xml_find_first(prj_doc, "//mesh")
        vtu_url <- paste0(prj_dir, xml2::xml_text(vtu_ref_node))

        vtu_dest_file <- paste0(path, basename(vtu_url))
        file.create(vtu_dest_file)
        curl::curl_download(vtu_url, vtu_dest_file)
    }else{
        vtu_ref_nodes <- xml2::xml_find_all(prj_doc, "//meshes/*")

        for (i in seq_len(length(vtu_ref_nodes))) {
            vtu_url <- paste0(prj_dir, xml2::xml_text(vtu_ref_nodes[[i]]))

            vtu_dest_file <- paste0(path, basename(vtu_url))
            file.create(vtu_dest_file)
            curl::curl_download(vtu_url, vtu_dest_file)
        }
    }

    return(invisible(prj_dest_file))
}
