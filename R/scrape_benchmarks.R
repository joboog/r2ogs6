# Functionality for downloading benchmark files

#'scrape_benchmarks
#'@description Downloads all OGS6 benchmarks (WIP)
#'@param url Optional: An URL
#'@param path Optional: The path to save the benchmark folders to
scrape_benchmarks <- function(
    url = "https://gitlab.opengeosys.org/ogs/ogs/-/tree/master/Tests/Data/",
    path = "extdata/benchmarks/") {


    data_page <- xml2::read_html(url)

    data_folders <- grep(
        "prj$",
        rvest::html_attr(
            rvest::html_nodes(data_page,
                              "a[href^='/ogs/ogs/-/tree/master/Tests/Data/']"),
            "href"), value = TRUE)

    invisible(pbapply::pbsapply(data_folders, function(data_folder) {
        httr::GET(paste0(url, data_folder),
                  httr::write_disk(paste0(path, data_folder)))
    }))
}


#'download_benchmark
#'@description Downloads a single benchmark (consisting of one .prj file and
#' either one .gml and one .vtu file OR multiple .vtu files)
#'@param prj_url The URL of a .prj file
#'@param path The path the benchmark files should be saved to
download_benchmark <- function(prj_url, path) {

    assertthat::assert_that(assertthat::is.string(prj_url))
    assertthat::assert_that(assertthat::is.string(path))

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
