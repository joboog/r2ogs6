
#===== Implementation utility =====


#' get_class_from_xpath
#' @description Gets r2ogs6 class name from an xpath-like expression
#' @param xpath string: An xpath expression. Works for path-like xpaths only
#' @return string: The class name.
#' @noRd
get_class_from_xpath <- function(xpath){

  assertthat::assert_that(assertthat::is.string(xpath))

  xpaths_for_classes <- xpaths_for_classes

  for(i in seq_len(length(xpaths_for_classes))){
    if((assertthat::is.string(xpaths_for_classes[[i]]) &&
        xpaths_for_classes[[i]] == xpath) ||
       (length(xpaths_for_classes[[i]]) > 1 &&
        xpath %in% xpaths_for_classes[[i]])){
      return(names(xpaths_for_classes)[[i]])
    }
  }

  return(NULL)
}


#' get_tag_from_class
#' @description Utility function, returns the tag name of a r2ogs6 class
#' @param class_name string: The name of a r2ogs6 class
#' @return string: The tag name corresponding to \code{class_name}
#' @noRd
get_tag_from_class <- function(class_name) {

  assertthat::assert_that(assertthat::is.string(class_name))

  xpaths_for_classes <- xpaths_for_classes

  xpath <- xpaths_for_classes[[class_name]]
  split_xpath <- unlist(strsplit(xpath[[1]], "/", fixed = TRUE))
  tag_name <- split_xpath[[length(split_xpath)]]

  return(tag_name)
}


#' get_tag_from_xpath
#' @description Gets the XML tag name from an xpath expression
#' @param xpath string: An xpath expression. Works for path-like xpaths only
#' @return string: The XML tag name
#' @noRd
get_tag_from_xpath <- function(xpath){

  xpath_split <- unlist(strsplit(xpath, "/", fixed = TRUE))
  tag_name <- xpath_split[[length(xpath_split)]]

  return(tag_name)
}


#' get_prj_top_level_tags
#' @description Gets top level .prj tags along with info if they are required.
#' @return list: List of lists.
#' @noRd
get_prj_top_level_tags <- function(){

  prj_reduxml <- system.file("extdata/xml_redux/", "prj_redu.xml",
                             package = "r2ogs6")

  xml_doc <- xml2::read_xml(prj_reduxml)

  prj_tag_info <- lapply(xml2::xml_children(xml_doc), function(x){
      list(tag_name = xml2::xml_name(x),
           is_required = as.logical(xml2::xml_attrs(x)[["required"]]))
  })

  return(prj_tag_info)
}


#===== Coercion utility =====


#' coerce_string_to_numeric
#' @description If an object is of type string, coerces it to a numeric type
#' @param obj object: Any object
#' @return numeric if \code{obj} was a string, else unchanged \code{obj}
#' @noRd
coerce_string_to_numeric <- function(obj){

  if(assertthat::is.string(obj)){
    obj <- trimws(gsub("\r?\n|\r|\\s+", " ", obj))
    obj <- as.double(unlist(strsplit(obj, " ")))
  }

  return(obj)
}


#' coerce_names
#' @description Validator function for a parameter vector
#' @param vector vector: Vector of parameters
#' @param names character: How the vector elements will be named as
#' per default
#' @return vector: Named vector where the names correspond to \code{names}
#' @noRd
coerce_names <- function(vector, names) {

  assertthat::assert_that(is.vector(vector))
  assertthat::assert_that(is.character(names))
  assertthat::assert_that(length(vector) == length(names))

  sorted_param_names <- sort(names(vector))
  sorted_names <- sort(names)

  if(is.null(names(vector)) ||
     (!is.null(names(vector)) &&
      any(sorted_param_names != sorted_names))){

    names(vector) <- names

    message(paste0(
      "Renaming elements of ",
      deparse(quote(vector)),
      " to fit their default names: '",
      paste(names, collapse = "', '"),
      "'"))
  }

  return(invisible(vector))
}


#' is_null_or_coerce_names
#' @description Validator function for a parameter list or vector or \code{NULL}
#' @param obj A list (or vector) of parameters
#' @param names How the list elements will be named as per default
#' @noRd
is_null_or_coerce_names <- function(obj, names){

  if(!is.null(obj)){
    obj <- coerce_names(obj, names)
  }

  return(invisible(obj))
}


#' clean_imported_list
#' @description Cleans an imported list because sometimes strings containing
#' only newline characters and spaces get imported in
#' @param list list: A list
#' @noRd
clean_imported_list <- function(list){

  assertthat::assert_that(is.list(list))

  cleaned_list <- list()

  for(i in seq_len(length(list))){
    if(assertthat::is.string(list[[i]]) &&
       stringr::str_remove_all(list[[i]], "[\n|[:space:]]") == ""){
      next
    }
    cleaned_list <- c(cleaned_list, list(list[[i]]))
    names(cleaned_list)[[length(cleaned_list)]] <- names(list)[[i]]
  }

  return(invisible(cleaned_list))
}


#' as_dir_path
#' @description Checks if a given path ends on \code{/}
#' @param path string: A path
#' @noRd
as_dir_path <- function(path){

  assertthat::assert_that(assertthat::is.string(path))

  path <- gsub("\\", "/", path, fixed = TRUE)

  nchar <- nchar(path)

  if(substring(path, nchar, nchar) != "/"){
    path <- paste0(path, "/")
  }

  return(invisible(path))
}

#' as_dir_path2
#' @description Removes leading \code{./} from a given path
#' @param file_path string: A path
#' @noRd
as_dir_path2 <- function(file_path){

  assertthat::assert_that(assertthat::is.string(file_path))

  if(stringr::str_detect(substr(file_path,1,2), "./")){
    file_path <- sub("\\./", "", file_path)
  }
  return(invisible(file_path))
}


#' win_to_linux_path
#' @description In case, converts windows style path to linux style path.
#' For instance, \code{C:\\path\\to} to \code{C:/path/to}.
#' @param file_path string: A path
#' @noRd
win_to_linux_path <- function(file_path){

    assertthat::assert_that(assertthat::is.string(file_path))
    file_path <- sub("\\\\\\\\", "//", file_path)
    file_path <- sub("\\\\", "/", file_path)

    return(invisible(file_path))
}


#' filter_invalid_xml
#' @description Filters invalid XML paths out of a vector
#' @param paths character: Vector of (maybe-)XML paths
#' @param encoding string: Optional: XML encoding. Defaults to ISO-8859-1
#' @param print_messages flag: Optional: Print error messages? Defaults to
#' \code{TRUE}
#' @return character: Vector of invalid XML paths
#' @noRd
filter_invalid_xml <- function(paths,
                               encoding = "ISO-8859-1",
                               print_messages = TRUE){

  invalid_paths <- character()

  for(i in seq_len(length(paths))){
    out <- tryCatch(
      {
        xml2::read_xml(paths[[i]],
                       encoding = encoding)
      },
      error = function(cond){
        if(print_messages){
          message(paste("\nxml2::read_xml() failed for",
                        paths[[i]], ". Original error message:"))
          message(cond)
        }
        invalid_paths <<- c(invalid_paths, paths[[i]])
      }
    )
  }

  return(invalid_paths)
}


#===== Validation utility =====


#' are_numbers
#' @description Checks if objects are numbers
#' @param ... Ellipsis
#' @noRd
are_numbers <- function(...){

  lapply(list(...), function(x){
    assertthat::assert_that(assertthat::is.number(x))
  })

  return(invisible(TRUE))
}


#' are_null_or_numbers
#' @description Checks if objects are either \code{NULL} or numbers
#' @param ... Ellipsis
#' @noRd
are_null_or_numbers <- function(...){

  lapply(list(...), function(x){
    if(!is.null(x)){
      assertthat::assert_that(assertthat::is.number(x))
    }
  })

  return(invisible(TRUE))
}


#' are_numeric
#' @description Checks if objects are numeric
#' @param ... Ellipsis
#' @noRd
are_numeric <- function(...){

  lapply(list(...), function(x){
    assertthat::assert_that(is.numeric(x))
  })

  return(invisible(TRUE))
}


#' are_null_or_numeric
#' @description Checks if objects are either \code{NULL} or numeric
#' @param ... Ellipsis
#' @noRd
are_null_or_numeric <- function(...){

  lapply(list(...), function(x){
    if(!is.null(x)){
      assertthat::assert_that(is.numeric(x))
    }
  })

  return(invisible(TRUE))
}


#' are_strings
#' @description Checks if objects are strings
#' @param ... Ellipsis
#' @noRd
are_strings <- function(...){

  lapply(list(...), function(x){
    assertthat::assert_that(assertthat::is.string(x))
  })

  return(invisible(TRUE))
}


#' are_null_or_strings
#' @description Checks if objects are either \code{NULL} or strings
#' @param ... Ellipsis
#' @noRd
are_null_or_strings <- function(...){

  lapply(list(...), function(x){
    if(!is.null(x)){
      assertthat::assert_that(assertthat::is.string(x))
    }
  })

  return(invisible(TRUE))
}


#' are_string_flags
#' @description Checks if objects are strings reading either "true" or "false"
#' @param ... Ellipsis
#' @noRd
are_string_flags <- function(...){

  lapply(list(...), function(x){
    assertthat::assert_that(assertthat::is.string(x))
    assertthat::assert_that(x %in% c("true", "false"))
  })

  return(invisible(TRUE))
}


#' are_null_or_string_flags
#' @description Checks if objects are either \code{NULL} or strings reading
#' either "true" or "false"
#' @param ... Ellipsis
#' @noRd
are_null_or_string_flags <- function(...){

  lapply(list(...), function(x){
    if(!is.null(x)){
      are_string_flags(x)
    }
  })

  return(invisible(TRUE))
}


#' is_wrapper_list
#' @description Checks if a list consists only of elements of class
#' \code{element_class}
#' @param list list: List to check
#' @param element_class string: Class each element of \code{list} should have
#' @noRd
is_wrapper_list <- function(list, element_class) {

  assertthat::assert_that(is.list(list))

  lapply(list, function(x){
    assertthat::assert_that(any(grepl(element_class, class(x), fixed = TRUE)))
  })

  return(invisible(TRUE))
}


#' is_null_or_wrapper_list
#' @description Checks if an object is either \code{NULL} or a list of elements
#' of class \code{element_class}
#' @param obj list | NULL: Object to check
#' @param element_class string: Class each element of \code{obj} should have
#' @noRd
is_null_or_wrapper_list <- function(obj, element_class) {

  if(!is.null(obj)){
    assertthat::assert_that(is.list(obj))

    lapply(obj, function(x){
      assertthat::assert_that(any(grepl(element_class, class(x), fixed = TRUE)))
    })
  }

  return(invisible(TRUE))
}


#' is_null_or_has_class
#' @description Checks if an object is either \code{NULL} or a class object of
#' class \code{class_name}
#' @param obj The object to check
#' @param class_name The name of the expected class
#' @noRd
is_null_or_has_class <- function(obj, class_name){

  if(!is.null(obj)){
      assertthat::assert_that(class(obj) == class_name)
  }

  return(invisible(TRUE))
}


#--- File utility ------------------------------------------------------------

#' make_abs_path
#' @description Creates an absolute file path based on a given file name or path
#'  and a reference path. The reference path will be adjusted if the file path
#'  is relative (e.g.  \code{../foo/bar.baz}). A file with the created absolute
#'  path should exist.
#' @param file_path string: Name or path to file.
#' @param ref_path string: Reference path whre file_path will be combined with.
#' Will be made absolute if relative path is given.
#' @noRd
make_abs_path <- function(file_path, ref_path, force=F){

  assertthat::assert_that(assertthat::is.string(file_path))
  assertthat::assert_that(assertthat::is.string(ref_path))

  file_path <- stringr::str_trim(file_path)

  # make shure that ref_path is absolute and ends with "/"
  ref_path <- normalizePath(ref_path, mustWork = T)
  ref_path <- win_to_linux_path(ref_path)
  ref_path <- as_dir_path(ref_path)
  file_path <- win_to_linux_path(file_path)
  file_path <- as_dir_path2(file_path)

  # case1: if file_path is absolute
  if((substr(file_path,1,1)=="/")| # abspath on unix
     (stringr::str_detect(substr(file_path,1,3), "[:alpha:]:/"))){# windows

    message(paste("file_path", file_path, "is already asolute."))
    if(isTRUE(force)){
      file_path <- paste0(ref_path, basename(file_path))
      message(paste0("Forced to convert to: ", ref_path, file_path))
    }
  }
  # case2: if file_path consist of filename only
  else if(basename(file_path)==file_path){
    file_path <- paste0(ref_path,file_path)
  }
  # case3: if file_path is in parent dir(s) (contains "../")
  else if(stringr::str_detect(file_path, "\\.\\./")){
    cds <- stringr::str_count(file_path, "\\.\\./")
    file_path <- gsub("\\.\\./", "", file_path)
    # cd.. through ref_path
    for(i in seq(cds)){
      ref_path <- sub("\\w+/$", "", ref_path)
    }
    file_path <- paste0(ref_path,file_path)
  }
  # case4: file is in child dir
  else {

    file_path_norm <- normalizePath(dirname(file_path), mustWork = T)
    file_path_norm <- win_to_linux_path(file_path_norm)

    assertthat::assert_that(
      stringr::str_detect(file_path_norm, ref_path),
      msg = paste(file_path, "is neither an absolute path nor a reference to",
                  "a file in", ref_path, "or in a child or parent directory."))

    file_path <- paste0(ref_path,file_path)
  }

  assertthat::assert_that(file.exists(file_path))

  return(file_path)
}


#' dir_make_overwrite
#' @description Creates a directory or cleans existing one at given path.
#' @param path String
#' @noRd
dir_make_overwrite <- function(path) {

  if (dir.exists(path)) {
    unlink(path, recursive = TRUE)
  }
  dir.create(path, recursive = TRUE)

  return(invisible(TRUE))
}
