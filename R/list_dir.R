#' List all files in a directory with a specific extension and return
#' them as a named list.
#'
#' @param location The directory to search for files.
#' @param ext The file extension to filter by (default is "xpt").
#'
#' @return A named list of file paths, where the names are derived from
#' the file names without extensions.
#' @export
#'
#' @examples list_dir("path/to/data", ext = "xpt")
#' @example list_dir("path/to/data")
#' @example list_dir("path/to/data", ext = "csv")

list_dir <- function(location, ext = "xpt") {
  files <- list.files(path = location, pattern = paste("*.", ext, sep = ""),
                      full.names = TRUE, recursive = TRUE) %>% as.list()
  names(files) <- files %>% stringr::str_split("/") %>% 
    purrr::map(dplyr::last) %>% 
    str_split("[.]") %>% purrr::map(dplyr::first) %>% unlist()
  return(files) # nolint
}