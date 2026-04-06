#' List all files in a directory with a specific extension and return
#' them as a named list.
#'
#' @param location The directory to search for files.
#'
#' @return A named list of file paths, where the names are derived from
#' the file names without extensions.
#' @export
#'
#' @examples list_dir("path/to/data") 

list_dir <- function(location, ...) {
  files <- list.files(path = location, ...) %>% as.list()
  
  names(files) <- files %>% stringr::str_split("/") %>% 
    purrr::map(dplyr::last) %>% 
    str_split("[.]") %>% purrr::map(dplyr::first) %>% unlist()
  return(files) # nolint
}
