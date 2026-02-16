#' Setup function to install and load libraries
#'
#' @param packages a vector of library names to install and load
#'
#' @return libraries installed and loaded
#' @export
#'
#' @examples
#' setup()
#' setup(c("ggplot2", "dplyr"))
#' setup("haven")
setup <- function(packages = c("lobstr", "tidyverse", "haven", "stringr",
                               "DBI", "odbc", "lubridate", "ggplot2",
                               "ggalluvial", "RMySQL")) {
  purrr::walk(packages, include)
}
