#' include function
#'
#' @param package library name
#'
#' @return library install and load
#' @export
#'
#' @examples
#' include("ggplot2")
#' include("dplyr")

include <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
  }

  library(package, character.only = TRUE, quietly = TRUE)
}