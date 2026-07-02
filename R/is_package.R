#' Check Whether an R Package Is Installed
#'
#' Returns `TRUE` if the specified package is installed and available,
#' and `FALSE` otherwise.
#'
#' @param x A character string containing the name of the package.
#'
#' @return A logical value:
#' \itemize{
#'   \item `TRUE` if the package is installed.
#'   \item `FALSE` otherwise.
#' }
#'
#' @export
#'
#' @examples
#' is_package("dplyr")
#' is_package("haven")
is_package <- function(x) {
    requireNamespace(x, quietly = TRUE)
}
