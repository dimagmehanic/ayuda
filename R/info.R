#' Print session info to a file
#'
#' @param file A character string specifying the name of the file to save
#' session info. Default is "info.txt".
#'
#' @return A file with session info
#' @export
#'
#' @examples
#' info()

info <- function(file = "info.txt") {
  sink(file.path(getwd(), file))
  print(sessionInfo())
  sink()
}