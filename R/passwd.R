#' Ask for user password input
#'
#' @param q A character string with the prompt message for the password input
#'
#' @return A character string containing the password entered by the user
#' @export
#'
#' @examples
#' passwd()
#' passwd(q="PASSWORD: ")

passwd <- function(q = "Enter Database password: ") {
  if (Sys.getenv("RSTUDIO") == "1") {
    rstudioapi::askForPassword(q)
  }else {
    # Get password with a custom message
    getPass::getPass(q)
  }
}