#' Get current system username
#'
#' Returns the username of the current system user. Works across
#' operating systems by checking environment variables (`USER` on
#' Unix/macOS and `USERNAME` on Windows).
#'
#' @return A character string with the current username. Returns
#'   `NA_character_` if the username cannot be determined.
#' @export
#'
#' @examples
#' whoami()

whoami <- function() {
  user <- Sys.getenv("USER")
  
  if (user == "") {
    user <- Sys.getenv("USERNAME")  # Windows fallback
  }
  
  if (user == "") {
    user <- NA_character_
  }
  return(user)
}