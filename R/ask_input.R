#' Ask for user input if variable is NULL
#'
#' @param var variable to store user input
#' @param pprompt prompt message to ask user input
#' @param default default value if user input is NULL
#'
#' @return user input or default value
#' @export
#'
#' @examples
#' a <- NULL
#' ask_input(a, "Please enter a value: ", default = NULL)

ask_input <- function(var, pprompt, default = NULL) {

  # Text input wrapper
  ask_input_gui <- function(message, default="") {
    if (Sys.getenv("RSTUDIO") == "1") {
      rstudioapi::showPrompt(title = "User Input Prompt 📝", message = message, default = default)
    } else {
      readline(paste0(message, " "))
    }
  }

  if (is.null(var)) {
    var <- ask_input_gui(pprompt)
    # check if empty user imput
    if (stringr::str_trim(var) == "") {
      var <- NULL
    }
  }

  if (is.null(var)) {
    default
  }else {
    var
  }
}