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
#' ask_input(a, "Please enter a value: ", default = NULL)

ask_input <- function(var, pprompt, default = NULL) {
  if (is.null(var)) {
    var <- readline(prompt = pprompt)
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