#' Ask for user input
#'
#' @param pprompt prompt message to ask user input
#' @param default default value if user input is NULL
#'
#' @return user input or default value
#' @export
#'
#' @examples
#' ask_input("Please enter a value: ", default = NULL)

ask_input <- function(pprompt, default = NULL) {
  if (is.null(var)) {
    var <- readline(prompt = pprompt)
  }
  if (is.null(var)) {
    default
  }else {
    var
  }
}