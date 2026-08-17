#' Compute the mode of a vector
#'
#' Returns the most frequent value. If multiple values tie,
#' the first occurring mode is returned.
#'
#' @param x A vector.
#' @param na.rm logical. Should missing values be removed? Defaults to \code{FALSE}.
#'
#' @return The modal value of \code{x} (same type as \code{x}), or \code{NA} if
#'   all values are missing after removing \code{NA}s (when \code{na.rm = TRUE}).
#'
#' @export
#'
#' @examples
#' get_mode(c(1, 2, 2, 3, 4))
#' get_mode(c(1, 1, 2, 2, 3))
#' get_mode(c(NA, NA))

get_mode <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }

  if (length(x) == 0L) {
    return(x[NA_integer_])
  }

  ux <- unique(x)
  ux[[which.max(tabulate(match(x, ux)))]]
}
