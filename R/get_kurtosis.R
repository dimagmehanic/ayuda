#' Compute kurtosis
#'
#' Calculates the kurtosis of a numeric vector using the sample
#' formula (fourth standardized moment).
#'
#' @param x A numeric vector.
#' @param ... Additional arguments passed to \code{\link[base]{mean}} and
#'   \code{\link[stats]{sd}}.
#'
#' @return The kurtosis coefficient.
#'
#' @export
#'
#' @examples
#' get_kurtosis(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

get_kurtosis <- function(x, ...) {

  n <- length(x[!is.na(x)])
  m <- mean(x, ...)
  s <- sd(x, ...)

  kurtosis <- sum(((x - m)^4) / (s^4)) / n

  kurtosis
}
