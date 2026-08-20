#' Compute skewness
#'
#' Calculates the skewness of a numeric vector using the sample
#' formula (third standardized moment).
#'
#' @param x A numeric vector.
#' @param ... Additional arguments passed to \code{\link[base]{mean}} and
#'   \code{\link[stats]{sd}}.
#'
#' @return The skewness coefficient.
#'
#' @export
#'
#' @examples
#' get_skewness(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

get_skewness <- function(x, ...) {

  n <- length(x[!is.na(x)])
  m <- mean(x, ...)
  s <- sd(x, ...)

  skew <- sum(((x - m)^3) / (s^3)) / n

  skew
}
