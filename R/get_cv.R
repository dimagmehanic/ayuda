#' Compute coefficient of variation
#'
#' Calculates the coefficient of variation (CV) of a numeric vector
#' as a percentage: \code{100 * sd(x) / mean(x)}.
#'
#' @param x A numeric vector.
#' @param ... Additional arguments passed to \code{\link[base]{mean}} and
#'   \code{\link[stats]{sd}}.
#'
#' @return The coefficient of variation (percentage).
#'
#' @export
#'
#' @examples
#' get_cv(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

get_cv <- function(x, ...) {

  m <- mean(x, ...)
  s <- sd(x, ...)

  cv <- 100 * s / m

  cv
}
