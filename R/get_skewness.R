#' Compute skewness
#'
#' Calculates the skewness of a numeric vector using the sample
#' formula (third standardized moment).
#'
#' @param x A numeric vector.
#' @param na.rm logical. Should missing values be removed? Defaults to \code{TRUE}.
#' @param ... Additional arguments passed to \code{\link[base]{mean}} and
#'   \code{\link[stats]{sd}}.
#'
#' @return The skewness coefficient.
#'
#' @export
#'
#' @examples
#' get_skewness(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

get_skewness <- function(x, na.rm = TRUE, ...) {
  args <- list(na.rm = na.rm, ...)

  if (args$na.rm) {
    x <- x[!is.na(x)]
  }

  n <- length(x)
  m <- mean(x, na.rm = na.rm, ...)
  s <- sd(x, na.rm = na.rm, ...)

  skew <- sum(((x - m)^3) / (s^3)) / n

  skew
}
