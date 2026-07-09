#' Compute central tendency statistics
#'
#' Calculates the mean, median, and mode for a numeric vector.
#'
#' @param x A numeric vector.
#' @param ... Additional arguments passed to \code{\link[base]{mean}} and
#'   \code{\link[stats]{median}}.
#'
#' @return A data frame with two columns:
#' \describe{
#'   \item{Statistic}{Statistic name.}
#'   \item{Value}{Calculated value for the statistic.}
#' }
#'
#' @export
#'
#' @examples
#' get_central(c(1, 2, 2, 3, 4, NA))

get_central <- function(x, ...) {
  mean <- mean(x, ...)
  median <- stats::median(x, ...)
  mode <- get_mode(x)

  data.frame(
    Statistic = c("Mean", "Median", "Mode"),
    Value = c(mean, median, mode)
  )
}
