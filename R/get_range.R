#' Compute range, minimum and maximum statistics
#'
#' Calculates the minimum, maximum, and range for a numeric vector.
#'
#' @param x A numeric vector.
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
#' get_range(c(1, 5, 3, 9, 2))

get_range <- function(x, ...) {
  min <- min(x, ...)
  max <- max(x, ...)
  range <- max - min

  data.frame(
    Statistic = c("Minimum", "Maximum", "Range"),
    Value = c(min, max, range)
  )
}
