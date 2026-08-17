#' Compute standard deviation
#'
#' Calculates the standard deviation of a numeric vector.
#'
#' @param x A numeric vector.
#' @param ... Additional arguments passed to \code{\link[stats]{sd}}.
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
#' get_sd(c(1, 2, 3, 4, 5))

get_sd <- function(x, ...) {
  sd <- sd(x, ...)

  data.frame(
    Statistic = "Standard deviation",
    Value = sd
  )
}
