#' Compute variance
#'
#' Calculates the variance of a numeric vector.
#'
#' @param x A numeric vector.
#' @param ... Additional arguments passed to \code{\link[stats]{var}}.
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
#' get_var(c(1, 2, 3, 4, 5))

get_var <- function(x, ...) {
  var <- var(x, ...)

  data.frame(
    Statistic = "Variance",
    Value = var
  )
}
