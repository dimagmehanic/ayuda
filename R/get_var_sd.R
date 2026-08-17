#' Compute variance and standard deviation
#'
#' Calculates both variance and standard deviation of a numeric vector.
#'
#' @param x A numeric vector.
#' @param ... Additional arguments passed to \code{\link[stats]{var}} and
#'   \code{\link[stats]{sd}}.
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
#' get_var_sd(c(1, 2, 3, 4, 5))

get_var_sd <- function(x, ...) {
  var <- var(x, ...)
  sd <- sd(x, ...)

  data.frame(
    Statistic = c("Variance", "Standard deviation"),
    Value = c(var, sd)
  )
}
