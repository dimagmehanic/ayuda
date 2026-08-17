#' Compute IQR and outlier fences
#'
#' Calculates quartiles, median, IQR, and Tukey's inner and outer fences.
#'
#' @param x A numeric vector.
#' @param ... Additional arguments passed to \code{\link[stats]{quantile}} and
#'   \code{\link[stats]{IQR}}.
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
#' get_iqr(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

get_iqr <- function(x, ...) {
  Q1 <- stats::quantile(x, 0.25, ...)
  median <- stats::quantile(x, 0.5, ...)
  Q3 <- stats::quantile(x, 0.75, ...)
  iqr <- stats::IQR(x, ...)

  lower_bound <- Q1 - 1.5 * iqr
  upper_bound <- Q3 + 1.5 * iqr

  lower_bound1 <- Q1 - 3 * iqr
  upper_bound1 <- Q3 + 3 * iqr

  data.frame(
    Statistic = c("Lower Outer fence", "Lower inner fence", "Q1",
                  "Median", "Q3", "Upper inner fence", "Upper outer fence"),
    Value = c(lower_bound1, lower_bound, Q1, median, Q3, upper_bound, upper_bound1)
  )
}
