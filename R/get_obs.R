#' Compute observation and missing value statistics
#'
#' Calculates the total number of observations, missing values,
#' non-missing values, and their corresponding percentages for a vector.
#'
#' @param x A vector.
#'
#' @return A data frame with two columns:
#' \describe{
#'   \item{Metric}{Statistic name.}
#'   \item{Value}{Calculated value for the statistic.}
#' }
#'
#' @export
#'
#' @examples
#' get_obs(c(1, 2, 3, NA))

get_obs <- function(x) {
    n <- length(x)
    miss <- sum(is.na(x))
    nonmiss <- sum(!is.na(x))

    data.frame(
        Metric = c("Total", "Missing", "Non-missing", "Missing (%)", "Non-missing (%)"),
        Value = c(
            n,
            miss,
            nonmiss,
            round(miss / n * 100, 2),
            round(nonmiss / n * 100, 2)
        )
    )
}
