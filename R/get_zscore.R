#' Compute z-scores
#'
#' Calculates the z-scores (standardized values) of a numeric vector
#' using \code{\link[base]{scale}}.
#'
#' @param x A numeric vector.
#' @param ... Additional arguments passed to \code{\link[base]{scale}}.
#'
#' @return The z-score matrix.
#'
#' @export
#'
#' @examples
#' get_zscore(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

get_zscore <- function(x, ...) {

  scale(x, ...)

}
