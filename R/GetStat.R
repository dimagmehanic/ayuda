#' Statistical analysis class
#'
#' @description Compute descriptive statistics, observation counts,
#'   missing values, and frequency tables for a vector.
#'
#' @field x Input vector.
#'
#' @examples
#' # Numeric vector
#' s <- GetStat$new(c(1, 2, 3, NA, 5, 5, 6))
#' s$get_obs()
#'
#'
#' @export
GetStat <- R6::R6Class("GetStat", # nolint
  public = list(
    x = NULL,

    #' @description Initialize a new GetStat object
    #' @param x A vector.
    initialize = function(x) {
      self$x <- x     # Detect type and print an icon + message
      if (is.data.frame(x)) {
          message("📦 a data frame with ",
                  ncol(x), " columns and ", nrow(x), " rows")
      } else {
          message("📊 a vector of length ", length(x))
      }
    },

    #' @description Observation and missing value statistics
    #' @return A data frame with Metric and Value columns.
    #' @param col column names
    #' @param ... Additional arguments passed to read functions
    obs = function(col = NULL, ...) {
        if (is.data.frame(self$x)) {
            if (is.null(col)) col <- names(self$x)

            tables <- lapply(col, function(col) {

                data <- self$x[[col]]

                out <- get_obs(data)

                names(out)[2] <- col
                out
            })

            purrr::reduce(tables, left_join, by = "Metric")
        } else {
            get_obs(self$x)
        }
    },
    #' @description List tables or files
    #' @param ... Additional arguments passed to read functions
    #' @return character vector
    list = function(...) {
        names(self$x)
    }

    )
)
